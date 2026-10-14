{
  # luv-in-space
  #   nix develop / direnv allow   the dev shell: luv's environment, `pm`, git
  #   nix build                    the game: a dumped SBCL image and a launcher
  #   nix run                      start the game
  description = "Luv in Space: a hard sci-fi spaceflight game on luv";

  nixConfig = {
    extra-substituters = [ "https://luv.swa.sh/nix-cache?priority=30" ];
    extra-trusted-public-keys = [
      "luv.swa.sh-1:PpD45iCBkJ38ZkvlyZcLiGdIz6yVehXn3fm1JvG18Bw="
    ];
  };

  inputs = {
    # Pinned in flake.lock (decision 1: How the game consumes luv).
    luv.url = "github:mbrock/luv/6abc9d10ec8991c2cfe92888b84946372d6030b9";
    # luv keeps its own nixpkgs, so its binary cache matches; ours follows it.
    nixpkgs.follows = "luv/nixpkgs";
    # `pm`, the project manager CLI, in the dev shell only.
    pm = {
      url = "github:rubenmoor/project-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, luv, pm }:
    let
      # decision 6: Target platforms
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f system (luv.lib.environmentFor system));

      # Our sources only, so docs and tasks.sql don't rebuild the game.
      gameSource = nixpkgs.lib.fileset.toSource {
        root = ./.;
        fileset = nixpkgs.lib.fileset.unions [
          ./luv-in-space.asd
          ./src
          ./scripts/build.lisp
        ];
      };

      # The test check's sources: the game's plus tests/ and scripts/test.
      testSource = nixpkgs.lib.fileset.toSource {
        root = ./.;
        fileset = nixpkgs.lib.fileset.unions [
          ./luv-in-space.asd
          ./src
          ./tests
          ./scripts/test
        ];
      };

      # luv's root systems (luv, luv/test-support, …) from the input.
      luvRegistry = "${luv}/";

      # On Darwin luv compiles its Objective-C bridge into <luv>/build/, which
      # the store can't hold (docs/luv-wishlist.md), so luv loads from a
      # writable copy in `dir` there; Linux keeps the input.
      isDarwin = environment: environment.pkgs.stdenv.hostPlatform.isDarwin;
      copyLuv = environment: dir:
        nixpkgs.lib.optionalString (isDarwin environment) ''
          if [ ! -d "${dir}" ]; then
            mkdir -p "$(dirname "${dir}")"
            rm -rf "${dir}.partial"
            cp -R ${luv} "${dir}.partial"
            chmod -R u+w "${dir}.partial"
            mv "${dir}.partial" "${dir}"
          fi
        '';
      luvRegistryIn = environment: dir:
        if isDarwin environment then "${dir}/" else luvRegistry;
      # The dev shell's copy, one per luv revision.
      luvCache = "\${XDG_CACHE_HOME:-$HOME/.cache}/luv-in-space/luv-${luv.shortRev or "dirty"}";

      # A copy of luv's `applicationPackage` (luv flake.nix, a `let` binding,
      # not exported; docs/luv-wishlist.md), adapted: our source, luv's
      # sources from the input, no fonts or Luvcraft extras, no inherited
      # LD_LIBRARY_PATH.
      applicationPackage = system: environment:
        let
          inherit (environment) pkgs;
          inherit (pkgs) lib;
          name = "luv-in-space";
          output = builtins.placeholder "out";
          runtimeEnvironment = lib.filterAttrs
            (variable: _value: builtins.elem variable [
              "LUV_ASDF_CACHE_KEY"
              "LUV_BASH"
              "LUV_FFMPEG_LIBDIR"
              "LUV_NATIVE_LIBRARY_PATH"
              "VK_DRIVER_FILES"
            ])
            environment.applicationEnvironment;
          exportRuntimeEnvironment = lib.concatStringsSep "\n"
            (lib.mapAttrsToList
              (variable: value:
                "export ${variable}=${lib.escapeShellArg (toString value)}")
              runtimeEnvironment);
          runtimePath = lib.makeBinPath [
            pkgs.bashInteractive
            pkgs.coreutils
            environment.ffmpeg
          ];
        in
        pkgs.stdenv.mkDerivation (environment.applicationEnvironment // {
          pname = name;
          version = "0-unstable";
          src = gameSource;

          CL_SOURCE_REGISTRY =
            "${luvRegistry}:${environment.applicationEnvironment.CL_SOURCE_REGISTRY}";
          nativeBuildInputs = environment.applicationPackages;
          dontConfigure = true;
          dontPatchELF = true;
          dontStrip = true;

          buildPhase = ''
            runHook preBuild
            mkdir -p "$out/share"
            cp -R . "$out/share/${name}"
            chmod -R u+w "$out/share/${name}"
            cd "$out/share/${name}"

            export HOME="$TMPDIR/home"
            export LUV_PROJECT_ROOT="$PWD"
            mkdir -p "$HOME"
            unset LD_LIBRARY_PATH
            ${environment.developmentEnvironmentHook}
            luv_activate_native_environment

            sbcl --script scripts/build.lisp
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            cd "$out/share/${name}"
            mkdir -p "$out/bin" "$out/lib/fasl" "$out/libexec"
            mv "build/${name}" "$out/libexec/${name}"
            # CFFI restores foreign libraries from the FASLs at start.
            cp -R "$HOME/.cache/common-lisp/luv/$LUV_ASDF_CACHE_KEY/." "$out/lib/fasl/"
            find "$out/lib/fasl" -type f \( -name '*__grovel' \
              -o -name '*__grovel.c' -o -name '*__grovel.o' \) -delete
            cd "$out"
            rm -rf "$out/share/${name}"

            cat > "$out/bin/${name}" <<'LUV_LAUNCHER'
            #!${pkgs.runtimeShell}
            set -eu
            # A host LD_LIBRARY_PATH breaks luv's SDL3 (docs/luv-capabilities.md, Packaging).
            unset LD_LIBRARY_PATH
            ${exportRuntimeEnvironment}
            export PATH=${lib.escapeShellArg runtimePath}:"''${PATH:-}"
            asdf_output_translations="''${ASDF_OUTPUT_TRANSLATIONS:-}"
            ${environment.developmentEnvironmentHook}
            luv_activate_native_environment
            export ASDF_OUTPUT_TRANSLATIONS="''${asdf_output_translations:-${lib.escapeShellArg "/:${output}/lib/fasl//"}}"
            unset asdf_output_translations
            exec ${lib.escapeShellArg "${output}/libexec/${name}"} "$@"
            LUV_LAUNCHER
            chmod 0555 "$out/bin/${name}"
            runHook postInstall
          '';

          meta = {
            description = "Luv in Space";
            mainProgram = name;
            platforms = systems;
          };
        } // lib.optionalAttrs (isDarwin environment) {
          preBuild = copyLuv environment "$out/share/luv" + ''
            export CL_SOURCE_REGISTRY="$out/share/luv/:${environment.applicationEnvironment.CL_SOURCE_REGISTRY}"
          '';
          # SBCL reopens the bridge by the path the image loaded it from:
          # keep it and objective-c/, as luv's applicationPackage does.
          postInstall = ''
            find "$out/share/luv" -mindepth 1 -maxdepth 1 \
              ! -name build ! -name objective-c -exec rm -rf {} +
            find "$out/share/luv/build" -mindepth 1 -maxdepth 1 \
              ! -name 'objective-c-exception-bridge-*.dylib' -exec rm -rf {} +
          '';
        });
    in {
      packages = forAll (system: environment: rec {
        luv-in-space = applicationPackage system environment;
        default = luv-in-space;
      });

      apps = forAll (system: _environment: {
        default = {
          type = "app";
          program = "${self.packages.${system}.luv-in-space}/bin/luv-in-space";
          meta.description = "Start Luv in Space";
        };
      });

      # scripts/test in the sandbox: no display, no GPU, so the frame renders
      # on lavapipe (decision 2: Screenshots and GPU test: offscreen capture
      # on a hidden canvas, headless on lavapipe). luv's slim environment.
      checks = forAll (system: environment: {
        default = environment.pkgs.runCommand "luv-in-space-tests"
          (environment.slimDevelopmentEnvironment // {
            nativeBuildInputs = environment.slimDevelopmentPackages;
          }) ''
          cp -R ${testSource} source
          chmod -R u+w source
          cd source
          export HOME="$TMPDIR/home" LUV_PROJECT_ROOT="$PWD" LUV_DEV_SHELL=slim LIS_DEV_SHELL=1
          mkdir -p "$HOME"
          ${copyLuv environment "$TMPDIR/luv"}export CL_SOURCE_REGISTRY="${luvRegistryIn environment "$TMPDIR/luv"}:$CL_SOURCE_REGISTRY"
          unset LD_LIBRARY_PATH DISPLAY WAYLAND_DISPLAY
          ${environment.slimDevelopmentEnvironmentHook}
          luv_activate_native_environment
          bash scripts/test
          touch "$out"
        '';
      });

      # luv's default dev shell (its environment, packages and hook), with
      # this repo and luv's sources on CL_SOURCE_REGISTRY, plus pm and git.
      devShells = forAll (system: environment: {
        default = environment.pkgs.mkShell (environment.developmentEnvironment // {
          LUV_DEV_ENVIRONMENT_MODE = "nix-develop";
          LUV_DEV_SHELL = "default";
          packages = environment.developmentPackages ++ [
            pm.packages.${system}.default
            environment.pkgs.git
          ];
          shellHook = copyLuv environment luvCache + ''
            export CL_SOURCE_REGISTRY="$PWD/:${luvRegistryIn environment luvCache}:${environment.developmentEnvironment.CL_SOURCE_REGISTRY}"
            unset LD_LIBRARY_PATH
          '' + environment.developmentEnvironmentHook + ''
            luv_activate_native_environment
          '';
        });
      });
    };
}
