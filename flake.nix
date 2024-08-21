{
  description = "common lisp web-app example";
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; };
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ ];
        });
      lispDeps = ps:
        with ps; [
          alexandria # general utilities
          clack # web application environment
          com_dot_inuoe_dot_jzon # json
          lack-middleware-mount # for mounting websocket driver
          lack-middleware-static # static middleware
          spinneret # html5 generator
          websocket-driver # web sockets
          woo # web server
        ];
      lispTestDeps = ps:
        with ps;
        [
          rove # testing framework
        ];
    in {

      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};

          cl-web-app-example = pkgs.sbcl.buildASDFSystem {
            pname = "cl-web-app-example";
            version = "dev";
            src = ./.;
            systems = [ "cl-web-app-example" "cl-web-app-example/tests" ];
            lispLibs = (lispDeps pkgs.sbcl.pkgs)
              ++ (lispTestDeps pkgs.sbcl.pkgs);
          };

          sbcl = pkgs.sbcl.withOverrides
            (self: super: { inherit cl-web-app-example; }).withPackages
            (ps: [ ps.cl-web-app-example ]);

          run-server = sbcl:
            pkgs.writeScriptBin "run-server" ''
              ${sbcl}/bin/sbcl --eval '(load (sb-ext:posix-getenv "ASDF"))' \
                               --eval '(require :cl-web-app-example)' \
                               --eval '(cl-web-app-example:main)'
            '';

          run-tests = sbcl:
            pkgs.writeScriptBin "run-tests" ''
              ${sbcl}/bin/sbcl --non-interactive \
                               --eval '(load (sb-ext:posix-getenv "ASDF"))' \
                               --eval '(asdf:test-system :cl-web-app-example)'
            '';

          build-styles = pkgs.stdenv.mkDerivation {
            name = "build-styles";
            buildInputs = [ pkgs.tailwindcss ];
            src = ./.;
            buildPhase = "tailwindcss -o tailwind.css";
            installPhase = "mkdir -p $out/public; cp tailwind.css $out/public";
          };

          docker = pkgs.dockerTools.buildImage {
            name = "cl-web-app-example";
            tag = "latest";
            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [
                (run-server sbcl)
                build-styles
                pkgs.bashInteractive
                pkgs.coreutils-full
                pkgs.imagemagick
              ];
              pathsToLink = [ "/bin" "/public" ];
            };
            config = {
              Env =
                [ "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];
              Cmd = [ "/bin/run-server" ];
            };
          };

        in { inherit run-server run-tests docker; });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};

          docker-build = pkgs.writeScriptBin "docker-build" ''
            #!${pkgs.stdenv.shell}
            set -e; set -o pipefail;
            nix build '.#docker'
            image=$((docker load < result) | sed -n '$s/^Loaded image: //p')
            docker image tag "$image" cl-web-app-example:latest
          '';

          docker-run = pkgs.writeScriptBin "docker-run" ''
            #!${pkgs.stdenv.shell}
            docker run --init -it --rm -p5000:5000 cl-web-app-example:latest
          '';

          sbcl =
            (pkgs.sbcl.withPackages (ps: (lispDeps ps) ++ (lispTestDeps ps)));

        in {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              sbcl
              (self.packages.${system}.run-server sbcl)
              (self.packages.${system}.run-tests sbcl)
              tailwindcss
              docker-build
              docker-run
            ];
            shellHook = ''
              export CL_SOURCE_REGISTRY="$(pwd)"
            '';
          };
        });
    };
}
