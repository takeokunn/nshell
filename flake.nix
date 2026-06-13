{
  description = "nshell - Modern interactive shell in Common Lisp";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          sbcl = pkgs.sbcl;
        in
        {
          default = pkgs.stdenv.mkDerivation {
            pname = "nshell";
            version = "0.1.0";
            src = ./.;

            nativeBuildInputs = [ sbcl ];

            buildPhase = ''
              export HOME=$TMPDIR
              export CL_SOURCE_REGISTRY=$PWD
              sbcl --non-interactive \
                --eval '(require :asdf)' \
                --eval '(push (truename "./") asdf:*central-registry*)' \
                --eval '(asdf:load-system :nshell)' \
                --eval '(sb-ext:save-lisp-and-die "nshell"
                          :executable t
                          :compression t
                          :toplevel #'"'"'nshell:main)'
            '';

            installPhase = ''
              mkdir -p $out/bin
              cp nshell $out/bin/
              chmod +x $out/bin/nshell
            '';
          };
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/nshell";
        };
      });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = [
              (pkgs.sbcl.withPackages (ps: []))
            ];
            shellHook = ''
              export CL_SOURCE_REGISTRY=$PWD
            '';
          };
        });
    };
}
