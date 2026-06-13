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
        in
        {
          default = pkgs.sbcl.buildASDFSystem {
            pname = "nshell";
            version = "0.1.0";
            src = ./.;
            systems = [ "nshell" ];
            lispLibs = [];
            buildScript = pkgs.writeText "build-nshell.lisp" ''
              (require :asdf)
              (setf asdf:*compile-file-warnings-behaviour* :ignore)
              (setf asdf:*compile-file-failure-behaviour* :warn)
              (push (truename "./") asdf:*central-registry*)
              (handler-bind ((warning (lambda (c) (muffle-warning c))))
                (asdf:load-system :nshell))
              (sb-ext:save-lisp-and-die "nshell"
                :executable t
                :compression t
                :toplevel #'nshell:main)
            '';
          };
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/nshell";
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
