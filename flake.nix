
{
  description = "Scoreboard for turn based games where points can be made at each turn.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      system = "x86_64-linux";
    in rec {
        devShells.${system}.default = pkgs.mkShell {
          buildInputs = [
            pkgs.sbcl

            pkgs.gnuplot
            pkgs.cairo
            pkgs.openssl
            pkgs.sqlite
            pkgs.plan9port
            pkgs.rc
            pkgs.redo-apenwarr
          ];
          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath([pkgs.openssl pkgs.sqlite])}
          '';
       };
    };
}
