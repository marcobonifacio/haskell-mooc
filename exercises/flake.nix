{
	inputs = { nixpkgs.url = "github:nixos/nixpkgs"; };

	outputs = { self, nixpkgs }:
		let pkgs = nixpkgs.legacyPackages.aarch64-linux;
		in {
			devShell.aarch64-linux = pkgs.mkShell {
				buildInputs = with pkgs; [
					haskell.compiler.ghc92
					haskell.packages.ghc92.haskell-language-server
					fish
					zlib
				];
			};
		};
}
