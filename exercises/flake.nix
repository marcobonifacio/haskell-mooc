{
	inputs = { nixpkgs.url = "github:nixos/nixpkgs"; };

	outputs = { self, nixpkgs }:
		let pkgs = nixpkgs.legacyPackages.aarch_64-linux;
		in {
			packages.aarch_64-linux = [];

			devShell.aarch_64-linux = pkgs.mkShell {
				buildInputs = [
					haskell.compiler.ghc92
				];
			};
		};
}
