with import <nixpkgs> {};

mkShell {
  buildInputs = [
    nodejs
    yarn
    helix
    elmPackages.elm-language-server
  ];
}
