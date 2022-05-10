let
  myNixPkgs = import <nixpkgs> {
    overlays = [myNixPkgsOverlay];
  };

  myNixPkgsOverlay = (nixSelf: nixSuper: {
    myHaskellPackages = nixSelf.haskellPackages.override (oldHaskellPkgs: {
      overrides = nixSelf.lib.composeExtensions (oldHaskellPkgs.overrides or (_: _: {}))  myHaskellPkgsOverlay;
    });
  });

  myHaskellPkgsOverlay = (hSelf: hSuper: {
    prisma2gql = hSelf.callCabal2nix "prisma2gql" ./. {};
  });

  myDevTools = with myNixPkgs; [
    cabal-install
    haskellPackages.ghcid
    haskellPackages.implicit-hie
    dynamicHLS
  ];

  dynamicHLS = myNixPkgs.haskellPackages.haskell-language-server.overrideAttrs(attrs: {
    configureFlags = ["--enable-executable-dynamic"];
  });

  myShellHook = ''
    alias repl="cabal new-repl"
  '';
in
myNixPkgs.myHaskellPackages.prisma2gql.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ myDevTools;
  buildInputs = oldEnv.buildInputs;
  shellHook = myShellHook;
})
