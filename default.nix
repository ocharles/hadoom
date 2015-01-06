{ cabal, netwire, basePrelude, nonEmpty
, distributive, gl, JuicyPixels, linear, monadLoops, profunctors, sdl2, text, vector
, diagrams, diagramsGtk, reactiveBanana, gtk3
}:
cabal.mkDerivation (self: {
  pname = "hadoom";
  src = ./.;
  version = "1.0";
  buildDepends = [
    distributive gl JuicyPixels linear monadLoops profunctors sdl2 text vector
    diagrams diagramsGtk reactiveBanana gtk3 netwire basePrelude nonEmpty
  ];
})
