{ cabal, distributive, JuicyPixels, linear, OpenGL, profunctors, sdl2, text, vector, diagrams, diagramsGtk
}:
cabal.mkDerivation (self: {
  pname = "hadoom";
  src = ./.;
  version = "1.0";
  buildDepends = [ distributive JuicyPixels linear OpenGL profunctors sdl2 text vector diagrams diagramsGtk ];
})