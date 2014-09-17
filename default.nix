{ cabal, linear, OpenGL, sdl2, vector
}:
cabal.mkDerivation (self: {
  pname = "hadoom";
  src = ./.;
  version = "1.0";
  buildDepends = [ linear OpenGL sdl2 vector ];
})