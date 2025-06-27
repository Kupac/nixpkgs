{ lib
, clangStdenv
, fetchFromGitHub
, gnustep-make
, wrapGNUstepAppsHook
, gnustep-base
, libGLU
, libogg
, nspr
, SDL
, libvorbis
, espeak-classic
, openal
, xorg
}:

clangStdenv.mkDerivation (attrs: {
  pname = "oolite";
  version = "1.90";

  src = fetchFromGitHub {
    owner = "OoliteProject";
    repo = "oolite";
    fetchSubmodules = true;
    rev = attrs.version;
    hash = "sha256-fCYYZR8oXd5z/L7je0fFIXTXjYk/nLcrwm+AsV/Ncbo=";
  };

  hardeningDisable = [ "format" ];

  env.NIX_CFLAGS_COMPILE = " -Wno-error=incompatible-pointer-types -Wno-error=implicit-function-declaration";

  nativeBuildInputs = [
    gnustep-make
    wrapGNUstepAppsHook
  ];

  buildInputs = [
    gnustep-base
    libGLU
    libogg
    nspr
    SDL
    libvorbis
    espeak-classic
    openal
    xorg.libX11
  ];

  meta = {
    description = "3D space trading and combat simulator in the spirit of Elite";
    mainProgram = "oolite";
    homepage = "http://oolite.space/";
    downloadPage = "https://oolite.space/#download";
    license = lib.licenses.gpl2;
    maintainers = [ lib.maintainers.Kupac ];
    platforms = lib.platforms.all;
  };
})
