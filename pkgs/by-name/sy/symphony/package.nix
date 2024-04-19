{ lib
, stdenv
, fetchFromGitHub
, cmake
, coin-utils
, clp
, cgl
, CoinMP
, gfortran
, libtool
, glpk
, osi
, pkg-config
}:

stdenv.mkDerivation rec {
  pname = "symphony";
  version = "5.7.2";

  outputs = [ "out" ];

  src = fetchFromGitHub {
    owner = "coin-or";
    repo = "SYMPHONY";
    rev = "releases/${version}";
    sha256 = "sha256-OdTUMG3iVhjhw5uKtUnsLCZ4DfMjYHm8+/ozfmw7J6c=";
  };

  nativeBuildInputs = [ libtool pkg-config glpk gfortran CoinMP coin-utils osi ];
  propagatedNativeBuildInputs = [ cgl clp ];


  meta = {
    description = "SYMPHONY is an open-source solver, callable library, and development framework for mixed-integer linear programs (MILPs) written in C with a number of unique features";
    homepage = "https://www.coin-or.org/SYMPHONY/index.htm";
    platforms = [ "x86_64-linux" ];
    license = lib.licenses.epl20;
    maintainers = with lib.maintainers; [ b-rodrigues ];
  };
}
