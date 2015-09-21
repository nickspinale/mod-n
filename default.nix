{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "mod-n";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/nickspinale/mod-n";
  description = "The integers with a modulus";
  license = stdenv.lib.licenses.mit;
}
