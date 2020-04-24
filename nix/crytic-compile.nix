{ buildPythonPackage, fetchPypi, python3Packages }:

buildPythonPackage rec {
  pname = "crytic-compile";
  version = "0.1.7";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0s4nr3jrm7665qyd04j3rxij0a58dwrfvc0sn05c7c5zwr0x155s";
  };

  propagatedBuildInputs = with python3Packages; [ pysha3 setuptools ];

  doCheck = false;
}
