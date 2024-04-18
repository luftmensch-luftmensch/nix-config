{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
buildGoModule rec {
  pname = "rHttp";
  name = pname;

  src = fetchFromGitHub {
    owner = "1buran";
    repo = "rHttp";
    rev = "9b7da3a0f7c2e35c9d326e7920ded15f806f8113";
    sha256 = "1nz3f6zgpbxlwn6c5rqxa8897ygi5r7h7f6624i27rq9kr729cra";
  };

  vendorHash = lib.fakeHash;

  meta = {
    description = "Go REPL for HTTP";
    homepage = "https://github.com/1buran/rHttp";
    license = lib.licenses.agpl3;
    maintainers = [];
  };
}
