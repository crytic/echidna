let dapptools = builtins.fetchGit {
      url = "https://github.com/dapphub/dapptools";
      rev = "af84e2ee0a0654fdaa91186384233cf1731ee7ce";
    };
in import "${dapptools}" {}
