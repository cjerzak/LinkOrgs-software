with import <nixpkgs> {};
let
  my-r = rWrapper.override {
    packages = with rPackages; [
       devtools
       Rfast
       stringdist
       glmnet
       fastmatch
       reticulate
       data_table
       plyr
       dplyr
       doParallel
    ];
  };
in
  pkgs.mkShell {
    buildInputs = [
      bashInteractive
      my-r
    ];
    shellHook = ''
      mkdir -p "$(pwd)/_libs"
      export R_LIBS_USER="$(pwd)/_libs"
    '';
  }
