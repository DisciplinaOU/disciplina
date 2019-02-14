{ pkgs ? import ./pkgs.nix, shell ? false }: with pkgs;
let
  withPostgreSQL = drv: pkgs.haskell.lib.overrideCabal drv (a: {
    testToolDepends = (a.testToolDepends or []) ++
      [ pkgs.ephemeralpg pkgs.postgresql pkgs.getopt ];
    preCheck = (a.preCheck or "") + ''
      export TEST_PG_CONN_STRING=$(pg_tmp -w 600)
    '';
    # we need the temporary directory from pg_tmp
    # so extract it out of $TEST_PG_CONN_STRING
    postCheck = (a.postCheck or "") + ''
      pg_tmp stop -d $(echo ''${TEST_PG_CONN_STRING#*=} | sed 's:%2F:/:g') || :
    '';
  });
  addLatex = drv: pkgs.haskell.lib.overrideCabal drv (old: {
    libraryToolDepends = (old.libraryToolDepends or []) ++
      [ (texlive.combine {
          inherit (texlive)
            collection-basic
            collection-fontsrecommended
            collection-langcyrillic
            collection-xetex
            scheme-basic
            extsizes
            titlesec
            url
            hyperref
            xltxtra
            geometry
            background
            realscripts
            datetime2
            tracklang
            etoolbox
            everypage
            xkeyval
            xcolor
            pgf
            fontspec;
        })
      ];
  });
in
stackToNix {
  # TODO: properly fix this in stack-to-nix
  root = constGitIgnore "disciplina-src" ./. [
    "*.nix"
    "/ChangeLog.md"
    "/README.md"
    "/.buildkite"
    "/config.yaml"
    "/scripts"
    "/secrets"
    "/specs"
  ];
  inherit shell;
  overrides = self: previous: {
    disciplina-educator =
      (withPostgreSQL
        (addLatex previous.disciplina-educator)
      );

    HList = pkgs.haskell.lib.overrideCabal previous.HList (o: {
      preCompileBuildDriver = ''
        rm -f Setup.lhs Setup.hs
        cat << EOF > Setup.hs
        import Distribution.Simple
        main = defaultMain
        EOF
      '';
    });
  };
}
