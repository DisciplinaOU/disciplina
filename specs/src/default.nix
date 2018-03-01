with import <nixpkgs> {}; {
  thesisTextEnv = stdenv.mkDerivation {
    name = "thesis-text-env";
    buildInputs = [
      (texlive.combine {
        inherit (texlive) 
          scheme-small
          biber
          bbm
          bbm-macros
          collection-langcyrillic
          csquotes
          tabu
          varwidth
          floatrow
	  fontawesome
          footmisc
          algorithms
          algorithmicx
          enumitem
          biblatex
          biblatex-gost
          logreq
          xstring
          lastpage
	  lipsum
	  preprint
          totcount
          chngcntr
          titlesec
          todonotes
          paratype
          was
          filecontents
	  multirow
          cm-super
          subfigure
          appendixnumberbeamer
	  ucs
	  siunitx;
      })
      pdftk
      ghostscript
      biber
      graphviz
    ];
  };
}
