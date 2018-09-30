{ pkgs ? import ../pkgs.nix }: with pkgs;

let
  latex = texlive.combine {
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
  };
in

stdenv.mkDerivation {
  name = "disciplina-specs";
  src = lib.cleanSource ./src;

  nativeBuildInputs = [
    latex
    pdftk
    ghostscript
    biber
    graphviz
  ];

  buildPhase = ''
    export HOME=$PWD

    for d in */; do
      (cd $d && pdflatex main.tex)
    done
  '';

  installPhase = ''
    mkdir $out

    for f in */main.pdf; do
      mv $f $out/$(dirname $f).pdf
    done
  '';
}
