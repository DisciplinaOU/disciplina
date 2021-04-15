(import (fetchGit {
  url = https://github.com/DisciplinaOU/serokell-closure;
  rev = "7b26d2aa7b99b11201679b51d80a963f9aac4847";
  ref = "20210415.2126";
})).extend(final: previous: {
  pdf-generator-xelatex = with previous; texlive.combine {
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
  };
})
