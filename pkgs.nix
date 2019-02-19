(import (fetchGit {
  url = https://github.com/serokell/serokell-closure;
  rev = "f5e7e6480eda346667aa24323d4b042fc2943515";
  ref = "20190116140155";
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
