#!/bin/bash

src_dir=$(dirname $(readlink -e $0))
root_dir=$(dirname $src_dir)
artifacts_dir="$root_dir/disciplina"

doc=$1
version=$2

doc_src_dir="$src_dir/$doc"

if [ ! -d "$doc_src_dir" ]; then
    echo "No sources for document '$doc' found"
    exit 1
fi

existing_artifact=$(find $artifacts_dir -name "*${doc}*")

if [ -n "$existing_artifact" ]; then
    artifact=$existing_artifact
else
    artifact="$artifacts_dir/$doc-$version.pdf"
fi

pushd $doc_src_dir

pdflatex main
biber    main
pdflatex main
pdflatex main

rm main.{bib,aux,log,bbl,bcf,blg,run.xml,toc,tct,out}

mv -f main.pdf $artifact

popd
