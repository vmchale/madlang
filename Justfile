size:
    sn d $(fd 'madlang$' -I | tail -n1)

manpages:
    pandoc man/MANPAGE.md -s -t man -o man/madlang.1

upload:
    rm -rf dist/
    cabal sdist
    cabal upload --publish $(fd '\.tar\.gz$')

# should it remove past libraries?

fetch:
    rget vmchale.com/static/packages.tar.gz
    tar xvf packages.tar.gz
    mv packages ~/.madlang

shell-completions:
    ./bash/mkCompletions

install:
    cabal new-build
    cp $(fd 'madlang$' -I | tail -n1) ~/.local/bin

clean:
    sn c .
    rm -f madlang.1 tags

libs:
    darcs clone https://hub.darcs.net/vmchale/madlang-libraries --repodir=~/.madlang/
