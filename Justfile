ci:
    cabal new-build
    cabal new-test
    cabal new-bench

size:
    sn d $(fd 'madlang$' -I | tail -n1)

manpages:
    pandoc man/MANPAGE.md -s -t man -o man/madlang.1

upload:
    rm -rf dist/
    cabal sdist
    cabal upload --publish $(fd '\.tar\.gz$')

install:
    cabal new-build
    cp $(fd 'madlang$' -I | tail -n1) ~/.local/bin

clean:
    sn c .
    rm -f madlang.1 tags
