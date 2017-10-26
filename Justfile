latest:
    @echo 'github:'
    @hub release | head -n2 | tail -n1
    @echo 'hackage:'
    @cabal list madlang | grep -P -o '\d+\.\d+\.\d+\.\d+' | head -n1

timed:
    time madlang sample demo/shakespeare.mad

darcs:
    darcs optimize clean
    darcs optimize pristine
    darcs optimize cache

shrink:
    upx $(fd 'madlang$' -I | tail -n1)

tokei:
    tokei . .travis.yml

next:
    @export VERSION=$(cat madlang.cabal | grep -P -o '\d+\.\d+\.\d+\.\d+' madlang.cabal | head -n1 | awk -F. '{$NF+=1; print $0}' | sed 's/ /\./g') && echo $VERSION && sed -i "2s/[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/$VERSION/" madlang.cabal
    git commit -am "next"
    sn c .

ci:
    @cabal new-build
    @cabal new-test
    @cabal new-bench
    @stack build --test --no-run-tests
    @stack bench --no-bench
    weeder .
    hlint .

size:
    sn d $(fd 'madlang$' -I | tail -n1)

manpages:
    pandoc man/MANPAGE.md -s -t man -o man/madlang.1

upload:
    rm -rf dist/ .ghc.environment.*
    cabal sdist
    cabal upload --publish $(fd '\.tar\.gz$' -I)

release:
    git tag "$(grep -P -o '\d+\.\d+\.\d+\.\d+' madlang.cabal | head -n1)"
    git push origin --tags
    git tag -d "$(grep -P -o '\d+\.\d+\.\d+\.\d+' madlang.cabal | head -n1)"
    git push origin master

install:
    cabal new-build --constraint='madlang +llvm-fast'
    cp $(fd 'madlang$' -I | tail -n1) ~/.local/bin

name:
    github-release edit -s $(cat .git-token) -u vmchale -r madlang -n "$(madlang run ~/programming/madlang/releases/releases.mad)" -t "$(grep -P -o '\d+\.\d+\.\d+\.\d+' madlang.cabal | head -n1)"

clean:
    sn c .
    rm -f .ghc.environment.* man/madlang.1 tags
