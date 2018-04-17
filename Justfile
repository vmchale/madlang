latest:
    @echo 'github:'
    @hub release | head -n2 | tail -n1
    @echo 'hackage:'
    @cabal list madlang | grep -P -o '\d+\.\d+\.\d+\.\d+' | head -n1

check:
    git diff master origin/master

deps:
    stack list-dependencies | less

timed:
    time madlang sample demo/shakespeare.mad

darcs:
    darcs optimize clean
    darcs optimize pristine
    darcs optimize cache

next:
    @export VERSION=$(ac madlang.cabal | grep -P -o '\d+\.\d+\.\d+\.\d+' madlang.cabal | head -n1 | awk -F. '{$NF+=1; print $0}' | sed 's/ /\./g') && echo $VERSION && sed -i "2s/[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/$VERSION/" madlang.cabal
    git commit -am "next"
    sn c .

install:
    cp $(fd -t x 'madlang$' -IH) ~/.cabal/bin

ci:
    @cabal new-build all
    @cabal new-test
    @stack build --test --bench --no-run-tests --no-run-benchmarks
    weeder .
    hlint src app test bench
    yamllint stack.yaml
    yamllint .travis.yml
    yamllint appveyor.yml
    yamllint .stylish-haskell.yaml
    yamllint .hlint.yaml
    yamllint .yamllint

size:
    sn d $(fd 'madlang$' -I | head -n2 | tail -n1)

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

name:
    github-release edit -s $(ac ~/.git-token) -u vmchale -r madlang -n "$(madlang run ~/programming/madlang/releases/releases.mad)" -t "$(grep -P -o '\d+\.\d+\.\d+\.\d+' madlang.cabal | head -n1)"

clean:
    sn c . -g
    rm -f man/madlang.1 madlang.hp madlang.ps madlang.aux
