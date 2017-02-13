#!/bin/bash

mkdir -p $HOME/.local/bin/

case "$OSTYPE" in
  linux*)   curl -sS -o $HOME/.local/bin/madlang https://raw.github.com/vmchale/madlibs/blob/master/bin/x86_64-linux/madlang ;;
  ##bsd*)     echo "BSD" ;;
  ##darwin*)  echo "OSX" ;; 
  msys*)    curl -sS -o $HOME/.local/bin/madlang https://raw.github.com/vmchale/madlibs/blob/master/bin/x86_64-win/madlang ;;
  *)        curl -sSL https://get.haskellstack.org/ | sh && stack install madlang ;;
esac

if [[ ! ":$PATH:" == *":$HOME/.local/bin:"* ]]; then
    echo 'PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
fi

madlang --bash-completion-script `which madlang` > tmp
sudo cp tmp /etc/bash_completion.d/madlang
rm tmp
