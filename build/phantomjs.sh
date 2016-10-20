if [ "$SCRIPT" = "ghcjs.nix" ]; then
  phantomjs --web-security=no jsbits/options.js result-2/bin/ghcjs-tests.jsexe/index.html;
fi
