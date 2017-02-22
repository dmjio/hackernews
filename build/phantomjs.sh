if [ "$GHCVER" = "ghcjs" ]; then
  phantomjs --web-security=no jsbits/options.js result/bin/ghcjs-tests.jsexe/index.html;
fi
