# Sandbox for trying Elm, elm-mdl, etc

## elm-live

https://github.com/wking-io/elm-live

```bash
npm install --global elm elm-live@next
```

elm-live with elm-mdl and custom HTML page
* Live reload of .elm files
* elm-mdl: using supplied index.html (can e.g. rename it as custom.html)
  * Using elm-live index.html it works misses the required style dependencies for elm-mdl
* WSL container: ip address specified instead of localhost
* SPA client routing with Browser.navigation: --pushstate
* with elm debugger: --debug

```bash
elm-live src/Main.elm -h 172.21.183.217 -p 8000 --pushstate --start-page elm-mdl-index.html -- --output=elm.js --debug
```

## elm-mdl

https://github.com/aforemny/elm-mdc

