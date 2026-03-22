# Personal blog

## Build and deploy

```sh
cabal run site build
npx wrangler login  # Only needed once
npx wrangler deploy
```
