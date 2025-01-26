# Personal blog

## Build and deploy

Uses [Cloudflare Pages direct upload](https://developers.cloudflare.com/pages/get-started/direct-upload/).

```sh
cabal run site build
npx wrangler login  # Only needed once
npx wrangler pages deploy
```
