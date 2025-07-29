# Datastar PocketBase SDK

[PocketBase](https://pocketbase.io) was originally designed as a SPA backend, but can be extended with [PocketPages](https://pocketpages.dev) to support multi-page applications (MPA).

> For complete documentation, visit [https://github.com/benallfree/pocketpages/tree/main/packages/plugins/datastar](https://github.com/benallfree/pocketpages/tree/main/packages/plugins/datastar).

## Quick Start

The fastest way to get started is using the Datastar starter template:

```bash
npm create pocketpages datastar myapp
cd myapp
pocketbase serve --dir=pb_data --dev
```

## Adding Datastar to an Existing PocketPages Project

Install the Datastar plugin:

```bash
npm i pocketpages-plugin-datastar
```

Configure the plugin in `./pb_hooks/pages/+config.js`:

```js
module.exports = {
  plugins: ['pocketpages-plugin-datastar'],
}
```
