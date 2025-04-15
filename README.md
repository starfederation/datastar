[![Version](https://img.shields.io/github/package-json/v/starfederation/datastar?filename=library/package.json)](https://github.com/starfederation/datastar/releases)
[![License](https://img.shields.io/github/license/starfederation/datastar)](https://github.com/starfederation/datastar/blob/main/LICENSE)
[![Stars](https://img.shields.io/github/stars/starfederation/datastar?style=flat)](https://github.com/starfederation/datastar/stargazers)

<p align="center"><img width="200" src="https://data-star.dev/static/images/rocket.webp"></p>

# Datastar

### The hypermedia framework.

Datastar helps you build reactive web applications with the simplicity of server-side rendering and the power of a full-stack SPA framework.

Getting started is as easy as adding a single 14.5 KiB script tag to your HTML.

```html
<script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.11/bundles/datastar.js"></script>
```

Then start adding frontend reactivity using declarative <code>data-*</code> attributes.

```html
<input data-bind-title />
<div data-text="$title.toUpperCase()"></div>
<button data-on-click="@post('/endpoint')">Save</button>
```

Visit the [Datastar Website »](https://data-star.dev/)

Watch the [Videos »](https://www.youtube.com/@data-star)

Join the [Discord Server »](https://discord.com/invite/bnRNgZjgPh)

## Getting Started

Read the [Getting Started Guide »](https://data-star.dev/guide/getting_started)

## Contributing

Read the [Contribution Guidelines »](https://github.com/starfederation/datastar/blob/develop/CONTRIBUTING.md)

## Custom Plugins

You can manually add your own plugins to the core:

```html
<script type="importmap">
{
    "imports": {
      "datastar": "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.11/bundles/datastar.js"
    }
}
</script>
<script type="module">
    import { load } from 'datastar'

    load(
        // Look ma’, I made a plugin!
    )
</script>
```

[![Star History Chart](https://api.star-history.com/svg?repos=starfederation/datastar&type=Date)](https://www.star-history.com/#starfederation/datastar&Date)
