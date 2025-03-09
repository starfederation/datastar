# Custom Builds

Datastar consists of a core library and a set of plugins that provide the functionality that makes up the framework. The plugins have been curated to give you everything you need to build a modern web application, while avoiding bloat.

## Custom Builds

Datastar is built using a modular architecture that allows you to create custom builds that include only the plugins you need. While this is unnecessary for most applications, it can be useful if you want to intentionally reduce the surface area of what Datastar can do.

### Bundler

The easiest way to create a custom build is to use the [bundler](/bundler), which allows you to include only the plugins you need. The bundler will automatically include the core library and any dependencies required by the plugins you select.

It is possible to alias `data-*` attributes to a custom alias (`data-foo-*`, for example) using the [bundler](/bundler). A custom alias should _only_ be used if you have a conflict with a legacy library and [`data-star-ignore`](#data-star-ignore) cannot be used.

We maintain a `data-ds-*` aliased version that can be included as follows.

```html
<script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.9/bundles/datastar-aliased.js"></script>
```

### NPM

Alternatively, you can use the NPM package `@starfederation/datastar` to create a custom build. The NPM package exports all official plugins and bundles, so you can include only the plugins you need.

```js
import { load } from '@starfederation/datastar/bundles/datastar-core'
import { Class, Show, Text } from '@starfederation/datastar/plugins'

load(Class, Show, Text)
```
