# Datastar Support JetBrains Plugin

[![Version](https://img.shields.io/jetbrains/plugin/v/26072.svg)](https://plugins.jetbrains.com/plugin/26072)
[![Downloads](https://img.shields.io/jetbrains/plugin/d/26072.svg)](https://plugins.jetbrains.com/plugin/26072)

<!-- Plugin description -->
This plugin offers IntelliJ IDE support for the [Datastar](https://data-star.dev/) framework.

It offers autocomplete for all of the Datastar custom `data-*` attributes, with additional autocomplete of DOM
attributes and events.

You can show the documentation alongside the autocomplete suggestions via the File &rarr; Settings &rarr; Editor &rarr;
General &rarr;
Code Completion: **Show the documentation popup in .... ms checkbox**
<!-- Plugin description end -->

![Screenshot](./resources/img/datastar-intellij-plugin.png)

## Installation

- Using the IDE built-in plugin system:

  <kbd>Settings/Preferences</kbd> > <kbd>Plugins</kbd> > <kbd>Marketplace</kbd> > <kbd>Search for "
  datastar-jetbrains-plugin"</kbd> >
  <kbd>Install</kbd>

- Using JetBrains Marketplace:

  Go to [JetBrains Marketplace](https://plugins.jetbrains.com/plugin/26072) and install it by clicking
  the <kbd>Install to ...</kbd> button in case your IDE is running.

  You can also download the [latest release](https://plugins.jetbrains.com/plugin/26072/versions) from
  JetBrains Marketplace and install it manually using
  <kbd>Settings/Preferences</kbd> > <kbd>Plugins</kbd> > <kbd>⚙️</kbd> > <kbd>Install plugin from disk...</kbd>

- Manually:

  Download the [latest release](https://github.com/starfederation/datastar-jetbrains-plugin/releases/latest) and install it
  manually using
  <kbd>Settings/Preferences</kbd> > <kbd>Plugins</kbd> > <kbd>⚙️</kbd> > <kbd>Install plugin from disk...</kbd>

## Errata

- The Datastar support plugin does have inline documentation and links to the official documentation, but due an
  [issue with the `web-types`](https://github.com/JetBrains/web-types/issues/85) implementation, it does not appear on
  hover currently.
- Autocomplete of Datastar actions in attribute values is not implemented yet
- CI build & release workflows are not implemented yet

Brought to you by [nystudio107](https://nystudio107.com/)

---
Plugin based on the [IntelliJ Platform Plugin Template][template].

[template]: https://github.com/JetBrains/intellij-platform-plugin-template

[docs:plugin-description]: https://plugins.jetbrains.com/docs/intellij/plugin-user-experience.html#plugin-description-and-presentation
