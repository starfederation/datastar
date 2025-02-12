# Release Notes for Datastar

Each tagged version of Datastar is accompanied by a release note. Read the [release notes »](https://github.com/starfederation/datastar/releases)

# WIP Release Notes

## v1.0.0-beta.6

### Fixed

- Fixed a bug in which the `data-on-signals-change` attribute expression was not being executed when signal values changed ([#641](https://github.com/starfederation/datastar/issues/641)).
- Fixed a bug in which the signal created by `data-ref` was being removed during the cleanup process ([#642](https://github.com/starfederation/datastar/issues/642)).
- Fixed a bug in which errors were being caught in preact core, which was obfuscating the original error ([#643](https://github.com/starfederation/datastar/issues/643)).
- Fixed a bug in which morphed DOM elements could lose their focus, due to a change of behavior in Idiomorph ([#645](https://github.com/starfederation/datastar/issues/645)).