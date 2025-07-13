# Datastar SDKs

The Datastar SDKs have moved to their own repositories for better maintainability and language-specific development.

## Official SDKs

| Language | Repository | Package |
|----------|------------|---------|
| Go | [starfederation/datastar-go](https://github.com/starfederation/datastar-go) | `go get github.com/starfederation/datastar-go` |
| PHP | [starfederation/datastar-php](https://github.com/starfederation/datastar-php) | `composer require starfederation/datastar` |

## SDK Development

### For SDK Authors

The SDK configuration has moved from a Go-based build system to a JSON-based configuration:

- **Configuration**: [`datastar-sdk-config-v1.json`](./datastar-sdk-config.json) - Contains all constants and defaults
- **Schema**: [`datastar-sdk-config-v1.schema.json`](./datastar-sdk-config.schema.json-v1.json) - JSON schema for validation
- **Migration Guide**: [`SDK_MIGRATION_GUIDE.md`](./SDK_MIGRATION_GUIDE.md) - Guide for migrating from the old build system
- **Architecture Decision Record**: [`ADR.md`](./ADR.md) - SDK architecture and specification

### Creating a New SDK

1. Read the [Architecture Decision Record](./ADR.md) for SDK specifications
2. Use [`datastar-sdk-config.json`](./datastar-sdk-config.json) as your source of constants
3. Implement the required `ServerSentEventGenerator` interface
4. Follow language-specific naming conventions while maintaining API consistency

## Community SDKs

We welcome community contributions! If you've created a Datastar SDK for a language not listed above, please open a PR to add it here.

## Support

For SDK-specific issues, please open an issue in the respective SDK repository. For general Datastar questions or cross-SDK concerns, use the main [Datastar repository](https://github.com/starfederation/datastar).