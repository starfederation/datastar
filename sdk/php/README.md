[![Stable Version](https://img.shields.io/packagist/v/starfederation/datastar-php?label=stable)]((https://packagist.org/packages/starfederation/datastar-php))
[![Total Downloads](https://img.shields.io/packagist/dt/starfederation/datastar-php)](https://packagist.org/packages/starfederation/datastar-php)

<p align="center"><img width="150" src="https://putyourlightson.com/assets/logos/datastar.svg"></p>

# Datastar PHP SDK

This package provides a PHP SDK for working with [Datastar](https://data-star.dev/).

## License

This package is licensed for free under the MIT License.

## Requirements

This package requires PHP 8.1 or later.

## Installation

Install using composer.

```shell
composer require starfederation/datastar-php
```

## Usage

```php
use starfederation\datastar\enums\EventType;
use starfederation\datastar\enums\ElementPatchMode;
use starfederation\datastar\ServerSentEventGenerator;

// Creates a new `ServerSentEventGenerator` instance.
$sse = new ServerSentEventGenerator();

// Sends the response headers. 
// If your framework has its own way of sending response headers, manually send the headers returned by `ServerSentEventGenerator::headers()` instead.
$sse->sendHeaders();

// Patches elements into the DOM.
$sse->patchElements('<div></div>', [
    'selector' => '#my-div',
    'mode' => ElementPatchMode::Append,
    'useViewTransition' => true,
]);

// Patches elements into the DOM.
$sse->removeElements('#my-div', [
    'useViewTransition' => true,
]);

// Patches signals.
$sse->patchSignals('{foo: 123}', [
    'onlyIfMissing' => true,
]);

// Executes JavaScript in the browser.
$sse->executeScript('console.log("Hello, world!")', [
    'autoRemove' => true,
    'attributes' => [
        'type' => 'application/javascript',
    ],
]);

// Redirects the browser by setting the location to the provided URI.
$sse->location('/guide');
```

```php
use starfederation\datastar\ServerSentEventGenerator;

$signals = ServerSentEventGenerator::readSignals();
```
