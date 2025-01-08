[![Stable Version](https://img.shields.io/packagist/v/starfederation/datastar-php?label=stable)]((https://packagist.org/packages/starfederation/datastar-php))
[![Total Downloads](https://img.shields.io/packagist/dt/starfederation/datastar-php)](https://packagist.org/packages/starfederation/datastar-php)

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
use starfederation\datastar\enums\FragmentMergeMode;
use starfederation\datastar\ServerSentEventGenerator;

// Creates a new `ServerSentEventGenerator` instance.
$sse = new ServerSentEventGenerator();

// Sends the response headers. 
// If your framework has its own way of sending response headers, manually send the headers returned by `ServerSentEventGenerator::headers()` instead.
$sse->sendHeaders();

// Merges HTML fragments into the DOM.
$sse->mergeFragments('<div></div>', [
    'selector' => '#my-div',
    'mergeMode' => FragmentMergeMode::Append,
    'settleDuration' => 1000,
    'useViewTransition' => true,
]);

// Removes HTML fragments from the DOM.
$sse->removeFragments('#my-div');

// Merges signals.
$sse->mergeSignals('{foo: 123}', [
    'onlyIfMissing' => true,
]);

// Removes signals.
$sse->removeSignals(['foo', 'bar']);

// Executes JavaScript in the browser.
$sse->executeScript('console.log("Hello, world!")');
```

```php
use starfederation\datastar\ServerSentEventGenerator;

$signals = ServerSentEventGenerator::readSignals();
```