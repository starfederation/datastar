# Datastar PHP SDK

## Usage

```php
use starfederation\datastar\enums\ConsoleMode;
use starfederation\datastar\enums\EventType;
use starfederation\datastar\enums\FragmentMergeMode;
use starfederation\datastar\ServerSentEventGenerator;

// Creates a new `ServerSentEventGenerator` instance.
$sseGenerator = new ServerSentEventGenerator();

// Sends an SSE event of the given type.
$sseGenerator->send(EventType::Fragment, ['<div></div>'], [
    'id' => '123',
    'retry' => 500,
]);

// Inserts a fragment into the DOM.
$sseGenerator->renderFragment('<div></div>', [
    'selector' => '#my-div',
    'merge' => FragmentMergeMode::Append,
    'settleDuration' => 1000,
    'useViewTransition' => true,
]);

// Removes one or more fragments from the DOM.
$sseGenerator->removeFragments('#my-div');

// Updates values in the store.
$sseGenerator->patchStore(['foo' => 123], ['onlyIfMissing' => true]);

// Removes one or more paths from the store.
$sseGenerator->removeFromStore(['foo', 'bar']);

// Redirects the browser.
$sseGenerator->redirect('/success');

// Sends a message to the browser console.
$sseGenerator->console(ConsoleMode::Log, 'Hello, world!');
```

```php
use starfederation\datastar\ParseIncoming;

$store = ParseIncoming::getStore();
```
