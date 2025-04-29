<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar;

use starfederation\datastar\enums\FragmentMergeMode;
use starfederation\datastar\events\EventInterface;
use starfederation\datastar\events\ExecuteScript;
use starfederation\datastar\events\MergeFragments;
use starfederation\datastar\events\MergeSignals;
use starfederation\datastar\events\RemoveFragments;
use starfederation\datastar\events\RemoveSignals;

class ServerSentEventGenerator
{
    /**
     * The response headers that should be sent.
     */
    public static function headers(): array
    {
        $headers = [
            'Cache-Control' => 'no-cache',
            'Content-Type' => 'text/event-stream',
            // Disable buffering for Nginx.
            // https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_buffering
            'X-Accel-Buffering' => 'no',
        ];

        // Connection-specific headers are only allowed in HTTP/1.1.
        // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Connection
        if ($_SERVER['SERVER_PROTOCOL'] === 'HTTP/1.1') {
            $headers['Connection'] = 'keep-alive';
        }

        return $headers;
    }

    /**
     * Returns the signals sent in the incoming request.
     */
    public static function readSignals(): array
    {
        $input = $_GET[Consts::DATASTAR_KEY] ?? file_get_contents('php://input');

        return $input ? json_decode($input, true) : [];
    }

    /**
     * Constructor.
     */
    public function __construct()
    {
        // Abort the process if the client closes the connection.
        ignore_user_abort(false);

        // Sets the response headers only if not already sent and not yet set.
        if (!headers_sent()) {
            foreach (static::headers() as $name => $value) {
                header("$name: $value", false);
            }
        }
    }

    /**
     * Merges HTML fragments into the DOM.
     *
     * @param array{
     *     selector?: string|null,
     *     mergeMode?: FragmentMergeMode|string|null,
     *     useViewTransition?: bool|null,
     *     eventId?: string|null,
     *     retryDuration?: int|null,
     * } $options
     */
    public function mergeFragments(string $fragments, array $options = []): void
    {
        $this->sendEvent(new MergeFragments($fragments, $options));
    }

    /**
     * Removes HTML fragments from the DOM.
     *
     * @param array{
     *      eventId?: string|null,
     *      retryDuration?: int|null,
     *  } $options
     */
    public function removeFragments(string $selector, array $options = []): void
    {
        $this->sendEvent(new RemoveFragments($selector, $options));
    }

    /**
     * Merges signals into the signals.
     */
    public function mergeSignals(array|string $signals, array $options = []): void
    {
        $this->sendEvent(new MergeSignals($signals, $options));
    }

    /**
     * Removes signal paths from the signals.
     */
    public function removeSignals(array $paths, array $options = []): void
    {
        $this->sendEvent(new RemoveSignals($paths, $options));
    }

    /**
     * Executes JavaScript in the browser.
     */
    public function executeScript(string $script, array $options = []): void
    {
        $this->sendEvent(new ExecuteScript($script, $options));
    }

    /**
     * Redirects the browser by setting the location to the provided URI.
     */
    public function location(string $uri, array $options = []): void
    {
        $script = 'setTimeout(() => window.location = "' . $uri . '")';
        $this->executeScript($script, $options);
    }

    /**
     * Sends an event and flushes the output buffer.
     */
    protected function sendEvent(EventInterface $event): void
    {
        echo $event->getOutput();

        if (ob_get_contents()) {
            ob_end_flush();
        }
        flush();
    }
}
