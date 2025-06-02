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
    }

    /**
     * Sends the response headers, if not already sent.
     */
    public function sendHeaders(): void
    {
        if (headers_sent()) {
            return;
        }

        foreach (static::headers() as $name => $value) {
            header("$name: $value");
        }
    }

    /**
     * Merges HTML fragments into the DOM and returns the resulting output.
     *
     * @param array{
     *     selector?: string|null,
     *     mergeMode?: FragmentMergeMode|string|null,
     *     useViewTransition?: bool|null,
     *     eventId?: string|null,
     *     retryDuration?: int|null,
     * } $options
     */
    public function mergeFragments(string $fragments, array $options = []): string
    {
        return $this->sendEvent(new MergeFragments($fragments, $options));
    }

    /**
     * Removes HTML fragments from the DOM and returns the resulting output.
     *
     * @param array{
     *      eventId?: string|null,
     *      retryDuration?: int|null,
     *  } $options
     */
    public function removeFragments(string $selector, array $options = []): string
    {
        return $this->sendEvent(new RemoveFragments($selector, $options));
    }

    /**
     * Merges signals and returns the resulting output.
     */
    public function mergeSignals(array|string $signals, array $options = []): string
    {
        return $this->sendEvent(new MergeSignals($signals, $options));
    }

    /**
     * Removes signal paths and returns the resulting output.
     */
    public function removeSignals(array $paths, array $options = []): string
    {
        return $this->sendEvent(new RemoveSignals($paths, $options));
    }

    /**
     * Executes JavaScript in the browser and returns the resulting output.
     */
    public function executeScript(string $script, array $options = []): string
    {
        return $this->sendEvent(new ExecuteScript($script, $options));
    }

    /**
     * Redirects the browser by setting the location to the provided URI and returns the resulting output.
     */
    public function location(string $uri, array $options = []): string
    {
        $script = "setTimeout(() => window.location = '$uri')";
        
        return $this->executeScript($script, $options);
    }

    /**
     * Sends an event, flushes the output buffer and returns the resulting output.
     */
    protected function sendEvent(EventInterface $event): string
    {
        $output = $event->getOutput();
        echo $output;

        if (ob_get_contents()) {
            ob_end_flush();
        }
        flush();

        return $output;
    }
}
