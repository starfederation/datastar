<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar;

use starfederation\datastar\enums\ElementPatchMode;
use starfederation\datastar\events\EventInterface;
use starfederation\datastar\events\PatchElements;
use starfederation\datastar\events\PatchSignals;

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
        $signals = $input ? json_decode($input, true) : [];

        return is_array($signals) ? $signals : [];
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
     * Patches HTML elements into the DOM and returns the resulting output.
     *
     * @param array{
     *     selector?: string|null,
     *     mode?: ElementPatchMode|string|null,
     *     useViewTransition?: bool|null,
     *     eventId?: string|null,
     *     retryDuration?: int|null,
     * } $options
     */
    public function patchElements(string $elements, array $options = []): string
    {
        return $this->sendEvent(new PatchElements($elements, $options));
    }

    /**
     * Patches signals and returns the resulting output.
     */
    public function patchSignals(array|string $signals, array $options = []): string
    {
        return $this->sendEvent(new PatchSignals($signals, $options));
    }

    /**
     * Executes JavaScript in the browser and returns the resulting output.
     */
    public function executeScript(string $script, array $options = []): string
    {
        $elements = '<script ';

        $attributes = $options['attributes'] ?? [];
        foreach ($attributes as $name => $value) {
             $elements .= ' ' . $name . '="' . htmlspecialchars($value, ENT_QUOTES) . '"';
        }

        $autoRemove = $options['autoRemove'] ?? true;
        if ($autoRemove === true) {
             $elements .= ' data-effect="el.remove()"';
        }

        $elements .= '>' . $script . '</script>';

        return $this->patchElements($elements, [
            'selector' => 'body',
            'mode' => ElementPatchMode::Append,
        ]);
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
