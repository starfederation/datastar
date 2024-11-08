<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar;

use starfederation\datastar\events\EventInterface;
use starfederation\datastar\enums\ConsoleMode;
use starfederation\datastar\enums\EventType;
use starfederation\datastar\enums\FragmentMergeMode;
use starfederation\datastar\events\Console;
use starfederation\datastar\events\Remove;
use starfederation\datastar\events\Fragment;
use starfederation\datastar\events\Redirect;
use starfederation\datastar\events\Signal;

class ServerSentEventGenerator
{
    protected const DEFAULT_SSE_SEND_RETRY = 1;

    protected int $id = 0;

    public function __construct()
    {
        $this->sendHeaders();
    }

    /**
     * Sends a Datastar event.
     *
     * @param EventType $eventType
     * @param string[] $dataLines
     * @param array{
     *     id: string|null,
     *     retry: int|null,
     * } $options
     */
    public function send(EventType $eventType, array $dataLines, array $options = []): void
    {
        $eventData = new ServerSentEventData(
            eventType: $eventType,
            data: $dataLines,
            retry: static::DEFAULT_SSE_SEND_RETRY,
        );

        foreach ($options as $key => $value) {
            if (property_exists($eventData, $key)) {
                $eventData->$key = $value;
            }
        }

        if (empty($eventData->id)) {
            $eventData->id = ++$this->id;
        }

        $output = [
            'event: ' . $eventData->eventType->value,
            'id: ' . $eventData->id,
            'retry: ' . $eventData->retry,
        ];

        foreach ($eventData->data as $line) {
            $output[] = 'data: ' . $line;
        }

        echo implode("\n", $output) . "\n\n";

        if (ob_get_contents()) {
          ob_end_flush();
        }
        flush();
    }

    /**
     * Inserts a fragment into the DOM.
     *
     * /**
     * @param string $data
     * @param array{
     *     selector: string|null,
     *     merge: FragmentMergeMode|null,
     *     settleDuration: int|null,
     *     useViewTransition: bool|null,
     * } $options
     */
    public function renderFragment(string $data, array $options = []): void
    {
        $this->sendEvent(new Fragment($data, $options));
    }

    /**
     * Removes one or more fragments from the DOM.
     */
    public function removeFragments(string $selector, array $options = []): void
    {
        $this->sendEvent(new Remove($selector, $options));
    }

    /**
     * Updates values in the store.
     */
    public function patchStore(string $data, array $options = []): void
    {
        $this->sendEvent(new Signal($data, $options));
    }

    /**
     * Removes one or more paths from the store.
     */
    public function removeFromStore(array $paths): void
    {
        $this->sendEvent(new Remove(paths: $paths));
    }

    /**
     * Redirects the browser.
     */
    public function redirect(string $url): void
    {
        $this->sendEvent(new Redirect($url));
    }

    /**
     * Sends a message to the browser console.
     */
    public function console(ConsoleMode $mode, string $message): void
    {
        $this->sendEvent(new Console($mode, $message));
    }

    /**
     * Sends the response headers, if not already sent.
     */
    protected function sendHeaders(): void
    {
        if (headers_sent()) {
            return;
        }

        header('Content-Type: text/event-stream');
        header('Cache-Control: no-cache');
        header('Connection: keep-alive');

        // Disable buffering for Nginx
        // https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_buffering
        header('X-Accel-Buffering: no');
    }

    /**
     * Sends an event.
     */
    protected function sendEvent(EventInterface $event): void
    {
        $this->send(
            $event->getEventType(),
            $event->getDataLines(),
        );
    }
}
