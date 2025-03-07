<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

use starfederation\datastar\Consts;
use starfederation\datastar\ServerSentEventData;

trait EventTrait
{
    public ?string $eventId = null;
    public ?int $retryDuration = null;

    /**
     * @inerhitdoc
     */
    public function getOptions(): array
    {
        $options = [];

        if (!empty($this->eventId)) {
            $options['eventId'] = $this->eventId;
        }

        if (!empty($this->retryDuration) && $this->retryDuration != Consts::DEFAULT_SSE_RETRY_DURATION) {
            $options['retryDuration'] = $this->retryDuration;
        }

        return $options;
    }

    /**
     * @inerhitdoc
     */
    public function getBooleanAsString(bool $value): string
    {
        return $value ? 'true' : 'false';
    }

    /**
     * @inerhitdoc
     */
    public function getDataLine(string $datalineLiteral, string|int $value = ''): string
    {
        return 'data: ' . $datalineLiteral . $value;
    }

    /**
     * @inerhitdoc
     */
    public function getMultiDataLines(string $datalineLiteral, string $data): array
    {
        $dataLines = [];
        $lines = explode("\n", trim($data));

        foreach ($lines as $line) {
            $dataLines[] = $this->getDataLine($datalineLiteral, $line);
        }

        return $dataLines;
    }

    /**
     * @inerhitdoc
     */
    public function getOutput(): string
    {
        $options = $this->getOptions();
        $eventData = new ServerSentEventData(
            $this->getEventType(),
            $this->getDataLines(),
            $options['eventId'] ?? null,
            $options['retryDuration'] ?? Consts::DEFAULT_SSE_RETRY_DURATION,
        );

        foreach ($options as $key => $value) {
            $eventData->$key = $value;
        }

        $output = ['event: ' . $eventData->eventType->value];

        if ($eventData->eventId !== null) {
            $output[] = 'id: ' . $eventData->eventId;
        }

        if ($eventData->retryDuration !== Consts::DEFAULT_SSE_RETRY_DURATION) {
            $output[] = 'retry: ' . $eventData->retryDuration;
        }

        foreach ($eventData->data as $line) {
            $output[] = $line;
        }

        return implode("\n", $output) . "\n\n";
    }
}
