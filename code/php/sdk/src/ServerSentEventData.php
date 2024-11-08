<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar;

use starfederation\datastar\enums\EventType;

class ServerSentEventData
{
    public EventType $eventType;
    public ?string $id;
    public array $data;
    public ?int $retry;

    public function __construct(
        EventType $eventType,
        ?string $id = null,
        array $data,
        ?int $retry = null,
    ) {
        $this->eventType = $eventType;
        $this->id = $id;
        $this->data = $data;
        $this->retry = $retry;
    }
}
