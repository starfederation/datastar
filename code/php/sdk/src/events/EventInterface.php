<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

use starfederation\datastar\enums\EventType;

interface EventInterface
{
    /**
     * Returns the event type for this event.
     */
    public function getEventType(): EventType;

    /**
     * Returns the data lines for this event.
     *
     * @return string[]
     */
    public function getDataLines(): array;
}
