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
     * Returns the options for this event.
     *
     * @return string[]|int[]
     */
    public function getOptions(): array;

    /**
     * Returns the data lines for this event.
     *
     * @return string[]
     */
    public function getDataLines(): array;

    /**
     * Returns a boolean as a string.
     */
    public function getBooleanAsString(bool $value): string;

    /**
     * Returns a data line.
     */
    public function getDataLine(string $value): string;

    /**
     * Returns multiple data lines.
     */
    public function getMultiDataLines(string $literal, string $data): array;

    /**
     * Returns the final event output.
     */
    public function getOutput(): string;
}
