<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

use starfederation\datastar\enums\EventType;

class Signal implements EventInterface
{
    public string $data;
    public ?bool $onlyIfMissing = null;

    public function __construct(string $data, array $options = [])
    {
        $this->data = $data;

        foreach ($options as $key => $value) {
            if (property_exists($this, $key)) {
                $this->$key = $value;
            }
        }
    }

    /**
     * @inerhitdoc
     */
    public function getEventType(): EventType
    {
        return EventType::Signal;
    }

    /**
     * @inerhitdoc
     */
    public function getDataLines(): array
    {
        $dataLines = [];

        if ($this->onlyIfMissing === true) {
            $dataLines[] = 'data: onlyIfMissing true';
        }

        $dataLines[] = 'data: store ' . $this->data;

        return $dataLines;
    }
}
