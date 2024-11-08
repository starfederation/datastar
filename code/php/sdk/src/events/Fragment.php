<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

use starfederation\datastar\enums\EventType;
use starfederation\datastar\enums\FragmentMergeMode;

class Fragment implements EventInterface
{
    protected const DEFAULT_SETTLE_DURATION = 300;

    public string $data;
    public ?string $selector = null;
    public ?FragmentMergeMode $merge = null;
    public ?string $settleDuration = null;
    public ?bool $useViewTransition = null;

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
        return EventType::Fragment;
    }

    /**
     * @inerhitdoc
     */
    public function getDataLines(): array
    {
        $data = trim($this->data);
        $dataLines = [];

        if ($this->selector !== null) {
            $dataLines[] = 'data: selector ' . $this->selector;
        }

        $dataLines[] = 'data: merge ' . ($this->merge ?? FragmentMergeMode::Morph->value);

        if ($this->settleDuration !== null && $this->settleDuration !== static::DEFAULT_SETTLE_DURATION) {
            $dataLines[] = 'data: settle ' . $this->settleDuration;
        }

        if ($this->useViewTransition === true) {
            $dataLines[] = 'data: useViewTransition true';
        }

        $dataLines[] = 'data: fragment';

        $lines = explode("\n", $data);
        foreach ($lines as $line) {
            $dataLines[] = 'data: ' . $line;
        }

        return $dataLines;
    }
}
