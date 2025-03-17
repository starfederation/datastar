<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

use starfederation\datastar\Consts;
use starfederation\datastar\enums\EventType;

class RemoveFragments implements EventInterface
{
    use EventTrait;

    public string $selector;
    public bool $useViewTransition = Consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS;

    public function __construct(string $selector, array $options = [])
    {
        $this->selector = $selector;

        foreach ($options as $key => $value) {
            $this->$key = $value;
        }
    }

    /**
     * @inerhitdoc
     */
    public function getEventType(): EventType
    {
        return EventType::RemoveFragments;
    }

    /**
     * @inerhitdoc
     */
    public function getDataLines(): array
    {
        $dataLines = [
            $this->getDataLine(Consts::SELECTOR_DATALINE_LITERAL, $this->selector),
        ];

        if ($this->useViewTransition !== Consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS) {
            $dataLines[] = $this->getDataLine(Consts::USE_VIEW_TRANSITION_DATALINE_LITERAL, $this->getBooleanAsString($this->useViewTransition));
        }

        return $dataLines;
    }
}
