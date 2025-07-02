<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

use starfederation\datastar\Consts;
use starfederation\datastar\enums\ElementPatchMode;
use starfederation\datastar\enums\EventType;

class RemoveElements implements EventInterface
{
    use EventTrait;

    public string $selector;
    public bool $useViewTransition = Consts::DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS;

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
        return EventType::PatchElements;
    }

    /**
     * @inerhitdoc
     */
    public function getDataLines(): array
    {
        $dataLines = [
            $this->getDataLine(Consts::SELECTOR_DATALINE_LITERAL, $this->selector),
            $this->getDataLine(Consts::MODE_DATALINE_LITERAL, ElementPatchMode::Remove->value),
        ];

        if ($this->useViewTransition !== Consts::DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS) {
            $dataLines[] = $this->getDataLine(Consts::USE_VIEW_TRANSITION_DATALINE_LITERAL, $this->getBooleanAsString($this->useViewTransition));
        }

        return $dataLines;
    }
}
