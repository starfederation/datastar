<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

use Exception;
use starfederation\datastar\Consts;
use starfederation\datastar\enums\ElementPatchMode;
use starfederation\datastar\enums\EventType;

class PatchElements implements EventInterface
{
    use EventTrait;

    public string $elements;
    public string $selector = '';
    public ElementPatchMode $mode = Consts::DEFAULT_ELEMENT_PATCH_MODE;
    public bool $useViewTransition = Consts::DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS;

    public function __construct(string $elements, array $options = [])
    {
        $this->elements = $elements;

        foreach ($options as $key => $value) {
            if ($key === 'mode') {
                $value = $this->getMode($value);
            }

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
        $dataLines = [];

        if (!empty($this->selector)) {
            $dataLines[] = $this->getDataLine(Consts::SELECTOR_DATALINE_LITERAL, $this->selector);
        }

        if ($this->mode !== Consts::DEFAULT_ELEMENT_PATCH_MODE) {
            $dataLines[] = $this->getDataLine(Consts::MODE_DATALINE_LITERAL, $this->mode->value);
        }

        if ($this->useViewTransition !== Consts::DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS) {
            $dataLines[] = $this->getDataLine(Consts::USE_VIEW_TRANSITION_DATALINE_LITERAL, $this->getBooleanAsString($this->useViewTransition));
        }

        return array_merge(
            $dataLines,
            $this->getMultiDataLines(Consts::ELEMENTS_DATALINE_LITERAL, $this->elements),
        );
    }

    private function getMode(ElementPatchMode|string $value): ElementPatchMode
    {
        $value = is_string($value) ? ElementPatchMode::tryFrom($value) : $value;

        if ($value === null) {
            $enumValues = array_map(fn($case) => '`' . $case->value . '`', ElementPatchMode::cases());

            throw new Exception('An invalid value was passed into `mode`. The value must be one of: ' . implode(', ', $enumValues) . '.');
        }

        return $value;
    }
}
