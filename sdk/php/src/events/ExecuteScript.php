<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

use starfederation\datastar\Consts;
use starfederation\datastar\enums\ElementPatchMode;
use starfederation\datastar\enums\EventType;

class ExecuteScript implements EventInterface
{
    use EventTrait;

    public string $script;
    public bool $autoRemove = true;
    public array $attributes = [];

    public function __construct(string $script, array $options = [])
    {
        $this->script = $script;

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
        $dataLines = [];
        $dataLines[] = $this->getDataLine(Consts::SELECTOR_DATALINE_LITERAL, 'body');
        $dataLines[] = $this->getDataLine(Consts::MODE_DATALINE_LITERAL, ElementPatchMode::Append->value);

        $elements = '<script';

        foreach ($this->attributes as $key => $value) {
            $elements .= ' ' . $key . '="' . htmlspecialchars($value, ENT_QUOTES) . '"';
        }

        if ($this->autoRemove) {
            $elements .= ' ' . 'data-effect="el.remove()"';
        }

        $elements .= '>' . $this->script . '</script>';

        return array_merge(
            $dataLines,
            $this->getMultiDataLines(Consts::ELEMENTS_DATALINE_LITERAL, $elements),
        );
    }
}
