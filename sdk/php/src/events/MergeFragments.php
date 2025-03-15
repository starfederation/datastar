<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

use Exception;
use starfederation\datastar\Consts;
use starfederation\datastar\enums\EventType;
use starfederation\datastar\enums\FragmentMergeMode;

class MergeFragments implements EventInterface
{
    use EventTrait;

    public string $fragments;
    public string $selector = '';
    public FragmentMergeMode $mergeMode = Consts::DEFAULT_FRAGMENT_MERGE_MODE;
    public bool $useViewTransition = Consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS;

    public function __construct(string $fragments, array $options = [])
    {
        $this->fragments = $fragments;

        foreach ($options as $key => $value) {
            if ($key === 'mergeMode') {
                $value = $this->getMergeMode($value);
            }

            $this->$key = $value;
        }
    }

    /**
     * @inerhitdoc
     */
    public function getEventType(): EventType
    {
        return EventType::MergeFragments;
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

        if ($this->mergeMode !== Consts::DEFAULT_FRAGMENT_MERGE_MODE) {
            $dataLines[] = $this->getDataLine(Consts::MERGE_MODE_DATALINE_LITERAL, $this->mergeMode->value);
        }

        if ($this->useViewTransition !== Consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS) {
            $dataLines[] = $this->getDataLine(Consts::USE_VIEW_TRANSITION_DATALINE_LITERAL, $this->getBooleanAsString($this->useViewTransition));
        }

        return array_merge(
            $dataLines,
            $this->getMultiDataLines(Consts::FRAGMENTS_DATALINE_LITERAL, $this->fragments),
        );
    }

    private function getMergeMode(FragmentMergeMode|string $value): FragmentMergeMode
    {
        $value = is_string($value) ? FragmentMergeMode::tryFrom($value) : $value;

        if ($value === null) {
            $enumValues = array_map(fn($case) => '`' . $case->value . '`', FragmentMergeMode::cases());

            throw new Exception('An invalid value was passed into `mergeMode`. The value must be one of: ' . implode(', ', $enumValues) . '.');
        }

        return $value;
    }
}
