<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\enums;

enum FragmentMergeMode: string
{
    case Morph = 'morph';
    case Inner = 'inner';
    case Outer = 'outer';
    case Prepend = 'prepend';
    case Append = 'append';
    case Before = 'before';
    case After = 'after';
    case Upsert = 'upsertAttributes';
}
