<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\enums;

enum EventType: string
{
    case Fragment = 'datastar-fragment';
    case Signal = 'datastar-signal';
    case Remove = 'datastar-remove';
    case Redirect = 'datastar-redirect';
    case Console = 'datastar-console';
}
