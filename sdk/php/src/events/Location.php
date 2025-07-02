<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\events;

class Location extends ExecuteScript
{
    public function __construct(string $uri, array $options = [])
    {
        $this->script = "setTimeout(() => window.location = '$uri')";

        foreach ($options as $key => $value) {
            $this->$key = $value;
        }
    }
}
