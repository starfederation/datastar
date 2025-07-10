<?php

require_once __DIR__ . '/../vendor/autoload.php';

use starfederation\datastar\ServerSentEventGenerator;

$signals = ServerSentEventGenerator::readSignals();
$delay = $signals['delay'] ?? 0;
$message = 'Hello, world!';

$sse = new ServerSentEventGenerator();

for ($i = 0; $i < strlen($message); $i++) {
    $sse->patchElements('<div id="message">'
        . substr($message, 0, $i + 1)
        . '</div>'
    );

    // Sleep for the provided delay in milliseconds.
    usleep($delay * 1000);
}
