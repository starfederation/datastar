<?php

require_once __DIR__ . '/../vendor/autoload.php';

use starfederation\datastar\ServerSentEventGenerator;

$sse = new ServerSentEventGenerator();
$sse->sendHeaders();
$signals = ServerSentEventGenerator::readSignals();
$delay = ($signals['delay'] ?? 0) * 1000;
$message = 'Hello, world!';

for ($i = 0; $i < strlen($message); $i++) {
    $sse->mergeFragments('<div id="message">'
        . substr($message, 0, $i + 1)
        . '</div>'
    );
    usleep($delay);
}
