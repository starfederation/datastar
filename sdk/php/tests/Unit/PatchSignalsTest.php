<?php

use starfederation\datastar\events\PatchSignals;

test('Options are correctly output', function() {
    $content = '{x: 1}';
    $event = new PatchSignals($content, [
        'onlyIfMissing' => true,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: onlyIfMissing true',
            'data: signals {x: 1}',
        ]);
});

test('Default options are not output', function() {
    $content = '{x: 1}';
    $event = new PatchSignals($content, [
        'onlyIfMissing' => false,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: signals {x: 1}',
        ]);
});

test('Multi-line content is correctly output', function() {
    $content = '{x: 1}';
    $event = new PatchSignals("\n" . $content . "\n" . $content . "\n");
    expect($event->getDataLines())
        ->toBe([
            'data: signals {x: 1}',
            'data: signals {x: 1}',
        ]);
});

test('Signals can be passed in as an array', function() {
    $signals = ['x' => 1];
    $event = new PatchSignals($signals);
    expect($event->getDataLines())
        ->toBe([
            'data: signals ' . json_encode($signals),
        ]);
});
