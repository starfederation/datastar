<?php

use starfederation\datastar\Consts;
use starfederation\datastar\events\RemoveFragments;

test('Options are correctly output', function() {
    $content = 'body';
    $event = new RemoveFragments($content, [
        'useViewTransition' => true,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: selector body',
            'data: useViewTransition true',
        ]);
});

test('Default options are not output', function() {
    $content = 'body';
    $event = new RemoveFragments($content, [
        'useViewTransition' => false,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: selector body',
        ]);
});
