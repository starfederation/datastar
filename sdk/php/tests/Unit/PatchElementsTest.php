<?php

use starfederation\datastar\enums\ElementPatchMode;
use starfederation\datastar\events\PatchElements;

test('Mode can be passed in', function($mode) {
    $content = '<div>content</div>';
    $event = new PatchElements($content, [
        'mode' => $mode,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: mode append',
            'data: elements ' . $content,
        ]);
})->with([
    'enum' => ElementPatchMode::Append,
    'string' => ElementPatchMode::Append->value,
]);

test('Options are correctly output', function() {
    $content = '<div>content</div>';
    $event = new PatchElements($content, [
        'selector' => 'selector',
        'mode' => ElementPatchMode::Append,
        'useViewTransition' => true,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: selector selector',
            'data: mode append',
            'data: useViewTransition true',
            'data: elements ' . $content,
        ]);
});

test('Default options are not output', function() {
    $content = '<div>content</div>';
    $event = new PatchElements($content, [
        'selector' => '',
        'mode' => ElementPatchMode::Outer,
        'useViewTransition' => false,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: elements ' . $content,
        ]);
});

test('Multi-line content is correctly output', function() {
    $content = '<div>content</div>';
    $event = new PatchElements("\n" . $content . "\n" . $content . "\n");
    expect($event->getDataLines())
        ->toBe([
            'data: elements ' . $content,
            'data: elements ' . $content,
        ]);
});
