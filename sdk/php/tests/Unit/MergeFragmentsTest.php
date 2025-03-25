<?php

use starfederation\datastar\Consts;
use starfederation\datastar\enums\FragmentMergeMode;
use starfederation\datastar\events\MergeFragments;

test('Merge mode can be passed in', function($value) {
    $content = '<div>content</div>';
    $event = new MergeFragments($content, [
        'mergeMode' => FragmentMergeMode::Append,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: mergeMode append',
            'data: fragments ' . $content,
        ]);
})->with([
    'enum' => FragmentMergeMode::Append,
    'string' => FragmentMergeMode::Append->value,
]);

test('Options are correctly output', function() {
    $content = '<div>content</div>';
    $event = new MergeFragments($content, [
        'selector' => 'selector',
        'mergeMode' => FragmentMergeMode::Append,
        'useViewTransition' => true,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: selector selector',
            'data: mergeMode append',
            'data: useViewTransition true',
            'data: fragments ' . $content,
        ]);
});

test('Default options are not output', function() {
    $content = '<div>content</div>';
    $event = new MergeFragments($content, [
        'selector' => '',
        'mergeMode' => FragmentMergeMode::Morph,
        'useViewTransition' => false,
    ]);
    expect($event->getDataLines())
        ->toBe([
            'data: fragments ' . $content,
        ]);
});

test('Multi-line content is correctly output', function() {
    $content = '<div>content</div>';
    $event = new MergeFragments("\n" . $content . "\n" . $content . "\n");
    expect($event->getDataLines())
        ->toBe([
            'data: fragments ' . $content,
            'data: fragments ' . $content,
        ]);
});
