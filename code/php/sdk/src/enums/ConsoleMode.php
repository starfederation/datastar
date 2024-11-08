<?php
/**
 * @copyright Copyright (c) PutYourLightsOn
 */

namespace starfederation\datastar\enums;

enum ConsoleMode: string
{
    case Assert = 'assert';
    case Clear = 'clear';
    case Count = 'count';
    case CountReset = 'countReset';
    case Debug = 'debug';
    case Dir = 'dir';
    case Dirxml = 'dirxml';
    case Error = 'error';
    case Group = 'group';
    case GroupCollapsed = 'groupCollapsed';
    case GroupEnd = 'groupEnd';
    case Info = 'info';
    case Log = 'log';
    case Table = 'table';
    case Time = 'time';
    case TimeEnd = 'timeEnd';
    case TimeLog = 'timeLog';
    case Trace = 'trace';
    case Warn = 'warn';
}
