const eventTypes = [
    "datastar-fragment" 
    , "datastar-signal" 
    , "datastar-remove" 
    , "datastar-redirect" 
    , "datastar-console" 
];

export type EventType = typeof eventTypes[number];
