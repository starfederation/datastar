export DEFAULT_SETTLE_TIME = 300;

export const fragmentMergeModes = [
    "morph" 
    , "inner" 
    , "outer" 
    , "prepend" 
    , "append" 
    , "before" 
    , "after" 
];

export type FragmentMergeMode = typeof fragmentMergeModes[number];

export type RenderFragmentOptions = {
    selector: string,
    mergeMode: FragmentMergeMode,
    settleDuration: number,
    useViewTransitions: boolean
};
