import { serve } from "https://deno.land/std@0.140.0/http/server.ts";
import { ServerSentEventGenerator } from "./serverSentEventGenerator.ts";

serve((req) => {
    const url = new URL(req.url);

    switch (url.pathname) {
        case "/":
            return new Response(`<html><head><script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js"></script></head><body><div id="toMerge" data-on-load="sse('/merge', {method: 'get'})">Not merged</div></body></html>`, {
                headers: { 'Content-Type': 'text/html' }
            });
        case "/merge":
            return ServerSentEventGenerator.stream((stream) => {
            stream.mergeFragments('<div id="toMerge">Merged</div>');
        });
    }
});
