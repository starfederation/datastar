export const DEFAULT_SSE_SEND_RETRY = 1000;

export const sseHeaders = {
    "Cache-Control": "no-cache",
    "Connection": "keep-alive",
    "Content-Type": "text/event-stream"
};

export type ServerSentEventData = {
	type: EventType,
	id:  string,
	data:  string[],
	retry: number
};
