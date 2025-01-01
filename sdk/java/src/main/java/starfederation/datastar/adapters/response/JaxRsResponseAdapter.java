package starfederation.datastar.adapters.response;

import jakarta.ws.rs.sse.Sse;
import jakarta.ws.rs.sse.SseEventSink;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

public class JaxRsResponseAdapter extends AbstractResponseAdapter {
    private final SseEventSink eventSink;
    private final Sse sse;

    public JaxRsResponseAdapter(SseEventSink eventSink, Sse sse) {
        this.eventSink = eventSink;
        this.sse = sse;
        StringWriter stringWriter = new StringWriter();
        this.writer = new PrintWriter(stringWriter, true);
    }

    @Override
    public void setHeader(String name, String value) {
    }

    @Override
    public void setStatus(int status) {
    }

    public void sendEvent(String eventName, String data) throws IOException {
        if (eventSink != null && !eventSink.isClosed()) {
            eventSink.send(sse.newEventBuilder()
                    .name(eventName)
                    .data(data)
                    .build());
        }
    }
}
