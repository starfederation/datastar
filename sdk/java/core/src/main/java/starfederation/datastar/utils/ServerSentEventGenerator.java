package starfederation.datastar.utils;

import starfederation.datastar.adapters.response.AbstractResponseAdapter;
import starfederation.datastar.events.AbstractDatastarEvent;

import java.io.Closeable;
import java.io.PrintWriter;
import java.util.concurrent.atomic.AtomicLong;

import static starfederation.datastar.Consts.DEFAULT_SSE_RETRY_DURATION;;

public class ServerSentEventGenerator implements Closeable {

    private final PrintWriter writer;
    private static final AtomicLong counter = new AtomicLong(-1);

    /**
     * Initializes the Server-Sent Event generator.
     *
     * @param response the response adapter to send the event to.
     */
    public ServerSentEventGenerator(AbstractResponseAdapter response) {
        if (response == null) {
            throw new IllegalArgumentException("Response adapter cannot be null.");
        }

        try {
            // Set headers for SSE
            response.setContentType("text/event-stream");
            response.setCharacterEncoding("UTF-8");
            response.setHeader("Cache-Control", "no-cache");
            response.setHeader("Connection", "keep-alive");

            this.writer = response.getWriter();
            this.writer.flush();
        } catch (Exception e) {
            throw new RuntimeException("Failed to initialize ServerSentEventGenerator.", e);
        }
    }

    /**
     * Sends a Datastar event to the client.
     *
     * @param event the event to send
     * @param id    the event id
     */
    public synchronized void send(AbstractDatastarEvent event, String id, int retry) {
        if (event == null) {
            throw new IllegalArgumentException("Event cannot be null.");
        }

        StringBuilder output = new StringBuilder();

        // Write the event type
        output.append("event: ").append(event.getEventType()).append("\n");
        output.append("id: ").append(id).append("\n");
        if(retry > 0) {
            output.append("retry: ").append(retry).append("\n");
        }
        // Write the data lines
        for (String line : event.getDataLines()) {
            output.append("data: ").append(line).append("\n");
        }

        // Add a blank line to separate events
        output.append("\n");

        // Send the event
        writer.print(output);
        writer.flush();
    }

    /**
     * Sends a Datastar event to the client with a monotonically increasing long ID
     * and default retry duration.
     *
     * @param event the event to send
     */
    public synchronized void send(AbstractDatastarEvent event) {
        send(event, String.valueOf(counter.incrementAndGet()), -1);
    }

    /**
     * Sends a Datastar event to the client with a specified ID and default retry
     * duration.
     *
     * @param event the event to send
     * @param id    the event id
     */
    public synchronized void send(AbstractDatastarEvent event, String id) {
        send(event, id, -1);
    }

    /**
     * Sends a Datastar event to the client with a monotonically increasing long ID
     * and specified retry duration.
     *
     * @param event the event to send
     * @param retry the retry duration
     */
    public synchronized void send(AbstractDatastarEvent event, int retry) {
        send(event, String.valueOf(counter.incrementAndGet()), retry);
    }

    /**
     * Closes the writer when finished.
     */
    @Override
    public synchronized void close() {
        writer.close();
    }
}
