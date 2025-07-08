package org.example.servlets;

import jakarta.servlet.AsyncContext;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import starfederation.datastar.adapters.request.AbstractRequestAdapter;
import starfederation.datastar.adapters.request.HttpServletRequestAdapter;
import starfederation.datastar.adapters.response.AbstractResponseAdapter;
import starfederation.datastar.adapters.response.HttpServletResponseAdapter;
import starfederation.datastar.events.PatchElements;
import starfederation.datastar.utils.DataStore;
import starfederation.datastar.utils.ServerSentEventGenerator;
import starfederation.datastar.utils.SignalReader;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * HelloWorldServlet loads and serves the Hello World animation.
 */
public class HelloWorldServlet extends HttpServlet {
    private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();

    @Override
    public void destroy() {
        scheduler.shutdownNow();
        super.destroy();
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        final AsyncContext asyncContext = request.startAsync();
        asyncContext.setTimeout(0);

        try {
            AbstractRequestAdapter requestAdapter = new HttpServletRequestAdapter(request);
            AbstractResponseAdapter responseAdapter = new HttpServletResponseAdapter(response);
            ServerSentEventGenerator sse = new ServerSentEventGenerator(responseAdapter);

            DataStore store = new DataStore();
            SignalReader.readSignals(requestAdapter, store.getStore());

            final String message = "Hello, World!";
            final Number delayNumber = (Number) store.getStore().get("delay");
            final long delayMs = delayNumber.longValue();

            scheduleMessageAnimation(message, delayMs, sse, asyncContext, 0);

        } catch (Exception e) {
            asyncContext.complete();
            throw new RuntimeException(e);
        }
    }

    private void scheduleMessageAnimation(String message, long delayMs, ServerSentEventGenerator sse, AsyncContext asyncContext, int currentIndex) {
        if (currentIndex >= message.length()) {
            sse.close();
            asyncContext.complete();
            return;
        }

        scheduler.schedule(() -> {
            try {
                String htmlFragment = String.format("<div id=\"message\">%s</div>",
                        message.substring(0, currentIndex + 1));

                PatchElements event = PatchElements.builder()
                        .selector("#message")
                        .data(htmlFragment)
                        .build();

                sse.send(event);

                scheduleMessageAnimation(message, delayMs, sse, asyncContext, currentIndex + 1);

            } catch (Exception e) {
                asyncContext.complete();
            }
        }, delayMs, TimeUnit.MILLISECONDS);
    }
}