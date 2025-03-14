package org.example.servlets;

import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import starfederation.datastar.adapters.request.AbstractRequestAdapter;
import starfederation.datastar.adapters.request.HttpServletRequestAdapter;
import starfederation.datastar.adapters.response.HttpServletResponseAdapter;
import starfederation.datastar.events.MergeFragments;
import starfederation.datastar.utils.DataStore;
import starfederation.datastar.utils.ServerSentEventGenerator;
import starfederation.datastar.utils.SignalReader;

import java.util.concurrent.TimeUnit;

public class HelloWorldServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        try (ServerSentEventGenerator sse = new ServerSentEventGenerator(new HttpServletResponseAdapter(response))) {
            AbstractRequestAdapter requestAdapter = new HttpServletRequestAdapter(request);

            DataStore store = new DataStore();
            SignalReader.readSignals(requestAdapter, store.getStore());

            String message = "Hello, World!";
            Number delayNumber = (Number) store.getStore().get("delay");

            for (int i = 0; i < message.length(); i++) {
                String htmlFragment = String.format("<div id=\"message\">%s</div>", message.substring(0, i + 1));
                MergeFragments event = MergeFragments.builder()
                        .selector("#message")
                        .useViewTransition(true)
                        .data(htmlFragment)
                        .build();

                sse.send(event);
                try {
                    TimeUnit.MILLISECONDS.sleep(delayNumber.longValue());
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}