package org.example.servlets;

import StarFederation.Datastar.ServerSentEventGenerator;
import StarFederation.Datastar.adapters.request.AbstractRequestAdapter;
import StarFederation.Datastar.adapters.request.HttpServletRequestAdapter;
import StarFederation.Datastar.adapters.response.AbstractResponseAdapter;
import StarFederation.Datastar.adapters.response.HttpServletResponseAdapter;
import StarFederation.Datastar.events.DataStore;
import StarFederation.Datastar.events.MergeFragments;
import StarFederation.Datastar.events.MergeFragmentsOptions;
import StarFederation.Datastar.events.SignalReader;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

public class HelloWorldServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        try {
            AbstractRequestAdapter requestAdapter = new HttpServletRequestAdapter(request);
            AbstractResponseAdapter responseAdapter = new HttpServletResponseAdapter(response);
            ServerSentEventGenerator sse = new ServerSentEventGenerator(responseAdapter);

            DataStore store = new DataStore();
            SignalReader.readSignals(requestAdapter, store.getStore());
            MergeFragmentsOptions options = MergeFragmentsOptions.create();

            String message = "Hello, World!";
            Number delayNumber = (Number) store.getStore().get("delay");

            for (int i = 0; i < message.length(); i++) {
                String htmlFragment = String.format("<div id=\"message\">%s</div>", message.substring(0, i + 1));
                MergeFragments event = new MergeFragments(htmlFragment, options);
                sse.MergeFragments(event);
                Thread.sleep(delayNumber.longValue());
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}