package org.example;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.ContextHandler;
import org.eclipse.jetty.server.handler.ResourceHandler;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.example.servlets.HelloWorldServlet;
import org.example.servlets.HtmlServlet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Main {

    private static final Logger logger = LoggerFactory.getLogger(Main.class);

    public static void main(String[] args) throws Exception {
        // Create the server
        Server server = new Server(8080);

        // Create a ServletContextHandler
        ServletContextHandler context = new ServletContextHandler();
        context.setContextPath("/");

        // Add servlets to the context
        context.addServlet(new ServletHolder(new HtmlServlet()), "/");
        context.addServlet(new ServletHolder(new HelloWorldServlet()), "/hello-world");

        // Set the context as the server's handler
        server.setHandler(context);

        // Start the server
        server.start();
        logger.info("Server started on http://localhost:8080");

        // Keep the main thread alive until the server is stopped
        server.join();
    }
}