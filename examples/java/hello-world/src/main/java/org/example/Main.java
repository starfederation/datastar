package org.example;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.example.servlets.HelloWorldServlet;
import org.example.servlets.HtmlServlet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Main {

    private static final Logger logger = LoggerFactory.getLogger(Main.class);

    public static void main(String[] args) throws Exception {
        Server server = new Server(8080);
        ServletHandler handler = new ServletHandler();
        server.setHandler(handler);
        handler.addServletWithMapping(new ServletHolder(new HtmlServlet()), "/");
        handler.addServletWithMapping(new ServletHolder(new HelloWorldServlet()), "/hello-world");
        server.start();
        server.join();
        logger.info("Server started on http://localhost:8080");
    }
}
