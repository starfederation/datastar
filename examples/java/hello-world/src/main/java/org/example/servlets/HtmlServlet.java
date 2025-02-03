package org.example.servlets;


import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.*;

/**
 * HtmlServlet loads and serves the HTML file from the resources directory.
 */
public class HtmlServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        // Load the HTML file from the resources directory
        // The file "hello-world.html" should be placed in src/main/resources/
        InputStream inputStream = getClass().getResourceAsStream("/hello-world.html");
        if (inputStream == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Resource not found: hello-world.html");
            return;
        }

        // Set the content type to HTML
        resp.setContentType("text/html;charset=UTF-8");

        // Write the content of the HTML file to the response
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
             PrintWriter writer = resp.getWriter()) {
            String line;
            while ((line = reader.readLine()) != null) {
                writer.println(line);
            }
        }
    }
}
