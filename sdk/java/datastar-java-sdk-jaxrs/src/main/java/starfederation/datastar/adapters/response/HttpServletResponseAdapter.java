package starfederation.datastar.adapters.response;

import jakarta.servlet.http.HttpServletResponse;

public class HttpServletResponseAdapter extends AbstractResponseAdapter {

    private final HttpServletResponse response;

    public HttpServletResponseAdapter(HttpServletResponse response) throws Exception {
        this.response = response;
        this.writer = response.getWriter();
    }

    @Override
    public void setHeader(String name, String value) {
        response.setHeader(name, value);
    }

    @Override
    public void setStatus(int status) {
        response.setStatus(status);
    }
}
