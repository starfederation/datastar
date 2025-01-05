package starfederation.datastar.adapters.response;

import java.io.PrintWriter;

public abstract class AbstractResponseAdapter implements ResponseAdapter {

    protected PrintWriter writer;

    @Override
    public PrintWriter getWriter() throws Exception {
        return writer;
    }

    @Override
    public void setHeader(String name, String value) {
        throw new UnsupportedOperationException("setHeader is not implemented for this response type.");
    }

    @Override
    public void setContentType(String contentType) {
        setHeader("Content-Type", contentType);
    }

    @Override
    public void setCharacterEncoding(String encoding) {
        setHeader("Content-Encoding", encoding);
    }

    @Override
    public void setStatus(int status) {
        throw new UnsupportedOperationException("setStatus is not implemented for this response type.");
    }
}
