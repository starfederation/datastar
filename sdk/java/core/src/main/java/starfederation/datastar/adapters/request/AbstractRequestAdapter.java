package starfederation.datastar.adapters.request;

import java.io.BufferedReader;
import java.io.IOException;

public abstract class AbstractRequestAdapter implements RequestAdapter {
    protected BufferedReader reader;

    @Override
    public BufferedReader getReader() throws IOException {
        return reader;
    }
}
