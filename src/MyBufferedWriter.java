import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;

public class MyBufferedWriter extends BufferedWriter{
    public MyBufferedWriter(OutputStreamWriter osw) {
        super(osw);
    }

    public void writeLine(String line) throws IOException {
        this.write(line);
        this.newLine();
    }

    public void writeLine(String line, int numNewLines) throws IOException {
        this.write(line);
        for(int i=0; i<numNewLines; i++) {
            this.newLine();
        }
    }

    public void writeNow(String line) throws IOException {
        this.write(line);
        this.newLine();
        this.flush();
    }

    public void writeNow(String line, int numNewLines) throws IOException {
        this.write(line);
        for(int i=0; i<numNewLines; i++) {
            this.newLine();
        }
        this.flush();
    }
}
