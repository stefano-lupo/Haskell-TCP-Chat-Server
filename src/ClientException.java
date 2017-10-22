import java.io.IOException;

class ClientException extends Exception {

    private MyBufferedWriter toClient;
    private ClientExceptionTypes clientExceptionType;

    public ClientException(ClientExceptionTypes clientExceptionType, MyBufferedWriter toClient) {
        this.toClient = toClient;
        this.clientExceptionType = clientExceptionType;
    }

    public void sendError() {
        System.out.println("Error occured, notifying client");
        try {
            toClient.writeLine("ERROR_CODE: " + clientExceptionType.getErrorCode());
            toClient.writeLine("ERROR_DESCRIPTION: " + clientExceptionType.getErrorMessage());
            toClient.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
