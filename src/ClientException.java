import java.io.IOException;


/**
 * Handles sending error messages back to Client when commands or parameters are not understood.
 */
class ClientException extends Exception {

    private MyBufferedWriter toClient;
    private ClientExceptionTypes clientExceptionType;

    /**
     * @param clientExceptionType the type of exception the client has triggered.
     * @param toClient the object for sending messages back to the client.
     */
    public ClientException(ClientExceptionTypes clientExceptionType, MyBufferedWriter toClient) {
        this.toClient = toClient;
        this.clientExceptionType = clientExceptionType;
    }

    /**
     * Sends the appropriate error message back to the Client based on the ClientExceptionType they triggered.
     */
    public void sendError() {
        try {
            toClient.writeLine("ERROR_CODE: " + clientExceptionType.getErrorCode());
            toClient.writeLine("ERROR_DESCRIPTION: " + clientExceptionType.getErrorMessage());
            toClient.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
