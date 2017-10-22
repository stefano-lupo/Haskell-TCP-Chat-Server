import java.io.*;
import java.net.Socket;
import java.util.Arrays;


public class Client implements Runnable {

    enum Command {
        JOIN("JOIN", 4),
        LEAVE("LEAVE_CHATROOM", 3),
        DISCONNECT("DISCONNECT", 3),
        CHAT("CHAT", 4);

        private String command;
        private int numLines;

        Command(String command, int numLines) {
            this.command = command;
            this.numLines = numLines;
        }

        public String command() {
            return command;
        }

        public int numLines () {
            return numLines;
        }
    }

    enum ComandParams {
        CLIENT_NAME,
        JOIN_ID,
        MESSAGE
    }



    private Server server;
    private Socket socket;
    private BufferedReader fromClient;
    private MyBufferedWriter toClient;

    private String clientName;



    Client(Server server, Socket socket) {
        this.server = server;
        this.socket = socket;

        try {
            fromClient = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            toClient = new MyBufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void sendMessage(String message) {
        // TODO: maybe should throw exception to inform Chatroom to drop client
        try {
            toClient.writeNow(message);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void sendMessage(String[] messages) {
        // TODO: maybe should throw exception to inform Chatroom to drop client
        try {
            for(String message : messages) {
                toClient.writeLine(message);
            }
            toClient.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void run() {
        System.out.println("Starting to listen");
        try {
            while(true) {
                String firstLine = fromClient.readLine();
                handleCommand(firstLine);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void handleCommand(String firstLine) {
        try {
            String[] parts = getCommandAndParams(firstLine);
            String command = parts[0];
            String params = parts[1];

            if (command.equals(Command.JOIN.command)) {
                joinChatRoom(params);
            } else if (command.equals(Command.LEAVE.command)) {
                leaveChatRoom(params);
            } else if (command.equals(Command.DISCONNECT.command)) {
                disconnectFromChatRoom(params);
            } else if (command.equals(Command.CHAT.command)) {
                chat(params);
            } else {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            }
        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        }
    }

    private void joinChatRoom(String chatRoomName) {
        try {
            // Throw away CLIENT_IP, PORT Fields
            for (int i = 1; i < Command.JOIN.numLines() - 1; i++) {
                System.out.println("Throwing away: " + fromClient.readLine());
            }

            String[] parts = getCommandAndParams(fromClient.readLine());

            if(parts[0].equals(ComandParams.CLIENT_NAME.name())) {
                this.clientName = parts[1];
                server.joinChatRoom(this, chatRoomName);
            } else {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            }
        } catch (ClientException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void leaveChatRoom(String params) {

    }

    private void disconnectFromChatRoom(String params) {

    }

    private void chat(String params) {
        int chatRoomId = Integer.valueOf(params);


    }


    //Todo Should throw a proper exception for each of these to catch invalid syntax in one
    private String getCommand(String line) throws ClientException{
        String[] parts = getCommandAndParams(line);
        if(parts.length == 1) {
            throw new ClientException(ClientExceptionTypes.INVALID_SYNTAX, toClient);
        }

        return parts[0];
    }

    private String getParams(String line) throws ClientException{
        String[] parts = getCommandAndParams(line);
        if(parts.length == 1) {
            throw new ClientException(ClientExceptionTypes.INVALID_SYNTAX, toClient);
        }

        return parts[1];
    }

    private String[] getCommandAndParams(String line) throws ClientException {
        String[] parts = line.toUpperCase().split(": ");
        if(parts.length == 1) {
            throw new ClientException(ClientExceptionTypes.INVALID_SYNTAX, toClient);
        }

        return parts;
    }

    public String getClientName() {
        return clientName;
    }
}

/*

JOIN_CHATROOM: Default
CLIENT_IP: 0
PORT: 0
CLIENT_NAME: Stefano

*/
