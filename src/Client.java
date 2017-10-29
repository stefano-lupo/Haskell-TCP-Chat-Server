import java.io.*;
import java.net.Socket;
import java.util.HashMap;


public class Client implements Runnable {

    enum Command {
        BLANK("BLANK",0),
        HELO("HELO", 0),
        KILL_SERVICE("KILL_SERVICE", 0),
        JOIN_CHATROOM("JOIN_CHATROOM", 4),
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

    enum CommandParams {
        CLIENT_NAME,
        JOIN_ID,
        MESSAGE
    }


    private boolean running = false;
    private Server server;
    private Socket socket;
    private BufferedReader fromClient;
    private MyBufferedWriter toClient;
    private int clientId;
    private HashMap<Integer, String> clientsNamesInChatrooms;



    Client(Server server, Socket socket, int clientId) {
        this.running = true;
        this.server = server;
        this.socket = socket;
        this.clientId = clientId;
        this.clientsNamesInChatrooms = new HashMap<>();

        try {
            fromClient = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            toClient = new MyBufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }



    public void sendMessageToClient(String message) {
        // TODO: maybe should throw exception to inform Chatroom to drop client
        try {
            toClient.writeNow(message);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void sendMessageToClient(String[] messages) {
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
        try {
            while(running) {
                String firstLine = fromClient.readLine();
                handleCommand(firstLine);
            }

        } catch (IOException e) {
            System.out.println(clientId + " interupted, I guess we're shutting down the server now..");
        }
    }

    public void safelyTerminate() {
        this.running = false;
        System.out.println("Shutting down " + this.clientId);
        try {
            socket.close();
            toClient.close();
            fromClient.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Parse Command sent from client and hand off to appropriate handler
    private void handleCommand(String firstLine) {
        try {
            String[] parts = getCommandAndParams(firstLine);
            String command = parts[0];
            String params = parts[1];
            if (command.equals(Command.HELO.command)) {
                respondToHelo(params);
            } else if(command.equals(Command.JOIN_CHATROOM.command)) {
                joinChatRoom(params);
            } else if (command.equals(Command.LEAVE.command)) {
                leaveChatRoom(params);
            } else if (command.equals(Command.DISCONNECT.command)) {
                disconnect(params);
            } else if (command.equals(Command.CHAT.command)) {
                chat(params);
            } else if (command.equals(Command.KILL_SERVICE.command)) {
                server.killService();
            } else if(command.equals(Command.BLANK.command)) {
                System.out.println("Client " + this.clientId + ": Blank read from socket.. skipping");
            } else {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            }
        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        }
    }


    //TODO Enfornce cant send message to wrong chatroom CHECK THIS

    private void respondToHelo(String text) {
        String[] messages = {
            "HELO " + text,
            "IP:" + server.getIP(),
            "Port:" + server.getPort(),
            "StudentID:14334933"
        };

        sendMessageToClient(messages);
    }

    // Handle a client attempting to join a chatroom
    private void joinChatRoom(String chatRoomName) {
        try {

            // Throw away CLIENT_IP, PORT Fields
            for (int i = 1; i < Command.JOIN_CHATROOM.numLines() - 1; i++) {
                fromClient.readLine();
            }

            String[] parts = getCommandAndParams(fromClient.readLine());
            if(parts[0].equals(CommandParams.CLIENT_NAME.name())) {
                String clientNameInThisChatRoom = parts[1];
                server.joinChatRoom(this, clientNameInThisChatRoom, chatRoomName);
            } else {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            }
        } catch (ClientException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Handles a client attempting to leave a chatroom
    private void leaveChatRoom(String roomRef) {
        try {
            int roomReferenceNumber = Integer.valueOf(roomRef);

            String line = fromClient.readLine();
            String[] parts = getCommandAndParams(line);

            int joinId;
            if(parts[0].equals(CommandParams.JOIN_ID.name())) {
                joinId = Integer.valueOf(parts[1]);
            } else {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            }

            line = fromClient.readLine();
            parts = getCommandAndParams(line);

            String clientName;
            if(parts[0].equals(CommandParams.CLIENT_NAME.name())) {
                clientName = parts[1];
            } else {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            }


            ChatRoom chatRoom = server.getChatRoomById(roomReferenceNumber);
            if(chatRoom == null) {
                throw new ClientException(ClientExceptionTypes.NO_CHATROOM_FOUND, toClient);
            }

            chatRoom.unsubscribeFromChatRoom(this);

        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Handles a client attempting to full disconnect from the server
    private void disconnect(String ip) {

        try {
            // Throw away Port
            fromClient.readLine();

            // Get clients name
            String line = fromClient.readLine();
            String[] parts = getCommandAndParams(line);
            String clientName;
            if(parts[0].equals(CommandParams.CLIENT_NAME.name())) {
                clientName = parts[1];
            } else {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            }

            server.terminateClientConnection(this);
            safelyTerminate();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        }

    }

    // Handles a client attempting to send a message to a chatroom
    private void chat(String roomRef) {
        try {
            int chatRoomId;
            chatRoomId = Integer.valueOf(roomRef);

            String line = fromClient.readLine();

            int joinId;
            String[] parts = getCommandAndParams(line);
            if(!parts[0].equals(CommandParams.JOIN_ID.name())) {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            } else {
                joinId = Integer.valueOf(parts[1]);
            }


            String clientName;
            line = fromClient.readLine();
            parts = getCommandAndParams(line);
            if(!parts[0].equals(CommandParams.CLIENT_NAME.name())) {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            } else {
                clientName = parts[1];
            }


            String message;
            line = fromClient.readLine();
            parts = getCommandAndParams(line);
            if(!parts[0].equals(CommandParams.MESSAGE.name())) {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            } else {
                message = parts[1];
            }

            ChatRoom chatRoom = server.getChatRoomById(chatRoomId);
            if(chatRoom == null) {
                throw new ClientException(ClientExceptionTypes.NO_CHATROOM_FOUND, toClient);
            }

            chatRoom.chat(this, message);


        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        }


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
        System.out.println(this.clientId + " Checking: " + line);
        String[] parts = line.split(": ");
        if(parts.length == 1) {
            if(parts[0].contains(Command.HELO.command)) {
                String[] parts2 = {Command.HELO.command, parts[0].substring(5)};
                return parts2;
            } else if(parts[0].contains(Command.KILL_SERVICE.command)) {
                String[] parts2 = {Command.KILL_SERVICE.command, null};
                return parts2;
            }
            else {
                if(line.equals("") || line == null) {
                    String[] parts2 = {
                        "BLANK",
                        "BLANK"
                    };
                    return parts2;
                }

                System.out.println("Invalid : " + line);
                throw new ClientException(ClientExceptionTypes.INVALID_SYNTAX, toClient);
            }
        }

        parts[0] = parts[0].toUpperCase();
        System.out.println("Valid command: " + parts[0]);
        return parts;
    }

    /*
        Instance Getter and Setters
     */
    public int getClientId() {
        return this.clientId;
    }

    public String getNameInChatroom(int chatRoomId) {
        return this.clientsNamesInChatrooms.get(chatRoomId);
    }

    public void setNameInChatroom(int chatRoomId, String name) {
        clientsNamesInChatrooms.put(chatRoomId, name);
    }

}
