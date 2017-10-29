import java.io.*;
import java.net.Socket;
import java.util.HashMap;


public class Client implements Runnable {

    /**
     * Defines all of the commands expected by the server and the total number of lines
     * that the command expects.
     */
    enum Command {
        HELO("HELO", 0),
        KILL_SERVICE("KILL_SERVICE", 0),
        JOIN_CHATROOM("JOIN_CHATROOM", 4),
        LEAVE("LEAVE_CHATROOM", 3),
        DISCONNECT("DISCONNECT", 3),
        CHAT("CHAT", 4);

        private String command;
        private int numParamLines;

        Command(String command, int numParamLines) {
            this.command = command;
            this.numParamLines = numParamLines;
        }
    }

    /**
     * Defines the allowed command parameters
     */
    enum CommandParams {
        CLIENT_NAME,
        JOIN_ID,
        MESSAGE,
        PORT
    }


    private boolean running = false;
    private Server server;
    private Socket socket;
    private BufferedReader fromClient;
    private MyBufferedWriter toClient;
    private int clientId;
    private HashMap<Integer, String> clientsNamesInChatrooms;


    /**
     * Handles all interaction between the Client and the Server.
     * @param server the Server instance currently running
     * @param socket the created Socket connection between the Client and Server
     * @param clientId the Id of the Client (specified by Server)
     */
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


    /**
     * Polls the socket for data from the Client
     */
    @Override
    public void run() {
        try {
            while(running) {
                String firstLine = fromClient.readLine();
                if(firstLine != null) {
                    handleCommand(firstLine);
                }

            }

        }

        // Closing the socket connection while readLine() is blocking throws an IOException.
        // This happens when another client issues the "KILL_SERVICE" command and is thus acceptable.
        catch (IOException e) {
            System.out.println(clientId + " interrupted, the server must be shutting down.");
        }
    }


    /**
     * Parses the data sent from the client and runs the appropriate callback method.
     */
    private void handleCommand(String firstLine) {
        try {
            String[] parts = getCommandAndParams(firstLine);
            String command = parts[0];
            String params = parts[1];
            if (command.equals(Command.HELO.command)) {
                respondToHelo(params);
            } else if(command.equals(Command.JOIN_CHATROOM.command)) {
                joinChatRoom(params);
            } else if (command.equals(Command.CHAT.command)) {
                chat(params);
            } else if (command.equals(Command.LEAVE.command)) {
                leaveChatRoom(params);
            } else if (command.equals(Command.DISCONNECT.command)) {
                disconnect();
            } else if (command.equals(Command.KILL_SERVICE.command)) {
                killService();
            } else {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            }
        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        }
    }

    /**
     * Responds to the HELO command
     * @param text the parameter text sent with the HELO command
     */
    private void respondToHelo(String text) {
        String[] messages = {
            "HELO " + text,
            "IP:" + server.getIP(),
            "Port:" + server.getPort(),
            "StudentID:14334933"
        };

        sendMessageToClient(messages);
    }

    /**
     * Handles the client request to join the ChatRoom with the specified name.
     * @param chatRoomName name of the ChatRoom the Client wishes to join
     */
    private void joinChatRoom(String chatRoomName) {
        try {

            // Throw away CLIENT_IP, PORT Fields
            for (int i = 1; i < Command.JOIN_CHATROOM.numParamLines - 1; i++) {
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

    /**
     * Handles the Client request to leave a ChatRoom
     * @param roomId the Id of the ChatRoom the Client wishes to leave.
     */
    private void leaveChatRoom(String roomId) {
        try {
            int roomReferenceNumber = Integer.valueOf(roomId);

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

            chatRoom.leaveChatRoom(this);

        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Handles the Client request to disconnect from the server
     */
    private void disconnect() {

        try {
            // Throw away Port
            String line = fromClient.readLine();
            String[] parts = getCommandAndParams(line);
            int port;
            if(parts[0].equals(CommandParams.PORT.name())) {
                port = Integer.valueOf(parts[1]);
            } else {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            }

            // Get clients name
            line = fromClient.readLine();
            parts = getCommandAndParams(line);
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

    /**
     *  Handles a Client request send a message to a ChatRoom
    */
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

            ChatRoom chatRoom = server.getChatRoomById(chatRoomId);
            if(chatRoom == null) {
                throw new ClientException(ClientExceptionTypes.NO_CHATROOM_FOUND, toClient);
            } else if(!chatRoom.isClientInChatRoom(joinId)) {
                throw new ClientException(ClientExceptionTypes.NOT_IN_CHATROOM, toClient);
            }

            String clientName;
            line = fromClient.readLine();
            parts = getCommandAndParams(line);
            if(!parts[0].equals(CommandParams.CLIENT_NAME.name())) {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            } else {
                clientName = parts[1];
            }



            // TODO: Maybe add multi line message functionality
            String message;
            line = fromClient.readLine();
            parts = getCommandAndParams(line);
            if(!parts[0].equals(CommandParams.MESSAGE.name())) {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            } else {
                message = parts[1];
            }

            // Read second newline character that terminates the Message
            fromClient.readLine();

            chatRoom.chat(this, message);


        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        }


    }

    /**
     * Handles a Client request to kill the service (shuts down server)
     */
    private void killService() {
        server.killService();
    }

    /**
     * Extracts the command and parameters from a line read from the Client Socket
     * @param line the line read from the Socket
     * @return an array containing the command and the parameter as Strings
     * @throws ClientException if the command is not recognised
     */
    private String[] getCommandAndParams(String line) throws ClientException {
        String[] parts = line.split(": ");

        if(parts.length == 1) {
            // No semi colon found, may be a HELO or KILL_SERVICE command.
            if(parts[0].contains(Command.HELO.command)) {
                String[] parts2 = {Command.HELO.command, parts[0].substring(5)};
                return parts2;
            } else if(parts[0].contains(Command.KILL_SERVICE.command)) {
                String[] parts2 = {Command.KILL_SERVICE.command, null};
                return parts2;
            }
            else {
                throw new ClientException(ClientExceptionTypes.INVALID_SYNTAX, toClient);
            }
        }

        parts[0] = parts[0].toUpperCase();
        System.out.println("Valid command: " + parts[0]);
        return parts;
    }


    /*******************************
        API Methods
     *******************************/

    /**
     * Sends an array of messages (each terminated with a newline) to the client immediately.
     * @param messages the lines to be sent to the client
     */
    public void sendMessageToClient(String[] messages) {
        try {
            for(String message : messages) {
                toClient.writeLine(message);
            }
            toClient.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    /**
     * Safely terminates the client's connection to the server.
     */
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

    /*******************************
        Getters and Setters
     *******************************/

    /**
     * @return the clientId of the Client instance
     */
    public int getClientId() {
        return this.clientId;
    }

    /**
     * @param chatRoomId the Id of the ChatRoom instance
     * @return the name of this Client in that ChatRoom
     */
    public String getNameInChatroom(int chatRoomId) {
        return this.clientsNamesInChatrooms.get(chatRoomId);
    }

    /**
     * Sets the name of the Client in the ChatRoom instance which has the given Id
     * @param chatRoomId the Id of the ChatRoom
     * @param name the desired name of this Client in the ChatRoom
     */
    public void setNameInChatroom(int chatRoomId, String name) {
        clientsNamesInChatrooms.put(chatRoomId, name);
    }

}
