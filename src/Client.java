import java.io.*;
import java.net.Socket;


public class Client implements Runnable {

    enum Command {
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



    private Server server;
    private Socket socket;
    private BufferedReader fromClient;
    private MyBufferedWriter toClient;
    private int clientId;



    Client(Server server, Socket socket, int clientId) {
        this.server = server;
        this.socket = socket;
        this.clientId = clientId;

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
            if (command.equals(Command.JOIN_CHATROOM.command)) {
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
            for (int i = 1; i < Command.JOIN_CHATROOM.numLines() - 1; i++) {
                System.out.println("Throwing away: " + fromClient.readLine());
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

    private void leaveChatRoom(String roomRef) {
        try {
            int roomReferenceNumber = Integer.valueOf(roomRef);

            String line = fromClient.readLine();
            int joinId = Integer.valueOf(getParams(line));

            line = fromClient.readLine();
            String clientNameInThisChatRoom = getParams(line);

            ChatRoom chatRoom = server.getChatRoomById(joinId);
            if(chatRoom == null) {
                throw new ClientException(ClientExceptionTypes.NO_CHATROOM_FOUND, toClient);
            }

            chatRoom.unsubscribeFromChatRoom(this, clientNameInThisChatRoom);

        } catch (ClientException c) {
            c.printStackTrace();
            c.sendError();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void disconnectFromChatRoom(String param) {

    }

    private void chat(String param) {
        try {
            int chatRoomId, joinId;
            String clientName, message;
            chatRoomId = Integer.valueOf(param);

            String line = fromClient.readLine();
            System.out.println(line);

            String[] parts = getCommandAndParams(line);
            if(!parts[0].equals(CommandParams.JOIN_ID.name())) {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            } else {
                joinId = Integer.valueOf(parts[1]);
            }


            line = fromClient.readLine();
            System.out.println(line);

            parts = getCommandAndParams(line);
            if(!parts[0].equals(CommandParams.CLIENT_NAME.name())) {
                throw new ClientException(ClientExceptionTypes.UNKNOWN_COMMAND, toClient);
            } else {
                clientName = parts[1];
            }


            line = fromClient.readLine();
            System.out.println(line);

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

            String[] fullMessage = {
                "CHAT: " + chatRoomId,
                "CLIENT_NAME: " + clientName,
                "MESSAGE: " + message + "\n\n"
            };

            chatRoom.broadcastToAllClients(fullMessage);



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
        String[] parts = line.toUpperCase().split(": ");
        if(parts.length == 1) {
            throw new ClientException(ClientExceptionTypes.INVALID_SYNTAX, toClient);
        }

        return parts;
    }


    public int getClientId() {
        return this.clientId;
    }

}

/*

FRANKS STUFF
JOIN_CHATROOM: Default
CLIENT_IP: 0
PORT: 0
CLIENT_NAME: Frank

CHAT: 1
JOIN_ID: 0
CLIENT_NAME: Frank
MESSAGE: I Love Cats!\n\n

JOIN_CHATROOM: Franks Cats
CLIENT_IP: 0
PORT: 0
CLIENT_NAME: Frank

CHAT: 1
JOIN_ID: 0
CLIENT_NAME: Frank
MESSAGE: Join my CATroom ha!\n\n

CHAT: 2
JOIN_ID: 0
CLIENT_NAME: Frank
MESSAGE: Does everyone here love cats?\n\n


STEFANO Stuff
JOIN_CHATROOM: Default
CLIENT_IP: 0
PORT: 0
CLIENT_NAME: Stefano

CHAT: 1
JOIN_ID: 1
CLIENT_NAME: Stefano
MESSAGE: I Love Dogs!\n\n

JOIN_CHATROOM: Franks Cats
CLIENT_IP: 0
PORT: 0
CLIENT_NAME: Stefano

CHAT: 2
JOIN_ID: 1
CLIENT_NAME: Stefano
MESSAGE: Dogs rule cats drool\n\n


// CATDOG
JOIN_CHATROOM: Default
CLIENT_IP: 0
PORT: 0
CLIENT_NAME: CatDogzz2k8

CHAT: 1
JOIN_ID: 1
CLIENT_NAME: CatDogzz2k8
MESSAGE: I Love cats & Dogs!\n\n

JOIN_CHATROOM: Franks Cats
CLIENT_IP: 0
PORT: 0
CLIENT_NAME: CatDogzz2k8

CHAT: 2
JOIN_ID: 1
CLIENT_NAME: CatDogzz2k8
MESSAGE: Dogs rule cats also rule\n\n
*/
