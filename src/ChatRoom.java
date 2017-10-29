import java.util.HashMap;

/**
 * Holds all of the required data for a ChatRoom
 */
public class ChatRoom {
    private Server server;
    private int chatRoomId;
    private String chatRoomName;
    private HashMap<Integer, Client> connectedClients;

    /**
     * Creates the ChatRoom
     * @param server the Server instance that created the ChatRoom
     * @param chatRoomId the id given to this ChatRoom by the Server
     * @param chatRoomName the name given to this ChatRoom by the Server
     */
    ChatRoom(Server server, int chatRoomId, String chatRoomName) {
        this.server = server;
        this.chatRoomId = chatRoomId;
        this.chatRoomName = chatRoomName;
        connectedClients = new HashMap<>();
        System.out.println("Created chatroom " + chatRoomId + " - " + chatRoomName);
    }


    /*******************************
     API Methods
     *******************************/

    /**
     * Sends a message to the ChatRoom with appropriate header information.
     * @param sender the Client who sent this message
     * @param message the message to send to the ChatRoom
     */
    public void chat(Client sender, String message){

        // Ensure all messages are terminated with ONE new line character
        // The second new line will be provided by the method that writes the message to the Socket
        if(message.endsWith("\n\n")) {
            message = message.substring(0,message.length() - 3);
        } else if(!message.endsWith("\n")) {
            message = message.concat("\n");
        }

        String[] fullMessage = {
                "CHAT: " + this.chatRoomId,
                "CLIENT_NAME: " + sender.getNameInChatroom(this.chatRoomId),
                "MESSAGE: " + message
        };

        broadcastToAllClients(fullMessage);
    }


    /**
     * Allows the Client to join this ChatRoom
     * @param client the Client who wishes to join this ChatRoom
     * @param clientNameInThisChatRoom the Client's desired name in this ChatRoom
     */
    public void joinChatRoom(Client client, String clientNameInThisChatRoom) {
        connectedClients.put(client.getClientId(), client);

        // Setting the clients in this ChatRoom here as the Client doesn't yet have a reference to this
        // ChatRoom before it calls this method and thus can't add it to its own data structure itself.
        client.setNameInChatroom(this.chatRoomId, clientNameInThisChatRoom);
        respondToClientJoinChatRoom(client);
        chat(client, client.getNameInChatroom(this.chatRoomId) + " has joined the chat room.");

    }


    /**
     * Allows the Client to leave this ChatRoom
     * @param client the Client who wishes to leave.
     */
    public void leaveChatRoom(Client client) {
        respondToClientLeavingChatRoom(client);
        chat(client, client.getNameInChatroom(this.chatRoomId) + " has left the chat room.");
        connectedClients.remove(client.getClientId());
    }

    /**
     * Informs the ChatRoom that a Client has terminated the connection to the Server using a DISCONNECT command.
     * @param client the Client who has disconnected from the server.
     */
    public void notifyOfClientTermination(Client client) {
        if(connectedClients.get(client.getClientId()) == null) {
            return;
        }
        chat(client, client.getNameInChatroom(this.chatRoomId) + " has disconnected from the chat room.");
        connectedClients.remove(client.getClientId());
    }

    /**
     * Checks if a Client with the specified id is in this ChatRoom. This method is used by the Client for validating
     * whether or not a Client may send a message to the request ChatRoom
     * @param clientId the id of the Client who wishes to send a message to this ChatRoom
     * @return true if Client is in this ChatRoom, false otherwise.
     */
    public boolean isClientInChatRoom(int clientId) {
        return connectedClients.get(clientId) != null;
    }


    /*************************************
     * Private instance methods
     ************************************/

    /**
     * Sends ChatRoom JOIN confirmation back to the Client.
     * @param client the Client who joined the ChatRoom
     */
    private void respondToClientJoinChatRoom(Client client) {
        String[] messages = {
                "JOINED_CHATROOM: " + chatRoomName,
                "SERVER_IP: " + server.getIP(),
                "PORT: " + server.getPort(),
                "ROOM_REF: " + chatRoomId,
                "JOIN_ID: " + client.getClientId()
        };

        client.sendMessageToClient(messages);
    }

    /**
     * Sends confirmation of leaving ChatRoom back to Client.
     * @param client the Client who has just left the ChatRoom
     */
    private void respondToClientLeavingChatRoom(Client client) {
        String[] messages = {
                "LEFT_CHATROOM: " + String.valueOf(this.chatRoomId),
                "JOIN_ID: " + String.valueOf(client.getClientId())
        };

        client.sendMessageToClient(messages);
    }

    /**
     * Broadcasts the message (which contains individual lines) to all Clients connected to the ChatRoom
     * @param message the messages to send
     */
    private void broadcastToAllClients(String[] message) {
        for(Client client : connectedClients.values()) {
            System.out.println("Sending " + message[2] + " to client " + client.getClientId());
            client.sendMessageToClient(message);
        }
    }


    /*******************************
     Getters and Setters
     *******************************/
    public String getChatRoomName() {
        return chatRoomName;
    }
}
