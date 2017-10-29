import java.util.HashMap;

public class ChatRoom {
    private Server server;
    private int chatRoomId;
    private String chatRoomName;
    private HashMap<Integer, Client> connectedClients;

    ChatRoom(Server server, int chatRoomId, String chatRoomName) {
        this.server = server;
        this.chatRoomId = chatRoomId;
        this.chatRoomName = chatRoomName;
        connectedClients = new HashMap<>();
        System.out.println("Created chatroom " + chatRoomId + " - " + chatRoomName);
    }

    public void chat(Client client, String message) {
        if(message.endsWith("\n\n")) {
            message = message.substring(0,message.length() -3);
        } else if(!message.endsWith("\n")) {
            message = message.concat("\n");
        }

        System.out.println("Sending message " + message);

        String[] fullMessage = {
                "CHAT: " + this.chatRoomId,
                "CLIENT_NAME: " + client.getNameInChatroom(this.chatRoomId),
                "MESSAGE: " + message
        };

        broadcastToAllClients(fullMessage);
    }


    public void subscribeToChatRoom(Client client, String clientNameInThisChatRoom) {
        connectedClients.put(client.getClientId(), client);
        client.setNameInChatroom(this.chatRoomId, clientNameInThisChatRoom);
        respondToClientJoinChatRoom(client);
        chat(client, client.getNameInChatroom(this.chatRoomId) + " has joined the chatroom");

    }

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



    public void unsubscribeFromChatRoom(Client client) {
        respondToClientUnsubscribingFromChatRoom(client);
        chat(client, client.getNameInChatroom(this.chatRoomId) + " has left the chatroom");
        connectedClients.remove(client.getClientId());
    }

    private void respondToClientUnsubscribingFromChatRoom(Client client) {
        String[] messages = {
            "LEFT_CHATROOM: " + String.valueOf(this.chatRoomId),
            "JOIN_ID: " + String.valueOf(client.getClientId())
        };

        client.sendMessageToClient(messages);
    }


    public void notifyOfClientTermination(Client client) {
        if(connectedClients.remove(client.getClientId()) != null) {
            broadcastToAllClients(this.chatRoomName + ": " + client.getNameInChatroom(this.chatRoomId) + " has left the chatroom");
        }
    }


    // Hide actual sending of messages to clients from Client
    // Could implement some safety precautions here
    private void broadcastToAllClients(String message) {
        for(Client client : connectedClients.values()) {
            client.sendMessageToClient(message);
        }
    }

    private void broadcastToAllClients(String[] message) {
        for(Client client : connectedClients.values()) {
            client.sendMessageToClient(message);
        }
    }


    /*
        Getters and Setters
     */


    public int getChatRoomId() {
        return chatRoomId;
    }

    public String getChatRoomName() {
        return chatRoomName;
    }
}
