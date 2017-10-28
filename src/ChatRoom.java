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


    public void broadcastToAllClients(String message) {
        for(Client client : connectedClients.values()) {
            client.sendMessage(message);
        }
    }

    public void broadcastToAllClients(String[] message) {
        for(Client client : connectedClients.values()) {
            client.sendMessage(message);
        }
    }

    public void subscribeToChatRoom(Client client, String clientNameInThisChatRoom) {
        connectedClients.put(client.getClientId(), client);
        respondToClientJoinChatRoom(client);
        broadcastToAllClients(this.chatRoomName + ": " + clientNameInThisChatRoom + " has joined the chatroom");
    }

    public void unsubscribeFromChatRoom(Client client, String clientNameInThisChatRoom) {
        broadcastToAllClients(this.chatRoomName + ": " + clientNameInThisChatRoom + " has left the chatroom");
        connectedClients.remove(client.getClientId());
        respondToClientUnsubscribingFromChatRoom(client);
    }

    private void respondToClientJoinChatRoom(Client client) {
        String[] messages = {
            "JOINED_CHATROOM: " + chatRoomName,
            "SERVER_IP: " + server.getIP(),
            "PORT: " + server.getPort(),
            "ROOM_REF: " + chatRoomId,
            "JOIN_ID: " + client.getClientId()
        };

        client.sendMessage(messages);
    }

    private void respondToClientUnsubscribingFromChatRoom(Client client) {
        String[] messages = {
            "LEFT_CHATROOM: " + chatRoomId,
            "JOIN_ID: " + client.getClientId()
        };

        client.sendMessage(messages);
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
