import java.util.HashMap;

public class ChatRoom {
    private Server server;
    private int chatRoomId;
    private String chatRoomName;
    private HashMap<Integer, Client> connectedClients;
    private int nextClientJoinId = 0;

    ChatRoom(Server server, int chatRoomId, String chatRoomName) {
        this.server = server;
        this.chatRoomId = chatRoomId;
        this.chatRoomName = chatRoomName;
        connectedClients = new HashMap<>();
        System.out.println("Created chatroom " + chatRoomId + " - " + chatRoomName);
    }

    void broadcastToAllClients(String message) {
        for(Client client : connectedClients.values()) {
            client.sendMessage(message);
        }
    }

    void subscribeToChatRoom(Client client) {
        connectedClients.put(nextClientJoinId, client);
        respondToClient(client, nextClientJoinId++);
        System.out.println("Broadcasting to chatroom");
        broadcastToAllClients(client.getClientName() + " has joined the chatroom");
    }

    void respondToClient(Client client, int joinId) {
        String[] messages = {
            "JOINED_CHATROOM: " + chatRoomName,
            "SERVER_IP: " + server.getIP(),
            "PORT: " + server.getPort(),
            "ROOM_REF: " + chatRoomId,
            "JOIN_ID: " + joinId
        };

        client.sendMessage(messages);
    }

    void unsubscribeFromChatRoom(Client client) {
        connectedClients.remove(client);
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
