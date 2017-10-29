import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.HashMap;


public class Server {

    private static final int DEFAULT_PORT = 3000;

    public static void main(String[] args) {
        int port;
        if(args.length > 0) {
            port = Integer.valueOf(args[0]);
        } else {
            port = DEFAULT_PORT;
        }
        new Server(port);
    }


    private ServerSocket serverSocket;
    private volatile boolean running = false;
    private ArrayList<Client> clients;
    private ArrayList<Thread> threads;
    private HashMap<Integer, ChatRoom> chatRooms;
    private static int nextChatRoomId = 0;
    private static int nextClientJoinId = 0;

    public Server(int port) {
        try {
            serverSocket = new ServerSocket(port);
            clients = new ArrayList<>();
            threads = new ArrayList<>();
            chatRooms = new HashMap();
            System.out.println("Server started at " + getIP() +  " on " + getPort());
            createChatRoom("Default");
            running = true;
            listen();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public int joinChatRoom(Client client, String clientNameInThisChatRoom, String name) {
        for(ChatRoom chatRoom : chatRooms.values()) {
            if(chatRoom.getChatRoomName().equals(name)) {
                chatRoom.subscribeToChatRoom(client, clientNameInThisChatRoom);
                return chatRoom.getChatRoomId();
            }
        }

        ChatRoom chatRoom =  createChatRoom(name);
        chatRoom.subscribeToChatRoom(client, clientNameInThisChatRoom);
        return chatRoom.getChatRoomId();
    }

    // TODO: Limit who has access to this functionality
    private ChatRoom createChatRoom(String name) {
        ChatRoom chatRoom = new ChatRoom(this, nextChatRoomId, name);
        chatRooms.put(nextChatRoomId++, chatRoom);
        return chatRoom;
    }


    public void terminateClientConnection(Client client) {
        for(ChatRoom chatRoom : chatRooms.values()) {
            chatRoom.notifyOfClientTermination(client);
        }

        clients.remove(client.getClientId());
    }

    public ChatRoom getChatRoomById(int chatRoomId) {
        return chatRooms.get(chatRoomId);
    }

    private void listen() {
        while(running) {
            try {
                Socket socket = serverSocket.accept();
                Client client = new Client(this, socket, nextClientJoinId);
                System.out.println("Client " + nextClientJoinId++ + " created");
                Thread thread = new Thread(client);
                thread.start();
                clients.add(client);

            } catch (SocketException s) {
                System.out.println("Shutting down server");
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public void killService() {
        this.running = false;
        for(Client client : clients) {
            client.safelyTerminate();
        }
        for(Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

        }
        try {
            serverSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String getIP() {
        return "86.43.98.198";
    }

    public int getPort() {
        return this.serverSocket.getLocalPort();
    }
}

