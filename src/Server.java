import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
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
    private boolean running = false;
    private ArrayList<Thread> threads;
    private ArrayList<Client> clients;
    private HashMap<Integer, ChatRoom> chatRooms;
    private static int nextChatRoomId = 0;

    public Server(int port) {
        try {
            serverSocket = new ServerSocket(port);
            threads = new ArrayList<>();
            clients = new ArrayList<>();
            chatRooms = new HashMap();
            System.out.println("Server started at " + getIP() +  " on " + getPort());
            createChatRoom("Default");
            running = true;
            listen();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public ChatRoom joinChatRoom(Client client, String name) {
        for(ChatRoom chatRoom : chatRooms.values()) {
            if(chatRoom.getChatRoomName().equals(name)) {
                chatRoom.subscribeToChatRoom(client);
                return chatRoom;
            }
        }

        // TODO: Limit who has access to this functionality
        ChatRoom chatRoom =  createChatRoom(name);
        chatRoom.subscribeToChatRoom(client);
        return chatRoom;
    }

    private ChatRoom createChatRoom(String name) {
        ChatRoom chatRoom = new ChatRoom(this, nextChatRoomId, name);
        chatRooms.put(nextChatRoomId++, chatRoom);
        return chatRoom;
    }

    private void listen() {
        while(running) {
            try {
                Socket socket = serverSocket.accept();
                System.out.println("Client created, starting thread");
                Client client = new Client(this, socket);
                Thread thread = new Thread(client);
                thread.start();
                threads.add(thread);

            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public String getIP() {
        return "86.43.98.198";
    }

    public int getPort() {
        return this.serverSocket.getLocalPort();
    }
}

