import javax.swing.*;
import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

public class Server extends JFrame {
    private JTextArea displayArea;      // displays information about packets

    private ObjectOutputStream output;  // output stream to client
    private ObjectInputStream input;    // input stream from client

    private ServerSocket server;        // server socket
    private Socket connection;           // connection to client

    private int counter = 1;            // keep count of number of connections

    private Player dealer;
    private Player otherPlayer;
    private Deck cardDeck;

    public Server() {
        super("Blackjack Server");

        Box box = Box.createVerticalBox();
        JScrollPane scrollPane;

        cardDeck = new Deck();
        cardDeck.shuffle();

        dealer = new Player();
        otherPlayer = new Player();

        displayArea = new JTextArea();
        setSize(300,300);
        setVisible(true);

        scrollPane = new JScrollPane(displayArea);
        box.add(scrollPane);

        add(box);
    }

    public void runServer() {
        try {
            server = new ServerSocket(23535, 5);

            while (true) {
                try {
                    waitForConnection();    // wait for a connection
                    getStreams();           // get input and output streams
                    processConnection();    // process the connection
                } catch (EOFException eofException) {
                    displayMessage("\nServer terminated connection.");
                } finally {
                    closeConnection();       // close the connection
                    counter++;
                }
            }
        } catch (IOException ioException) {
            ioException.printStackTrace();
        }
    }

    private void waitForConnection() throws IOException {
        displayMessage("Waiting for connection\n");
        connection = server.accept();   // allow server to accept connection
        displayMessage("Connection " + counter + " received from: " +
                connection.getInetAddress().getHostName());
    }

    private void getStreams() throws IOException {
        // set up output streams for objects
        output = new ObjectOutputStream(connection.getOutputStream());
        output.flush();     // flush output buffer to send header information

        // set up input streams for objects
        input = new ObjectInputStream(connection.getInputStream());

        displayMessage("\nGot I/O Streams\n");
    }

    private void processConnection() throws IOException {
        String message = "";
        displayArea.setText("");

        for(int i = 0; i < 2; i++) {
            Card dealerCard = cardDeck.deal();
            Card otherPlayerCard = cardDeck.deal();

            dealer.addToHand(dealerCard);       // deal first two cards to dealer
            sendData(otherPlayerCard);          // deal first two cards to player
            otherPlayer.addToHand(otherPlayerCard);
        }

        // display dealer's second card
        displayMessage("\n?");
        displayMessage(dealer.getHand().get(1).toString());

        do {
            try {
                message = (String) input.readObject();

                if(message.equals("hit")) {
                    Card otherPlayerCard = cardDeck.deal();
                    sendData(otherPlayerCard);
                    otherPlayer.addToHand(otherPlayerCard);
                } else if (message.equals("stand")) {
                    while((otherPlayer.getHandSum() >= dealer.getHandSum()) && (!dealer.isOver())) {
                        dealer.addToHand(cardDeck.deal());
                    }

                    displayArea.setText("");
                    for (Card c : dealer.getHand()) {
                        displayMessage(c.toString());
                    }

                    if (dealer.didWin(otherPlayer)) {
                        displayMessage("\nLooks like the house takes this one!" +
                                "\nClick the restart button to begin a new game.");
                    } else if (otherPlayer.didWin(dealer)) {
                        displayMessage("\nCongratulations! You win!" +
                                "\nClick the restart button to begin a new game.");
                    } else {
                        displayMessage("\nTie!\nClick the restart button to begin a new game.");
                    }

                } else if (message.equals("restart")) {
                    dealer.reset();
                    otherPlayer.reset();
                    displayArea.setText("");

                    for(int i = 0; i < 2; i++) {
                        Card dealerCard = cardDeck.deal();
                        Card otherPlayerCard = cardDeck.deal();

                        dealer.addToHand(dealerCard);       // deal first two cards to dealer
                        sendData(otherPlayerCard);          // deal first two cards to player
                        otherPlayer.addToHand(otherPlayerCard);
                    }

                    // display dealer's second card
                    displayMessage("\n?");
                    displayMessage(dealer.getHand().get(1).toString());

                } else if (message.equals("playerLost")) {
                    displayMessage("\nLooks like the house takes this one!" +
                            "\nClick the restart button to begin a new game.");
                }

            } catch (ClassNotFoundException classNotFoundException) {
                displayMessage("\nUnknown object type received");
            }
        } while (!message.equals("quit"));
    }

    private void closeConnection() {
        displayMessage("\nTerminating connection\n");

        try {
            output.close();         // close output stream
            input.close();          // close input stream
            connection.close();     // close socket
        } catch (IOException ioException) {
            ioException.printStackTrace();
        }
    }

    private void sendData(Card card) {
        try {
            output.writeObject(card);
            output.flush();
        } catch (IOException ioException) {
            displayArea.append("\nError writing card.");
        }
    }

    private void displayMessage(final String messageToDisplay) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                displayArea.append(messageToDisplay);
            }
        });
    }
}