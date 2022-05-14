import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.net.InetAddress;
import java.net.Socket;

public class Client extends JFrame {
    private JTextArea displayArea;

    private ObjectOutputStream output;      // output stream to server
    private ObjectInputStream input;        // input from server

    private String message = "";            // message from the server
    private String blackjackServer;         // host server for application
    private Socket client;                  // socket to communicate with server

    private JButton hitButton;              // button that the user hits if they want another card
    private JButton standButton;            // button that the user hits if they don't want another card
    private JButton restartButton;

    private Player player;
    private Card card;

    public Client(String host) {
        super("Blackjack Client");

        blackjackServer = host;     // set the server that this client will connect to
        player = new Player();

        Box box = Box.createHorizontalBox();
        JScrollPane scrollPane;

        hitButton = new JButton("Hit");
        hitButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                sendData("hit");
            }
        });
        box.add(hitButton);

        standButton = new JButton("Stand");
        standButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                hitButton.setEnabled(false);
                standButton.setEnabled(false);
                sendData("stand");
            }
        });
        box.add(standButton);

        restartButton = new JButton("Restart");
        restartButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                sendData("restart");
                player.reset();
                hitButton.setEnabled(true);
                standButton.setEnabled(true);
                displayArea.setText("");
            }
        });
        box.add(restartButton);

        displayArea = new JTextArea();

        setSize(300,300);
        setVisible(true);

        scrollPane = new JScrollPane(displayArea);

        add(scrollPane, BorderLayout.CENTER);
        add(box, BorderLayout.NORTH);
    }

    // connect to server and process messages from server
    public void runClient() {
        try {
            connectToServer();      // create a socket to make a connection
            getStreams();           // get input and output streams
            processConnection();    // process the connection
        } catch (EOFException eofException) {
            displayMessage("\nClient terminated connection.");
        } catch (IOException ioException) {
            ioException.printStackTrace();
        } finally {
            closeConnection();
        }
    }

    private void connectToServer() throws IOException {
        // create socket to make connection to server
        client = new Socket(InetAddress.getByName(blackjackServer), 23535);
    }

    private void getStreams() throws IOException {
        // set up output stream for objects
        output = new ObjectOutputStream(client.getOutputStream());
        output.flush();

        // set up input stream for objects
        input = new ObjectInputStream(client.getInputStream());
    }

    private void processConnection() throws IOException {
        // process the data sent from the server
        do {
            // try to read in a card object
            try {
                card = (Card) input.readObject();
                displayMessage(card.toString());

                // add the card that was read in to the player's hand
                player.addToHand(card);

                if (player.isOver()) {
                    hitButton.setEnabled(false);
                    standButton.setEnabled(false);
                    sendData("playerLost");
                }
            // throw exception if Object can't be converted to Card
            } catch (ClassNotFoundException classNotFoundException) {
                displayMessage("Unknown object type received.");
            }
        } while (!message.equals("quit"));
    }

    private void closeConnection() {
        displayMessage("\nClosing connection.");

        try {
            output.close();     // close output stream
            input.close();      // close input stream
            client.close();     // close socket
        } catch (IOException ioException) {
            ioException.printStackTrace();
        }
    }

    private void sendData(String message) {
        try {
            output.writeObject(message);
            output.flush();
        } catch (IOException ioException) {
            displayArea.append("\nError writing object");
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