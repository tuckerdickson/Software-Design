import java.util.Collections;
import java.util.LinkedList;

public class Deck {
    private LinkedList<Card> cardDeck;

    public Deck() {
        cardDeck = new LinkedList<>();
        this.reshuffle();
    }

    public void shuffle() {
        Collections.shuffle(cardDeck);
    }

    public void reshuffle() {
        // add numeric cards (except ten)
        for(int i = 1; i <=9; i++) {
            cardDeck.add(new Card(i, 'h', (char)(i + '0')));
            cardDeck.add(new Card(i, 'd', (char)(i + '0')));
            cardDeck.add(new Card(i, 's', (char)(i + '0')));
            cardDeck.add(new Card(i, 'c', (char)(i + '0')));
        }

        // face cards are worth ten; add four more value ten cards for each suit
        for(int i = 0; i < 4; i++) {
            char face;
            if (i == 0) { face = 't'; }
            else if (i == 1) { face = 'j'; }
            else if (i == 2) { face = 'q'; }
            else { face = 'k'; }

            cardDeck.add(new Card(10, 'h', face));
            cardDeck.add(new Card(10, 'd', face));
            cardDeck.add(new Card(10, 's', face));
            cardDeck.add(new Card(10, 'c', face));
        }
    }

    public Card deal() {

        Card returnCard;

        if (cardDeck.size() > 0) {
            returnCard = cardDeck.pop();
        } else {
            System.out.println("\nThe current deck is out of cards. Reshuffling...");
            this.reshuffle();
            this.shuffle();
            returnCard = cardDeck.pop();
        }

        return returnCard;
    }

    public void print() {
        for(Card c : cardDeck) {
            System.out.println(c);
        }
    }
}
