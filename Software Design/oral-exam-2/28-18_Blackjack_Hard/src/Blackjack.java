public class Blackjack {

    public static void main(String[] args) {
        Deck deck = new Deck();

        for(int i = 0; i < 60; i++) {
            Card dealCard = deck.deal();
            System.out.println("Dealing " + dealCard.getFace() + " of " + dealCard.getSuit());
        }

        System.out.println("\nDeck after dealing:");
        deck.print();
    }

}
