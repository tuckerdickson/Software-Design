import java.util.LinkedList;

public class Player {
    private LinkedList<Card> hand;
    private int handSum;
    private boolean isOver;
    private boolean isStanding;

    public Player() {
        reset();
    }

    public void addToHand(Card card) {
        hand.add(card);
        calculateSum(card);
    }

    public LinkedList<Card> getHand() { return hand; }
    public int getHandSum() { return handSum; }

    public boolean isOver() { return isOver; }
    public boolean didWin(Player otherPlayer) {
        if (handSum > 21) {
            return false;
        }
        if (otherPlayer.handSum > 21) {
            return true;
        }

        return handSum > otherPlayer.handSum;
    }

    public void calculateSum(Card card) {
        handSum = handSum + card.getValue();

        if (handSum > 21) { isOver = true; }
    }

    public void reset() {
        hand = new LinkedList<>();
        handSum = 0;
        isOver = false;
        isStanding = false;
    }
}
