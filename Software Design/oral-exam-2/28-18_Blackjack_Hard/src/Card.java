import java.io.Serializable;

public class Card implements Serializable {
    private int value;
    private char suit;
    private char face;

    public Card(int value, char suit, char face) {
        setValue(value);
        setSuit(suit);
        setFace(face);
    }

    public void setValue(int value) {
        if (value>= 0 && value <= 10) {
            this.value = value;
        } else {
            System.out.println("Invalid value. Failed to construct Card.");
        }
    }

    public void setSuit(char suit) {
        if (suit == 'd' || suit == 'c' || suit == 's' || suit == 'h') {
            this.suit = suit;
        } else {
            System.out.println("Invalid suit. Failed to construct Card.");
        }
    }

    public void setFace(char face) {
        if (face == '1' || face == '2' || face == '3' || face == '4' || face == '5' ||
                face == '6' || face == '7' || face == '8' || face == '9' || face == 't' ||
                face =='j' || face == 'q' || face == 'k') {
            this.face = face;
        } else {
            System.out.println("Invalid face. Failed to construct Card.");
        }
    }

    public int getValue() { return value; }
    public char getSuit() { return suit; }
    public char getFace() { return face; }

    @Override
    public String toString() {
        return ("\n" + face + " of " + suit);
    }
}
