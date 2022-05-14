import java.util.ArrayList;
import java.util.Random;

/**
 * This class is responsible for storing all of the information about the game and handling all of the game mechanics
 * that don't directly affect the GUI components. Some of its important functions include generating new words and
 * keeping track of the number of lives remaining, the incorrect guesses made so far, and all of the guesses made so far.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 11/10/21
 */
public class HangmanModel {

    /**
     * This private array of Strins holds 213 possible words to choose from. The words in this list were taken from
     * https://www.hangmanwords.com/words
     */
    private String[] wordList;

    /**
     * This private String represents the word being used in a game
     */
    private String word;

    /**
     * This ArrayList of Strings holds all of the characters incorrectly guessed by the user
     */
    private ArrayList<String> incorrectGuesses;

    /**
     * This ArrayList of Strings holds all of the guesses by the user
     */
    private ArrayList<String> totalGuesses;

    /**
     * This private int represents the number of "lives" the user has remaining
     */
    private int numLives;

    public HangmanModel() {
        /*
        the following words were taken from https://www.hangmanwords.com/words
         */
         wordList = new String[]{"abruptly", "absurd", "abyss", "affix", "askew", "avenue", "awkward", "axiom", "azure",
                "bagpipes", "bandwagon", "banjo", "bayou", "beekeeper", "bikini", "blitz", "blizzard", "boggle",
                "bookworm", "boxcar", "boxful", "buckaroo", "buffalo", "buffoon", "buxom", "buzzard", "buzzing", "buzzwords",
                "caliph", "cobweb", "cockiness", "croquet", "crypt", "curacao", "cycle",
                "daiquiri", "dirndl", "disavow", "dizzying", "duplex", "dwarves",
                "embezzle", "equip", "espionage", "euouae", "exodus",
                "faking", "fishhook", "fixable", "fjord", "flapjack", "flopping", "fluffiness", "flyby",
                "foxglove", "frazzled", "frizzled", "fuchsia", "funny",
                "gabby", "galaxy", "galvanize", "gazebo", "giaour", "gizmo", "glowworm", "glyph", "gnarly", "gnostic",
                "gossip", "grogginess",
                "haiku", "haphazard", "hyphen",
                "iatrogenic", "icebox", "injury", "ivory", "ivy",
                "jackpot", "jaundice", "jawbreaker", "jaywalk", "jazziest", "jazzy", "jelly", "jigsaw", "jinx",
                "jiujitsu", "jockey", "jogging", "joking", "jovial", "joyful", "juicy", "jukebox", "jumbo",
                "kayak", "kazoo", "keyhole", "khaki", "kilobyte", "kiosk", "kitsch", "kiwifruit", "klutz", "knapsack",
                "larynx", "lengths", "lucky", "luxury", "lymph",
                "marquis", "matrix", "megahertz", "microwave", "mnemonic", "mystify",
                "naphtha", "nightclub", "nowadays", "numbskull", "nymph",
                "onyx", "ovary", "oxidize", "oxygen",
                "pajama", "peekaboo", "phlegm", "pixel", "pizazz", "pneumonia", "polka", "pshaw", "psyche",
                "puppy", "puzzling",
                "quartz", "queue", "quips", "quixotic", "quiz", "quizzes", "quorum",
                "razzmatazz", "rhubarb", "rhythm", "rickshaw",
                "schnapps", "scratch", "shiv", "snazzy", "sphinx", "spritz", "squawk", "staff", "strength", "strengths",
                "stretch", "stronghold", "stymied", "subway", "swivel", "syndrome",
                "thriftless", "thumbscrew", "topaz", "transcript", "transgress", "transplant", "triphthong", "twelfth", "twelfths",
                "unknown", "unworthy", "unzip", "uptown",
                "vaporize", "vixen", "vodka", "voodoo", "vortex", "voyeurism",
                "walkway", "waltz", "wave", "wavy", "waxy", "wellspring", "wheezy", "whiskey", "whizzing", "whomever",
                "wimpy", "witchcraft", "wizard", "woozy", "wristwatch", "wyvern",
                "xylophone",
                "yachtsman", "yippee", "yoked", "youthful", "yummy",
                "zephyr", "zigzag", "zigzagging", "zilch", "zipper", "zodiac", "zombie"};

         // call clear method to initialize numLives, incorrectGuesses, totalGuesses, and word
        this.clear();
    }

    /**
     * This method acts as the getter for word. It simply returns word.
     *
     * @return This function returns the word private instance variable.
     */
    String getWord() {
        return word;
    }

    /**
     * This method acts as the get for numLives. It simply returns numLives.
     *
     * @return This method returns an int representing numLives.
     */
    int getNumLives() { return numLives; }

    /**
     * This method acts as the getter for incorrectGuesses. It simply returns incorrectGuesses.
     *
     * @return This method returns an ArrayList of Strings referring to incorrectGuesses.
     */
    ArrayList<String> getIncorrectGuesses() { return incorrectGuesses; }

    /**
     * This method acts as the getter for totalGuesses. It simply returns totalGuesses.
     *
     * @return This method returns an ArrayList of Strings referring to totalGuesses.
     */
    ArrayList<String> getTotalGuesses() { return totalGuesses; }

    /**
     * This method acts as the setter for numLives. It takes in an int and uses it to initialize numLives.
     *
     * @param numLives This int is used to initialize numLives.
     */
    void setNumLives(int numLives) {
        this.numLives = numLives;
    }

    /**
     * This method takes in a String and adds it to incorrectGuesses. It does this by using the List.add() method.
     *
     * @param guess This String will be added to incorrectGuesses.
     */
    void addIncorrectGuess(String guess) {
        incorrectGuesses.add(guess);
    }

    /**
     * This method takes in a String and adds it to totalGuesses. It does this by using the List.add() method.
     *
     * @param guess This String will be added to totalGuesses.
     */
    void addGuess(String guess) {
        totalGuesses.add(guess);
    }

    /**
     * This method is responsible for determining whether the game is over. If numLives is less than or equal to zero,
     * this method returns false, otherwise it returns true.
     *
     * @return The method returns a boolean value indicating whether the game is over.
     */
    boolean gameOver() {
        if (numLives <= 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * This method is responsible for determining whether the player won the game or not. It does this by evaluating
     * numLives and then comparing the letters in totalGuesses with the letters in word. If numLives is greater than
     * zero and all of the letters in word have been guessed, this method returns true.
     *
     * @return This method returns a boolean value indicating whether the user has won the game.
     */
    boolean didWin() {

        boolean livesLeft = numLives > 0;   // does the user have more than zero lives?
        boolean guessedLetters = true;      // has the user guessed all of the letters in the word?

        // loop over each of the characters in word, checking to see if they're all in totalGuesses
        // if at any point, word contains a letter that isn't in totalGuesses, set guessedLetters to false
        for (int i = 0; i < word.length(); i++) {
            String currChar = String.valueOf(word.charAt(i));
            if (!totalGuesses.contains(currChar)) {
                guessedLetters = false;
            }
        }

        // only returns true when numLives is greater than zero and all of the letters in word have been guessed
        return livesLeft && guessedLetters;
    }

    /**
     * This method is responsible for re-initializing all of the member variables to their start-game state. This method
     * is called when the a new HangmanModel object is constructed (at the start of the first game), and then again
     * each subsequent game.
     */
    void clear() {
        // use a random number generator to select a word out of the wordList
        Random random = new Random();
        int wordIndex = random.nextInt(213);
        word = wordList[wordIndex];
        System.out.println(word);


        incorrectGuesses = new ArrayList<>(0);  // reinitialize incorrectGuesses to empty ArrayList
        totalGuesses = new ArrayList<>(0);      // reinitialize totalGuesses to empty ArrayList
        numLives = 6;                                       // reinitialize numLives to six
    }


}
