public class Card {
    private final int value;
    private final int tokens;

    public Card(int value, int tokens) {
        this.value = value;
        this.tokens = tokens;
    }

    public int getValue() {
        return value;
    }

    public int getTokens() {
        return tokens;
    }

    public Card addTokens(int tokens) {
        return new Card(this.value, this.tokens + tokens);
    }

    public Card resetTokens() {
        return new Card(this.value, 0);
    }
}
