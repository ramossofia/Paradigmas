public class Card {
    private int value;
    private int tokens;

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

    public void addTokens(int tokens) {
        this.tokens += tokens;
    }

}
