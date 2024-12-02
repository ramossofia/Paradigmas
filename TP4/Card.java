public class Card implements Comparable<Card> {
    private final int value;
    private int tokens;

    public Card(int value) {
        this.value = value;
        this.tokens = 0;
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

    public void removeTokens(int tokens) {
        this.tokens -= tokens;
    }

    @Override
    public int compareTo(Card other) {
        return Integer.compare(this.value, other.value);
    }
}
