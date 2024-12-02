public class Card implements Comparable<Card> {
    private final int value;
    private int tokens; // Tokens associated with the card

    public Card(int value, String suit) {
        this.value = value;
        this.tokens = 0; // Initial tokens set to 0
    }

    public int getValue() {
        return value;
    }

    public int getTokens() {
        return tokens;
    }

    // Method to add tokens to the card
    public void addTokens(int tokens) {
        this.tokens += tokens;
    }

    // Method to remove tokens from the card
    public void removeTokens(int tokens) {
        this.tokens -= tokens;
    }

    @Override
    public int compareTo(Card other) {
        return Integer.compare(this.value, other.value); // Natural order by value
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Card card = (Card) obj;
        return value == card.value;
    }

}
