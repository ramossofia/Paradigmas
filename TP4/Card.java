public class Card implements Comparable<Card> {
    private final int value;
    private final String suit; // Optional for differentiation
    private int tokens; // Tokens associated with the card

    public Card(int value, String suit) {
        this.value = value;
        this.suit = suit;
        this.tokens = 0; // Initial tokens set to 0
    }

    public int getValue() {
        return value;
    }

    public String getSuit() {
        return suit;
    }

    public int getTokens() {
        return tokens;
    }

    // Method to add tokens to the card
    public void addTokens(int tokens) {
        if (tokens < 0) {
            throw new IllegalArgumentException("Cannot add negative tokens.");
        }
        this.tokens += tokens;
    }

    // Method to remove tokens from the card
    public void removeTokens(int tokens) {
        if (tokens < 0) {
            throw new IllegalArgumentException("Cannot remove negative tokens.");
        }
        if (this.tokens < tokens) {
            throw new IllegalStateException("Not enough tokens on the card to remove.");
        }
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
        return value == card.value && suit.equals(card.suit);
    }

    @Override
    public int hashCode() {
        return value * 31 + suit.hashCode();
    }

    @Override
    public String toString() {
        return value + " of " + suit;
    }
}
