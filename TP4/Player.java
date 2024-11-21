public class Player {
    private String name;
    private int tokens;
    private boolean hasPlayedThisTurn = false; // Flag to track if the player has played this turn

    public Player(String name, int initialTokens) {
        this.name = name;
        this.tokens = initialTokens;
    }

    public String getName() {
        return name;
    }

    public int getTokens() {
        return tokens;
    }

    public void setTokens(int tokens) {
        this.tokens = tokens;
    }

    public boolean hasPlayedThisTurn() {
        return hasPlayedThisTurn;
    }

    public void resetTurn() {
        hasPlayedThisTurn = false;  // Reset at the start of each turn
    }

    public void decrementTokens() {
        if (tokens <= 0) {
            throw new IllegalStateException(name + " has no tokens left.");
        }
        tokens--;  // Decrease token count
    }

    public void setHasPlayedThisTurn(boolean played) {
        this.hasPlayedThisTurn = played;
    }

    public void takeCard(Card card, int tokens) {
        this.tokens += tokens;
        // Additional logic for taking a card can be added here
    }
}
