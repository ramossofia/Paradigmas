import java.util.Objects;

public class Card {
    private final int value;
    public int tokens;

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
        Objects.requireNonNull(tokens <= this.tokens, "Not enough tokens to remove.");
        this.tokens -= tokens;
    }
}
