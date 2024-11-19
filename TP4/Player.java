public class Player {
    private final String name;
    private int tokens;
    private int score;

    public Player(String name, int initialTokens) {
        this.name = name;
        this.tokens = initialTokens;
        this.score = 0;
    }

    public int getTokens() {
        return tokens;
    }

    public int getScore() {
        return score;
    }

    public void addScore(int value) {
        score += value;
    }

    public void takeTokens(int collectedTokens) {
        tokens += collectedTokens;
    }

    public void payToken() {
        if (tokens <= 0) {
            throw new IllegalStateException("Not enough tokens to pay!");
        }
        tokens--;
    }

    public PlayerAction chooseTakeAction() {
        return new Take(this);
    }

    public PlayerAction choosePayAction() {
        return new Pay(this);
    }
}
