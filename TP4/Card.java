public class Card {
    private int value;
    private int tokens;

    public Card(int value) {
        this.value = value;
        this.tokens = 0; // Inicializa los tokens de la carta en 0
    }


    public int getTokens() {
        return tokens;
    }

    public void addTokens(int tokens) {
        this.tokens += tokens;
    }

    public int getValue() {
        return value;
    }
}
