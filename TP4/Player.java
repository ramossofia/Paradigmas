import java.util.Set;
import java.util.TreeSet;

public class Player {
    private final String name;
    private int tokens;
    private final Set<Card> cards; // Usamos TreeSet para mantener las cartas ordenadas

    public Player(String name) {
        this.name = name;
        this.tokens = 0;
        this.cards = new TreeSet<>((c1, c2) -> Integer.compare(c1.getValue(), c2.getValue())); // Ordenar por valor
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

    public Set<Card> getCards() {
        return new TreeSet<>(cards); // Devolvemos una copia para evitar modificaciones externas
    }

    public void addCard(Card card) {
        cards.add(card);
    }

    public void removeTokens(int amount) {
        if (tokens < amount) {
            throw new IllegalStateException("Insufficient tokens.");
        }
        this.tokens -= amount;
    }

    public void addTokens(int amount) {
        this.tokens += amount;
    }

    public void removeCard(Card card) {
        if (!cards.remove(card)) {
            throw new IllegalStateException("The card is not in the player's collection.");
        }
    }

    public int calculateScore() {
        int score = tokens;
        if (!cards.isEmpty()) {
            int minCardValue = cards.stream().mapToInt(Card::getValue).min().orElse(0);
            score -= minCardValue;
        }
        return score;
    }
}
