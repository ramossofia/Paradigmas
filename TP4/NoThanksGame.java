import java.util.*;

public class NoThanksGame {
    private final List<Player> players;
    private final Stack<Card> deck;
    private final List<Card> discardedCards;

    public NoThanksGame(List<Player> players, List<Card> deck) {
        this.players = players;
        this.deck = new Stack<>();
        this.deck.addAll(deck);
        Collections.shuffle(this.deck); // Mezclar cartas
        this.discardedCards = new ArrayList<>();
    }

    public void playRound(PlayerAction action) {
        if (deck.isEmpty()) {
            System.out.println("El mazo está vacío. Fin del juego.");
            return;
        }

        Card currentCard = deck.peek();
        action.execute(currentCard);

        if (action instanceof Take) {
            discardedCards.add(deck.pop());
        }
    }

    public boolean isGameOver() {
        return deck.isEmpty();
    }
}
