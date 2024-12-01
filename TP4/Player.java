
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Represents an immutable deck of cards.
 * All operations return a new Deck instance, adhering to functional programming principles.
 */
public class Deck {
    private List<Card> cards;

    /**
     * Constructs a new Deck from a list of card values.
     *
     * @param cardValues A list of integers representing the card values.
     */
    public Deck(List<Integer> cardValues) {
        this.cards = cardValues.stream()
                .map(value -> new Card(value, 0))
                .collect(Collectors.toUnmodifiableList());
    }

    private Deck(List<Card> cards, boolean isPrebuilt) {
        this.cards = List.copyOf(cards); // Ensure immutability
    }

    /**
     * Draws the top card of the deck, if available.
     *
     * @return An Optional containing the top Card if the deck is not empty, or Optional.empty() otherwise.
     */
    public Optional<Card> drawCard() {
        return cards.isEmpty() ? Optional.empty() : Optional.of(cards.get(0));
    }

    /**
     * Removes the top card from the deck and returns a new Deck instance without it.
     *
     * @return A new Deck instance with the top card removed.
     * @throws IllegalStateException If the deck is empty.
     */
    public Deck removeTopCard() {
        if (cards.isEmpty()) {
            throw new IllegalStateException("The deck is empty.");
        }
        return new Deck(cards.subList(1, cards.size()), true);
    }

    /**
     * Adds tokens to the top card of the deck and returns a new Deck instance with the updated card.
     *
     * @param tokens The number of tokens to add to the top card.
     * @return A new Deck instance with the updated top card.
     * @throws IllegalStateException If the deck is empty.
     */



    public void addTokensToTopCard(int tokens) {
        if (cards.isEmpty()) {
            throw new IllegalStateException("The deck is empty.");
        }
        cards.get(0).addTokens(1);
    }

    /**
     * Checks if the deck is empty.
     *
     * @return True if the deck is empty, false otherwise.
     */
    public boolean isEmpty() {
        return cards.isEmpty();
    }

    public int size() {
        return cards.size();
    }

    @Override
    public String toString() {
        return "Deck{" +
                "cards=" + cards +
                '}';
    }




}
