public class TakeCard extends Action {
    private final boolean isPlacingCard;
    private final Card cardToPlace;

    public TakeCard() {
        this.isPlacingCard = false;
        this.cardToPlace = null;
    }

    @Override
    public GameStatus execute(GameStatus gameState) {
        GameInProgress game = (GameInProgress) gameState;
        Player player = game.getCurrentPlayer();

        if (isPlacingCard) {
            if (!player.getCards().contains(cardToPlace)) {
                throw new IllegalStateException("Player does not have the specified card to place.");
            }
            player.removeCard(cardToPlace);
            game.addCardToTable(cardToPlace);
        } else {
            Card card;
            if (game.hasCardsOnTable()) {
                card = game.takeCardFromTable();
                player.addTokens(card.getTokens());
                card.removeTokens(card.getTokens());
            } else {
                if (game.getDeck().isEmpty()) {
                    throw new IllegalStateException("The deck is empty. No card to take.");
                }
                card = game.getDeck().drawCard()
                        .orElseThrow(() -> new IllegalStateException("No card drawn from the deck."));
                game.getDeck().removeCard(card);
            }
            player.addCard(card);
        }

        return game;
    }
}
