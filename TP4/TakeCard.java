// src/TakeCard.java
public class TakeCard implements Action {

    @Override
    public GameStatus execute(GameStatus gameState) {
        GameInProgress game = (GameInProgress) gameState;
        Player player = game.getCurrentPlayer();

        // If the deck is empty, check if the game should end
        if (game.getDeck().isEmpty()) {
            return game.checkGameOver();
        }

        // Draw the top card from the deck
        Card drawnCard = game.getDeck().drawCard().orElse(null);
        if (drawnCard == null) {
            return game; // If no card to draw, return the same state
        }

        // Update the player with the card and tokens
        player.addCard(drawnCard.getValue());
        player.addTokens(drawnCard.getTokens());

        // Remove the top card from the deck
        Deck updatedDeck = game.getDeck().removeTopCard();

        // Return the updated state
        return game.withUpdatedPlayer(player)
                .withUpdatedDeck(updatedDeck)
                .nextPlayer()
                .checkGameOver();
    }
}
