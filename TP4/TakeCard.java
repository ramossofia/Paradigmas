public class TakeCard extends Action {
    @Override
    public GameStatus execute(GameInProgress game, Player player) {
        // If the deck is empty, check if the game should end
        if (game.getDeck().isEmpty()) {
            return game.checkGameOver();
        }

        // If the player has no tokens, they must take the card
        if (player.getTokens() == 0) {
            // Draw a card unconditionally
            Card drawnCard = game.getDeck().drawCard().orElse(null);
            if (drawnCard == null) {
                return game; // If no card to draw, return the same state
            }

            // Update the player
            Player updatedPlayer = player
                    .addCard(drawnCard.getValue())  // Assuming adding a card modifies the player
                    .addTokens(drawnCard.getTokens());  // Add the tokens from the card

            // Update the deck
            Deck updatedDeck = game.getDeck().addTokensToTopCard(-drawnCard.getTokens());  // Modify the deck

            // Return the new game state
            return game.withUpdatedPlayer(updatedPlayer)
                    .withUpdatedDeck(updatedDeck)
                    .nextPlayer()
                    .checkGameOver();
        }

        // If the player has tokens, normal behavior
        Card drawnCard = game.getDeck().drawCard().orElse(null);
        if (drawnCard == null) {
            return game;
        }

        // Update the player with the card
        Player updatedPlayer = player
                .addCard(drawnCard.getValue())
                .addTokens(drawnCard.getTokens());

        // Update the deck
        Deck updatedDeck = game.getDeck().addTokensToTopCard(-drawnCard.getTokens());

        // Return the updated state
        return game.withUpdatedPlayer(updatedPlayer)
                .withUpdatedDeck(updatedDeck)
                .nextPlayer()
                .checkGameOver();
    }
}
