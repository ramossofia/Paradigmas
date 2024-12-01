public class PlaceToken extends Action {

    @Override
    public GameStatus execute(GameStatus gameState) {
        GameInProgress game = (GameInProgress) gameState;
        Player currentPlayer = game.getCurrentPlayer();

        // Check if the player has no tokens left, and force them to take a card
        if (currentPlayer.getTokens() <= 0) {
            // Execute TakeCard action if the player has no tokens left
            return game.executeAction(new TakeCard());
        }

        if (game.hasCardsOnTable()) {
            game.addTokensToLastTableCard(1); // Add a token to the last card on the table
        }

        currentPlayer.removeTokens(1); // Remove one token from the player
        game.nextPlayer(); // Move to the next player

        return game;
    }
}
