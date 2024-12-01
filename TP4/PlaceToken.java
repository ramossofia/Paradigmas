public class PlaceToken extends Action {

    @Override
    public GameStatus execute(GameStatus gameState) {
        GameInProgress game = (GameInProgress) gameState;
        Player currentPlayer = game.getCurrentPlayer();
        Deck deck = game.getDeck();

        if (currentPlayer.getTokens() <= 0) {
            throw new IllegalStateException("Player has no tokens to place.");
        }

        deck.addTokensToTopCard(1);

        currentPlayer.removeToken();

        return game;
    }
}
