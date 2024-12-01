public class PlaceToken extends Action {

    @Override
    public GameStatus execute(GameStatus gameState) {
        GameInProgress game = (GameInProgress) gameState;
        Player currentPlayer = game.getCurrentPlayer();

        if (currentPlayer.getTokens() <= 0) {
            return game.executeAction(new TakeCard());
        }

        if (game.hasCardsOnTable()) {
            game.addTokensToLastTableCard(1);
        }

        currentPlayer.removeTokens(1);
        game.nextPlayer();

        return game;
    }
}
