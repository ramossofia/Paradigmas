public class PlaceToken implements Action {

    @Override
    public GameStatus execute(GameStatus gameState) {
        GameInProgress game = (GameInProgress) gameState;
        Player currentPlayer = game.getCurrentPlayer();
        currentPlayer.addTokens(-1);
        return game.withUpdatedPlayer(currentPlayer).nextPlayer();
    }
}
