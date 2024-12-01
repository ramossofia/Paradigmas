
public class TakeCard extends Action {

    @Override
    public GameStatus execute(GameStatus gameState) {
        GameInProgress game = (GameInProgress) gameState;
        Player player = game.getCurrentPlayer();

        if (game.getDeck().isEmpty()) {
            return game.checkGameOver();
        }

        Card drawnCard = game.getDeck().drawCard().orElse(null);
        if (drawnCard == null) {
            return game;
        }

        player.addCard(drawnCard.getValue());
        System.out.println("Player before adding tokens: " + player.getTokens());

        player.addTokens(drawnCard.getTokens());

        return game.checkGameOver();
    }
}




