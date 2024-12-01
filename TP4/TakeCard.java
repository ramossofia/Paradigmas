// src/TakeCard.java
public class TakeCard implements Action {

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
        player.addTokens(drawnCard.getTokens());
        Deck updatedDeck = game.getDeck().removeTopCard();

        game.withUpdatedPlayer(player).withUpdatedDeck(updatedDeck).nextPlayer();
        return game.checkGameOver();
    }
}
