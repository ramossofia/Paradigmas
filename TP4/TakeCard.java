public class TakeCard extends Action {

    @Override
    public GameStatus execute(GameStatus gameState) {
        GameInProgress game = (GameInProgress) gameState;
        Player player = game.getCurrentPlayer();

        Card card;
        if (game.hasCardsOnTable()) {
            card = game.takeCardFromTable();
            player.addTokens(card.getTokens());
            card.removeTokens(card.getTokens());
        } else {
            card = game.getDeck().drawCard()
                    .orElseThrow(() -> new IllegalStateException("The deck is empty. No card to take."));
            game.getDeck().removeCard(card);
        }
        player.addCard(card);
        return game;
    }
}
