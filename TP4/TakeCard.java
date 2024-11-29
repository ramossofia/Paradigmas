public class TakeCard extends Action {
    @Override
    public void execute(GameInProgress game, Player player) {
        Card card = game.drawCard();
        player.addCard(card);
        player.addTokens(card.getTokens());
        game.nextPlayer(); // Pasa el turno al siguiente jugador
    }
}
