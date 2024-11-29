public class PlaceToken extends Action {
    @Override
    public void execute(GameInProgress game, Player player) {
        if (player.tokens() <= 0) {
            throw new IllegalStateException("El jugador no tiene fichas para colocar.");
        }
        player.placeToken();
        game.nextPlayer(); // Pasa el turno al siguiente jugador
    }
}
