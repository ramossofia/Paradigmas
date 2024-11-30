public class PlaceToken extends Action {

    @Override
    public GameStatus execute(GameInProgress game, Player player) {
        // Si el jugador ya colocó un token, simplemente retornamos el estado actual.
        if (player.hasPlacedToken()) {
            return game;
        }

        // Actualizar el jugador con un token colocado.
        Player updatedPlayer = player.placeToken();

        // Agregar un token a la carta superior del mazo.
        Deck updatedDeck = game.getDeck().addTokensToTopCard(1);

        // Crear un nuevo estado del juego con el jugador y mazo actualizados, y avanzar al siguiente jugador.
        return game.withUpdatedPlayer(updatedPlayer).withUpdatedDeck(updatedDeck).nextPlayer();
    }
}
