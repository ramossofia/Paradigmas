import java.util.List;
import java.util.ArrayList;

public class GameOver extends GameStatus {

    public GameOver(List<Player> players, List<Card> deck, int currentPlayerIndex) {
        super(players, new ArrayList<>()); // El mazo está vacío en GameOver
        this.currentPlayerIndex = currentPlayerIndex;
    }

    @Override
    public GameStatus nextPlayer() {
        throw new IllegalStateException("El juego ha terminado.");
    }

    @Override
    public Card drawCard() {
        throw new IllegalStateException("No se pueden robar cartas, el juego ha terminado.");
    }

    @Override
    public GameStatus placeToken() {
        throw new IllegalStateException("No se pueden colocar fichas, el juego ha terminado.");
    }

    @Override
    public GameStatus addTokenToCard(int tokens) {
        throw new IllegalStateException("No se pueden agregar fichas, el juego ha terminado.");
    }

    @Override
    public GameStatus updateDeck(List<Card> deck) {
        throw new IllegalStateException("No se puede actualizar el mazo, el juego ha terminado.");
    }
}
