// src/GameOver.java
import java.util.List;

public class GameOver extends GameStatus {

    public GameOver(List<Player> players, Deck deck) {
        super(players, deck);
    }

    @Override
    public GameStatus nextPlayer() {
        // No hacer nada, el juego ha terminado
        return this;
    }

    @Override
    public GameStatus executeAction(Action action) {
        // No hacer nada, el juego ha terminado
        return this;
    }

    public int calculateInitialTokens(int playerCount) {
        // No hacer nada, el juego ha terminado
        return 0;
    }

    @Override
    public List<Player> getPlayers() {
        // No hacer nada, el juego ha terminado
        return super.getPlayers();
    }

    public Deck getDeck() {
        // No hacer nada, el juego ha terminado
        return super.getDeck();
    }

    @Override
    public int getCurrentPlayerIndex() {
        // No hacer nada, el juego ha terminado
        return super.getCurrentPlayerIndex();
    }
}
