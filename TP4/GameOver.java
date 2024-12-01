
import java.util.List;

public class GameOver extends GameStatus {

    public GameOver(List<Player> players, Deck deck) {
        super(List.copyOf(players), deck, -1);
    }

    @Override
    public void nextPlayer() {
    }

    @Override
    public GameStatus executeAction(Action action) {
        return this;
    }

    @Override
    public GameStatus checkGameOver() {
        return this;
    }

}
