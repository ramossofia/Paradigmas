// src/GameInProgress.java
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class GameInProgress extends GameStatus {

    public GameInProgress(List<Player> players, Deck deck) {
        super(initializePlayers(players), deck, 0);
    }

    public GameInProgress(List<Player> players, Deck deck, int currentPlayerIndex) {
        super(initializePlayers(players), deck, currentPlayerIndex);
    }

    private static List<Player> initializePlayers(List<Player> players) {
        int initialTokens = calculateInitialTokens(players.size());
        players.forEach(player -> player.setTokens(initialTokens));
        return players;
    }

    public static int calculateInitialTokens(int numberOfPlayers) {
        if (numberOfPlayers <= 5) {
            return 11;
        } else if (numberOfPlayers == 6) {
            return 9;
        } else {
            return 7;
        }
    }

    @Override
    public GameStatus nextPlayer() {
        int newCurrentPlayerIndex = (getCurrentPlayerIndex() + 1) % getPlayers().size();
        return new GameInProgress(getPlayers(), getDeck(), newCurrentPlayerIndex).checkGameOver();
    }

    @Override
    public GameStatus executeAction(Action action) {
        GameStatus newState = action.execute(this);
        return newState.checkGameOver();
    }

    public GameInProgress withUpdatedPlayer(Player updatedPlayer) {
        List<Player> updatedPlayers = IntStream.range(0, getPlayers().size())
                .mapToObj(index -> index == getCurrentPlayerIndex() ? updatedPlayer : getPlayers().get(index))
                .collect(Collectors.toList());
        return new GameInProgress(updatedPlayers, getDeck(), getCurrentPlayerIndex());
    }

    public GameInProgress withUpdatedDeck(Deck updatedDeck) {
        return new GameInProgress(getPlayers(), updatedDeck, getCurrentPlayerIndex());
    }

    @Override
    public GameStatus checkGameOver() {
        if (getDeck().isEmpty()) {
            return new GameOver(getPlayers(), getDeck());
        }
        return this;
    }

    @Override
    public String toString() {
        return "GameInProgress{" +
                "players=" + getPlayers() +
                ", deck=" + getDeck() +
                ", currentPlayerIndex=" + getCurrentPlayerIndex() +
                '}';
    }


}
