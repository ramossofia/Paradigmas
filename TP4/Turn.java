// src/Turn.java
import java.util.*;

public class Turn {
    private List<Player> players;
    private int currentIndex = 0;

    public Turn(List<Player> players) {
        this.players = players;
    }

    public Player getCurrentPlayer() {
        return players.get(currentIndex);
    }

    public void advanceToNext() {
        currentIndex = (currentIndex + 1) % players.size();
    }
}
