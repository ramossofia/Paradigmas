// src/PlaceToken.java
public class PlaceToken implements Action {
    private Player player;

    public PlaceToken(Player player) {
        this.player = player;
    }

    @Override
    public Player getPlayer() {
        return player;
    }

    @Override
    public void execute(Player player, Game game) {
        if (player.hasPlayedThisTurn()) {
            throw new IllegalStateException(player.getName() + " has already played this turn.");
        }
        player.decrementTokens();  // Calls the decrementTokens method on the player
        player.setHasPlayedThisTurn(true); // Mark this player as played
    }
}
