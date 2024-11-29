public class PlayerTurn {
    private final GameInProgress game;

    public PlayerTurn(GameInProgress game) {
        this.game = game;
    }

    public void playAction(Action action) {
        game.executeAction(action);
    }
}
