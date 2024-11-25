
public class PlayerTurn {
    private Player player;
    private NoThanks game;

    public PlayerTurn(Player player, NoThanks game) {
        this.player = player;
        this.game = game;
    }

    public Player getPlayer() {
        return player;
    }

    public void execute() {
        game.checkAndForceTakeCard();
    }
}
