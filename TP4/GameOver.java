public class GameOver extends GameStatus {
    public GameOver(NoThanks game) {
        super(game);
    }

    @Override
    public void start() {
        throw new IllegalStateException("Game is already over.");
    }

    @Override
    public void nextTurn() {
        throw new IllegalStateException("Game is already over.");
    }

    @Override
    public void end() {
        System.out.println("Game over!");
        for (Player player : getGame().getPlayers()) {
            System.out.println(player.getName() + " has " + player.calculatePoints() + " points.");
        }
    }
}
