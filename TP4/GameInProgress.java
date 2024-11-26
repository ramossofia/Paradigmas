public class GameInProgress extends GameStatus {
    private int currentPlayerIndex;

    public GameInProgress(NoThanks game) {
        super(game);
        this.currentPlayerIndex = 0;
    }

    @Override
    public void start() {
        System.out.println("Game started!");
        nextTurn();
    }

    @Override
    public void nextTurn() {
        if (getGame().getDeck().isEmpty()) {
            getGame().endGame();
            return;
        }
        Player currentPlayer = getGame().getPlayers().get(currentPlayerIndex);
        PlayerTurn playerTurn = new PlayerTurn(currentPlayer, getGame());
        playerTurn.execute();
        currentPlayerIndex = (currentPlayerIndex + 1) % getGame().getPlayers().size();
    }

    @Override
    public void end() {
        System.out.println("Game is in progress...");
    }
}
