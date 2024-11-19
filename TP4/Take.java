public class Take implements PlayerAction {
    private final Player player;

    public Take (Player player) {
        this.player = player;
    }

    @Override
    public void execute(Card card) {
        player.addScore(card.getValue());
        player.takeTokens(card.clearTokens());
    }
}