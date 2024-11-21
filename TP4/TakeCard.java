public class TakeCard implements Action {
    private Player player;
    private Card card;

    public TakeCard(Player player, Card card) {
        this.player = player;
        this.card = card;
    }

    @Override
    public Player getPlayer() {
        return player;
    }

    @Override
    public void execute(Player player, Game game) {
        player.takeCard(card, card.getTokens());
        card.resetTokens();
    }
}
