public class Pay implements PlayerAction {
    private final Player player;

    public Pay(Player player) {
        this.player = player;
    }

    @Override
    public void execute(Card card) {
        player.payToken();   // Deduct one token from the player
        card.addToken();     // Add one token to the card
    }
}
