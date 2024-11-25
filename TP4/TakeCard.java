public class TakeCard extends Action {
    private Card card;

    public TakeCard(Player player, Card card) {
        super(player);
        this.card = card;
    }

    public Card getCard() {
        return card;
    }
}
