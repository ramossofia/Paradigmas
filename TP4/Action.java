public interface Action {
    Player getPlayer();
    void execute(Player player, Game game);
}
