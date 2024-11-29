import java.util.List;
import java.util.stream.Collectors;

public abstract class GameStatus {
    protected List<Player> players;
    protected List<Card> deck;
    protected int currentPlayerIndex;

    public GameStatus(List<Player> players, List<Integer> cardValues) {
        this.players = players;
        this.deck = cardValues.stream().map(value -> new Card(value)).collect(Collectors.toList());
        this.currentPlayerIndex = 0;

        // Calcular y asignar fichas iniciales en función del número de jugadores
        int initialTokens = calculateInitialTokens(players.size());
        for (Player player : players) {
            player.setTokens(initialTokens);
        }
    }

    // Método para calcular el número de fichas iniciales dependiendo del número de jugadores
    private int calculateInitialTokens(int numberOfPlayers) {
        if (numberOfPlayers >= 3 && numberOfPlayers <= 5) {
            return 11;  // 3 a 5 jugadores
        } else if (numberOfPlayers == 6) {
            return 8;  // 6 jugadores
        } else if (numberOfPlayers == 7) {
            return 7;  // 7 jugadores
        } else {
            throw new IllegalArgumentException("Número de jugadores no soportado");
        }
    }

    public List<Player> players() {
        return players;
    }

    public List<Card> deck() {
        return deck;
    }

    public int currentPlayerIndex() {
        return currentPlayerIndex;
    }

    // Verifica si el mazo está vacío, en cuyo caso cambia el estado a GameOver
    protected GameStatus checkGameOver() {
        if (deck.isEmpty()) {
            return new GameOver(players, deck, currentPlayerIndex);
        }
        return this;
    }

    public abstract GameStatus nextPlayer();

    public abstract Card drawCard();

    public abstract GameStatus placeToken();

    public abstract GameStatus addTokenToCard(int tokens);

    public GameStatus updateDeck(List<Card> newDeck) {
        if (newDeck.isEmpty()) {
            return new GameOver(players, deck, currentPlayerIndex);
        }
        this.deck = newDeck;
        return this;
    }

    public GameStatus updatePlayers(List<Player> newPlayers) {
        this.players = newPlayers;
        return this;
    }

    // Este método ahora se puede llamar para obtener los tokens iniciales en los tests
    public int getInitialTokens() {
        return calculateInitialTokens(players.size());
    }

}
