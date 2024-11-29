import java.util.List;

public class GameInProgress extends GameStatus {
    private int tokens; // Add this field

    public GameInProgress(List<Player> players, List<Integer> cardValues) {
        super(players, cardValues);
        this.tokens = 0; // Initialize tokens

        // Eliminar las primeras 9 cartas, asumiendo que el mazo tiene al menos 9 cartas
        if (deck.size() > 9) {
            deck = deck.subList(9, deck.size());
        }
    }

    @Override
    public GameStatus nextPlayer() {
        currentPlayerIndex = (currentPlayerIndex + 1) % players.size();
        players.get(currentPlayerIndex).resetPlacedToken();
        return checkGameOver(); // Verifica si el juego debe terminar después del turno
    }

    @Override
    public Card drawCard() {
        if (deck.isEmpty()) {
            throw new IllegalStateException("El mazo está vacío.");
        }
        Card card = deck.remove(0);
        checkGameOver(); // Verifica si el juego debe terminar
        return card;
    }

    @Override
    public GameStatus placeToken() {
        Player currentPlayer = players.get(currentPlayerIndex);
        if (currentPlayer.hasPlacedToken()) {
            throw new IllegalStateException("Cannot place more than one token per turn.");
        }
        currentPlayer.placeToken();
        deck.get(0).addTokens(1); // Asegúrate de que se está sumando un token a la carta
        return nextPlayer(); // Pasa el turno al siguiente jugador
    }

    @Override
    public GameStatus addTokenToCard(int tokens) {
        if (deck.isEmpty()) {
            throw new IllegalStateException("No hay cartas para agregar fichas.");
        }
        deck.get(0).addTokens(tokens);
        return this;
    }

    @Override
    protected GameStatus checkGameOver() {
        if (deck.isEmpty()) {
            return new GameOver(players, deck, currentPlayerIndex);
        }
        return this;
    }

    public GameStatus executeAction(Action action) {
        action.execute(this, players.get(currentPlayerIndex));
        return this;
    }

    public void addTokens(int tokens) {
        this.tokens += tokens;
    }
}
