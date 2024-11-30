import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Player {
    private final String name;
    private final int tokens;
    private final List<Integer> cards;
    private final boolean placedToken;

    public Player(String name, int tokens, List<Integer> cards, boolean placedToken) {
        this.name = name;
        this.tokens = tokens;
        this.cards = List.copyOf(cards);
        this.placedToken = placedToken;
    }

    public String getName() {
        return name;
    }

    public int getTokens() {
        return tokens;
    }

    public List<Integer> getCards() {
        return cards;
    }

    public boolean hasPlacedToken() {
        return placedToken;
    }

    public Player placeToken() {
        // Devuelve el jugador actual si no tiene tokens o ya colocó un token.
        if (tokens <= 0 || placedToken) {
            return this;
        }
        return new Player(name, tokens - 1, cards, true);
    }

    public Player addCard(int card) {
        // Devuelve una nueva instancia con la carta añadida.
        List<Integer> newCards = new ArrayList<>(cards);
        newCards.add(card);
        return new Player(name, tokens, newCards, placedToken);
    }

    public Player addTokens(int tokens) {
        // Devuelve una nueva instancia con los tokens añadidos.
        return new Player(name, this.tokens + tokens, cards, placedToken);
    }

    public int calculateScore() {
        List<Integer> sortedCards = new ArrayList<>(cards);
        Collections.sort(sortedCards);

        int negativePoints = 0;
        int seriesPoints = 0;

        // Procesamos las cartas para identificar secuencias
        boolean inSeries = false; // Bandera para verificar si estamos en una secuencia
        int seriesStart = sortedCards.get(0); // El comienzo de la secuencia

        for (int i = 0; i < sortedCards.size(); i++) {
            // Si no estamos en una secuencia, sumamos la carta aislada a los puntos negativos
            if (i == 0 || sortedCards.get(i) != sortedCards.get(i - 1) + 1) {
                if (inSeries) {
                    // Si antes había una secuencia, sumamos sus puntos
                    seriesPoints += sortedCards.get(i - 1);
                }
                negativePoints += sortedCards.get(i); // Es carta aislada
                inSeries = false; // No estamos en secuencia
            } else {
                if (!inSeries) {
                    // Iniciamos una nueva secuencia
                    seriesStart = sortedCards.get(i - 1);
                    inSeries = true;
                }
                seriesPoints += sortedCards.get(i); // Sumamos la carta a la secuencia
            }
        }

        // Si hemos terminado con una secuencia, la sumamos también
        if (inSeries) {
            seriesPoints += sortedCards.get(sortedCards.size() - 1);
        }

        // La puntuación final es la diferencia entre los puntos negativos y de la secuencia
        return -(negativePoints - seriesPoints) + tokens;
    }



    @Override
    public String toString() {
        return name + " tiene " + calculateScore() + " puntos.";
    }
}
