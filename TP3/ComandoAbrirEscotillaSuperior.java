package Explorer;

public class ComandoAbrirEscotillaSuperior extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        try {
            explorer.abrirEscotillaSuperior();
        } catch (Exception e) {
            // Handle the exception, e.g., log it or rethrow as a runtime exception
            System.err.println("Error opening the upper hatch: " + e.getMessage());
        }
    }
}
