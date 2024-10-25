package Explorer;

public class ComandoAbrirEscotillaInferior extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        try {
            explorer.abrirEscotillaInferior();
        } catch (IllegalStateException e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}