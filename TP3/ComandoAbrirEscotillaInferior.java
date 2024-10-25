package Explorer;

public class ComandoAbrirEscotillaInferior extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        try {
            explorer.abrirEscotillaInferior();
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}
