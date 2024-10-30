package Explorer;

public class ComandoAbrirEscotillaSuperior extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        try {
            explorer.abrirEscotillaSuperior();
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), e);
        }
    }
}
