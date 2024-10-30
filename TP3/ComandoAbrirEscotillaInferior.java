package Explorer;

public class ComandoAbrirEscotillaInferior extends Comando {
    @Override
    public void ejecutar(Explorer explorer) throws EscotillaException {
        explorer.abrirEscotillaInferior();
    }
}
