package Explorer;

public class AbrirEscotillaInferior extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.abrirEscotillaInferior();
    }

    @Override
    public boolean canHandle(char command) {
        return command == 'o';
    }
}
