package Explorer;

public class RotarIzquierda extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.rotarIzquierda();
    }

    @Override
    public boolean canHandle(char command) {
        return command == 'l';
    }
}
