package Explorer;

public class RotarDerecha extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.rotarDerecha();
    }

    @Override
    public boolean canHandle(char command) {
        return command == 'r';
    }
}
