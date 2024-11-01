package Explorer;

public class MoverAdelante extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.moverAdelante();
    }

    @Override
    public boolean canHandle(char command) {
        return command == 'f';
    }
}
