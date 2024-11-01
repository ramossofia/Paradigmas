package Explorer;

public class MoverAtras extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.moverAtras();
    }

    @Override
    public boolean canHandle(char command) {
        return command == 'b';
    }
}
