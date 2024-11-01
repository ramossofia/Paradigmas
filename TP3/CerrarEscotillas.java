package Explorer;

public class CerrarEscotillas extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.cerrarEscotillas();
    }

    @Override
    public boolean canHandle(char command) {
        return command == 'c';
    }
}
