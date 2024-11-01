package Explorer;

public class AbrirEscotillaSuperior extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.abrirEscotillaSuperior();
    }

    @Override
    public boolean canHandle(char command) {
        return command == 'O';
    }
}
