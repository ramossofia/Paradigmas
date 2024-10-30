package Explorer;

public class ComandoAbrirEscotillaSuperior extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.abrirEscotillaSuperior();
    }
}
