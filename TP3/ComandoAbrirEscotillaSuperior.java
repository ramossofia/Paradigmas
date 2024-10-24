public class ComandoAbrirEscotillaSuperior implements Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.abrirEscotillaSuperior();
    }
}
