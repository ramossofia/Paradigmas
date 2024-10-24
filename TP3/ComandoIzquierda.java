public class ComandoIzquierda implements Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.rotarIzquierda();
    }
}
