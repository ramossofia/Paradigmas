public class ComandoAdelante implements Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.moverAdelante();
    }
}
