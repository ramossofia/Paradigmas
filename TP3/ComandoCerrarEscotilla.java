public class ComandoCerrarEscotilla implements Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.cerrarEscotillas();
    }
}
