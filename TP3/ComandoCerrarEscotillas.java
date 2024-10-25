package Explorer;

public class ComandoCerrarEscotillas extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.cerrarEscotillas();
    }
}