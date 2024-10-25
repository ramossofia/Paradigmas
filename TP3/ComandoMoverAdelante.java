package Explorer;

public class ComandoMoverAdelante extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.moverAdelante();
    }
}
