package Explorer;

public class ComandoMoverAtras extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.moverAtras();
    }
}
