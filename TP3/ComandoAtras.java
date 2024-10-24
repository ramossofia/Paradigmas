public class ComandoAtras implements Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.moverAtras();
    }
}
