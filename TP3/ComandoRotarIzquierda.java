package Explorer;

public class ComandoRotarIzquierda extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.rotarIzquierda();
    }
}