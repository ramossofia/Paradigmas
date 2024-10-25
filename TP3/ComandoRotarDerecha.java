package Explorer;

public class ComandoRotarDerecha extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.rotarDerecha();
    }
}