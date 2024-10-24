public class ComandoRotacionHaciaDerecha implements Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        explorer.rotarDerecha();
    }
}
