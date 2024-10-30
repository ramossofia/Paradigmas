package Explorer;

public class ComandoCerrarEscotillas extends Comando {
    @Override
    public void ejecutar(Explorer explorer) {
        try {
            explorer.cerrarEscotillas();
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}
