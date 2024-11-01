package Explorer;

public abstract class Comando {
    public abstract void ejecutar(Explorer explorer);

    public boolean canHandle(char command) {
        return false;
    }
}
