package Explorer;

public abstract class Cardinal {
    public abstract void moverAdelante(Explorer explorer);
    public abstract void moverAtras(Explorer explorer);
    public abstract Cardinal rotarIzquierda();
    public abstract Cardinal rotarDerecha();
    public abstract String getNombre();
}
