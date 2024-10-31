package Explorer;

public abstract class Direccion {
    public abstract void moverAdelante(Explorer explorer);
    public abstract void moverAtras(Explorer explorer);
    public abstract Direccion rotarIzquierda();
    public abstract Direccion rotarDerecha();
    public abstract String getNombre();
}
