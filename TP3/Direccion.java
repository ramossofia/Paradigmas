package Explorer;

public abstract class Direccion {
    abstract void moverAdelante(Explorer explorer);
    abstract void moverAtras(Explorer explorer);
    abstract Direccion rotarIzquierda();
    abstract Direccion rotarDerecha();
}

