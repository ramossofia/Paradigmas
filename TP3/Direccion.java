public interface Direccion {
    void moverAdelante(Explorer explorer);
    void moverAtras(Explorer explorer);
    Direccion rotarIzquierda();
    Direccion rotarDerecha();
}
