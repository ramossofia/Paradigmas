public class Norte implements Direccion {

    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() + 1); // Mover hacia arriba en el eje Y
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() - 1); // Mover hacia abajo en el eje Y
    }

    @Override
    public Direccion rotarIzquierda() {
        return new Oeste(); // Rotar 90 grados a la izquierda desde el Norte da como resultado Oeste
    }

    @Override
    public Direccion rotarDerecha() {
        return new Este(); // Rotar 90 grados a la derecha desde el Norte da como resultado Este
    }

    @Override
    public String toString() {
        return "N";
    }
}
