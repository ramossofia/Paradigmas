package Explorer;

public class Oeste extends Direccion {

    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() - 1, explorer.getY()); // Mover hacia la izquierda en el eje X
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() + 1, explorer.getY()); // Mover hacia la derecha en el eje X
    }

    @Override
    public Direccion rotarIzquierda() {
        return new Sur(); // Rotar 90 grados a la izquierda desde Oeste da como resultado Sur
    }

    @Override
    public Direccion rotarDerecha() {
        return new Norte(); // Rotar 90 grados a la derecha desde Oeste da como resultado Norte
    }

    @Override
    public String toString() {
        return "O";
    }
}
