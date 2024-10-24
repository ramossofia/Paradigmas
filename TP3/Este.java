public class Este implements Direccion {
    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() + 1, explorer.getY());
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() - 1, explorer.getY());
    }

    @Override
    public Direccion rotarIzquierda() {
        return new Norte();
    }

    @Override
    public Direccion rotarDerecha() {
        return new Sur();
    }
}
