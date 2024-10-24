public class Sur implements Direccion {
    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() - 1);
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() + 1);
    }

    @Override
    public Direccion rotarIzquierda() {
        return new Este();
    }

    @Override
    public Direccion rotarDerecha() {
        return new Oeste();
    }
}
