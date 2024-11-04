package Explorer;

public class Norte extends Cardinal {

    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() + 1);
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() - 1);
    }

    @Override
    public Cardinal rotarIzquierda() {
        return new Oeste();
    }

    @Override
    public Cardinal rotarDerecha() {
        return new Este();
    }

    @Override
    public String getNombre() {
        return "N";
    }
}
