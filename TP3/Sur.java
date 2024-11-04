package Explorer;

public class Sur extends Cardinal {
    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() - 1);
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() + 1);
    }

    @Override
    public Cardinal rotarIzquierda() {
        return new Este();
    }

    @Override
    public Cardinal rotarDerecha() {
        return new Oeste();
    }

    @Override
    public String getNombre() {
        return "S";
    }
}
