package Explorer;

public class Este extends Cardinal {
    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() + 1, explorer.getY());
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() - 1, explorer.getY());
    }

    @Override
    public Cardinal rotarIzquierda() {
        return new Norte();
    }

    @Override
    public Cardinal rotarDerecha() {
        return new Sur();
    }

    @Override
    public String getNombre() {
        return "E";
    }
}
