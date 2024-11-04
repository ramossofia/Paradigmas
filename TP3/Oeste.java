package Explorer;

public class Oeste extends Cardinal {
    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() - 1, explorer.getY());
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() + 1, explorer.getY());
    }

    @Override
    public Cardinal rotarIzquierda() {
        return new Sur();
    }

    @Override
    public Cardinal rotarDerecha() {
        return new Norte();
    }

    @Override
    public String getNombre() {
        return "O";
    }
}
