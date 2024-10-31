package Explorer;

public class Este extends Direccion {
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

    @Override
    public String getNombre() {
        return "E";
    }
}
