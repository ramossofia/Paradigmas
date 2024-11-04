package Explorer;

public class Explorer {
    private int x;
    private int y;
    private Cardinal cardinal;
    private EstadoEscotilla estadoEscotilla;

    public Explorer(int x, int y, Cardinal cardinal) {
        this.x = x;
        this.y = y;
        this.cardinal = cardinal;
        this.estadoEscotilla = new EscotillasCerradas(this);
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public String getDireccion() {
        return cardinal.getNombre();
    }

    public void moverAdelante() {
        cardinal.moverAdelante(this);
    }

    public void moverAtras() {
        cardinal.moverAtras(this);
    }

    public void rotarIzquierda() {
        cardinal = cardinal.rotarIzquierda();
    }

    public void rotarDerecha() {
        cardinal = cardinal.rotarDerecha();
    }

    public void actualizarPosicion(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public void abrirEscotillaSuperior() {
        estadoEscotilla.abrirEscotillaSuperior();
    }

    public void abrirEscotillaInferior() {
        estadoEscotilla.abrirEscotillaInferior();
    }

    public void cerrarEscotillas() {
        estadoEscotilla.cerrarEscotillas();
    }

    public boolean isEscotillaSuperiorAbierta() {
        return estadoEscotilla.isEscotillaSuperiorAbierta();
    }

    public boolean isEscotillaInferiorAbierta() {
        return estadoEscotilla.isEscotillaInferiorAbierta();
    }

    public void setEstadoEscotilla(EstadoEscotilla estadoEscotilla) {
        this.estadoEscotilla = estadoEscotilla;
    }

    public void aspirar() {
        estadoEscotilla.aspirar();
    }

    public void recogerMuestra() {
        estadoEscotilla.recogerMuestra();
    }
}
