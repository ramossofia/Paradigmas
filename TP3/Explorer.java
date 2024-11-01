package Explorer;

public class Explorer {
    private int x;
    private int y;
    private Direccion direccion;
    private EstadoEscotilla estadoEscotilla;

    public Explorer(int x, int y, Direccion direccion) {
        this.x = x;
        this.y = y;
        this.direccion = direccion;
        this.estadoEscotilla = new EscotillasCerradas(this);
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public String getDireccion() {
        return direccion.getNombre();
    }

    public void moverAdelante() {
        direccion.moverAdelante(this);
    }

    public void moverAtras() {
        direccion.moverAtras(this);
    }

    public void rotarIzquierda() {
        direccion = direccion.rotarIzquierda();
    }

    public void rotarDerecha() {
        direccion = direccion.rotarDerecha();
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
