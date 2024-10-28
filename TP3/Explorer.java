package Explorer;

public class Explorer {
    private int x;
    private int y;
    private Direccion direccion;

    public Explorer(int x, int y, Direccion direccion) {
        this.x = x;
        this.y = y;
        this.direccion = direccion;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public void actualizarPosicion(int x, int y) {
        this.x = x;
        this.y = y;
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

    public void abrirEscotillaInferior() throws Exception {
        direccion.abrirEscotillaInferior(this);
    }

    public void abrirEscotillaSuperior() throws Exception {
        direccion.abrirEscotillaSuperior(this);
    }

    public void cerrarEscotillas() throws Exception {
        direccion.cerrarEscotillas(this);
    }

    public boolean isEscotillaSuperiorAbierta() {
        return direccion.isEscotillaSuperiorAbierta();
    }

    public boolean isEscotillaInferiorAbierta() {
        return direccion.isEscotillaInferiorAbierta();
    }

    public String getDireccion() {
        return direccion.getNombre();
    }

    public void aspirar() throws Exception {
        direccion.aspirar(this);
    }

    public void recogerMuestra() throws Exception {
        direccion.recogerMuestra(this);
    }
}
