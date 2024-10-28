package Explorer;

public class Explorer {
    private int x;
    private int y;
    private Direccion direccion;
    private boolean escotillaSuperiorAbierta;
    private boolean escotillaInferiorAbierta;

    public Explorer(int x, int y, Direccion direccion) {
        this.x = x;
        this.y = y;
        this.direccion = direccion;
        this.escotillaSuperiorAbierta = false;
        this.escotillaInferiorAbierta = false;
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
        if (escotillaSuperiorAbierta) {
            throw new Exception("No se puede abrir escotilla inferior con la superior abierta");
        }
        escotillaInferiorAbierta = true;
    }

    public void abrirEscotillaSuperior() throws Exception {
        if (escotillaInferiorAbierta) {
            throw new Exception("No se puede abrir escotilla superior con la inferior abierta");
        }
        escotillaSuperiorAbierta = true;
    }

    public void cerrarEscotillas() throws Exception {
        if (!escotillaSuperiorAbierta && !escotillaInferiorAbierta) {
            throw new Exception("No hay escotillas abiertas para cerrar");
        }
        escotillaSuperiorAbierta = false;
        escotillaInferiorAbierta = false;
    }

    public boolean isEscotillaSuperiorAbierta() {
        return escotillaSuperiorAbierta;
    }

    public boolean isEscotillaInferiorAbierta() {
        return escotillaInferiorAbierta;
    }

    public String getDireccion() {
        return direccion.getNombre();
    }

    public void aspirar() throws Exception {
        if (!escotillaSuperiorAbierta) {
            throw new Exception("No se puede aspirar sin abrir la escotilla superior");
        }
    }

    public void recogerMuestra() throws Exception {
        if (!escotillaInferiorAbierta) {
            throw new Exception("No se puede recoger muestra sin abrir la escotilla inferior");
        }
    }
}
