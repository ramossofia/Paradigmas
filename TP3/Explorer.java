public class Explorer {
    private int x, y;
    private Direccion direccion;
    private boolean escotillaSuperiorAbierta = false;
    private boolean escotillaInferiorAbierta = false;

    public Explorer(int x, int y, Direccion direccion) {
        this.x = x;
        this.y = y;
        this.direccion = direccion;
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

    // Abre la escotilla superior, pero lanza una excepción si la inferior está abierta
    public void abrirEscotillaSuperior() {
        if (escotillaSuperiorAbierta) throw new IllegalStateException("La escotilla superior ya está abierta");
        if (escotillaInferiorAbierta) throw new IllegalStateException("No se puede abrir la escotilla superior con la inferior abierta");
        escotillaSuperiorAbierta = true;
    }

    // Abre la escotilla inferior, pero lanza una excepción si la superior está abierta
    public void abrirEscotillaInferior() {
        if (escotillaInferiorAbierta) throw new IllegalStateException("La escotilla inferior ya está abierta");
        if (escotillaSuperiorAbierta) throw new IllegalStateException("No se puede abrir la escotilla inferior con la superior abierta");
        escotillaInferiorAbierta = true;
    }

    // Cierra ambas escotillas
    public void cerrarEscotillas() {
        if (!escotillaSuperiorAbierta && !escotillaInferiorAbierta)
            throw new IllegalStateException("No hay escotillas abiertas para cerrar");
        escotillaSuperiorAbierta = false;
        escotillaInferiorAbierta = false;
    }

    public void actualizarPosicion(int nuevoX, int nuevoY) {
        this.x = nuevoX;
        this.y = nuevoY;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public boolean isEscotillaSuperiorAbierta() {
        return escotillaSuperiorAbierta;
    }

    public boolean isEscotillaInferiorAbierta() {
        return escotillaInferiorAbierta;
    }

    public String getDireccion() {
        return direccion.toString();  // Asume que Direccion tiene un método toString adecuado
    }
}
