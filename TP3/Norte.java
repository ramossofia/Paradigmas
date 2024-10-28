package Explorer;

public class Norte extends Direccion {
    private boolean escotillaSuperiorAbierta = false;
    private boolean escotillaInferiorAbierta = false;

    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() + 1);
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() - 1);
    }

    @Override
    public Direccion rotarIzquierda() {
        return new Oeste();
    }

    @Override
    public Direccion rotarDerecha() {
        return new Este();
    }

    @Override
    public String getNombre() {
        return "N";
    }

    @Override
    public void abrirEscotillaSuperior(Explorer explorer) throws Exception {
        validarAbrirEscotillaSuperior();
        escotillaSuperiorAbierta = true;
    }

    @Override
    public void abrirEscotillaInferior(Explorer explorer) throws Exception {
        validarAbrirEscotillaInferior();
        escotillaInferiorAbierta = true;
    }

    @Override
    public void cerrarEscotillas(Explorer explorer) throws Exception {
        if (!escotillaSuperiorAbierta && !escotillaInferiorAbierta) {
            throw new Exception("No hay escotillas abiertas para cerrar");
        }
        escotillaSuperiorAbierta = false;
        escotillaInferiorAbierta = false;
    }

    @Override
    public void aspirar(Explorer explorer) throws Exception {
        if (!escotillaSuperiorAbierta) {
            throw new Exception("No se puede aspirar sin abrir la escotilla superior");
        }
    }

    @Override
    public void recogerMuestra(Explorer explorer) throws Exception {
        if (!escotillaInferiorAbierta) {
            throw new Exception("No se puede recoger muestra sin abrir la escotilla inferior");
        }
    }

    @Override
    public boolean isEscotillaSuperiorAbierta() {
        return escotillaSuperiorAbierta;
    }

    @Override
    public boolean isEscotillaInferiorAbierta() {
        return escotillaInferiorAbierta;
    }

    @Override
    protected void validarAbrirEscotillaSuperior() throws Exception {
        if (escotillaInferiorAbierta) {
            throw new Exception("No se puede abrir escotilla superior con la inferior abierta");
        }
    }

    @Override
    protected void validarAbrirEscotillaInferior() throws Exception {
        if (escotillaSuperiorAbierta) {
            throw new Exception("No se puede abrir escotilla inferior con la superior abierta");
        }
    }

    @Override
    protected void validarCerrarEscotillas() {}

    @Override
    protected void validarAspirar() {}

    @Override
    protected void validarRecogerMuestra() {}
}
