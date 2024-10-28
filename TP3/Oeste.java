package Explorer;

public class Oeste extends Direccion {
    private boolean escotillaSuperiorAbierta = false;
    private boolean escotillaInferiorAbierta = false;

    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() - 1, explorer.getY());
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() + 1, explorer.getY());
    }

    @Override
    public Direccion rotarIzquierda() {
        return new Sur();
    }

    @Override
    public Direccion rotarDerecha() {
        return new Norte();
    }

    @Override
    public String getNombre() {
        return "O";
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
        validarCerrarEscotillas();
        escotillaSuperiorAbierta = false;
        escotillaInferiorAbierta = false;
    }

    @Override
    protected void validarAbrirEscotillaSuperior() {}

    @Override
    protected void validarAbrirEscotillaInferior() {}

    @Override
    protected void validarCerrarEscotillas() {}

    @Override
    public void aspirar(Explorer explorer) throws Exception {
        validarAspirar();
    }

    @Override
    protected void validarAspirar() {}

    @Override
    public void recogerMuestra(Explorer explorer) throws Exception {
        validarRecogerMuestra();
    }

    @Override
    protected void validarRecogerMuestra() {}

    @Override
    public boolean isEscotillaSuperiorAbierta() {
        return escotillaSuperiorAbierta;
    }

    @Override
    public boolean isEscotillaInferiorAbierta() {
        return escotillaInferiorAbierta;
    }
}
