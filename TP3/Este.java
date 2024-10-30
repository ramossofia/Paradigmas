package Explorer;

public class Este extends Direccion {
    private boolean escotillaSuperiorAbierta = false;
    private boolean escotillaInferiorAbierta = false;

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
    public void aspirar(Explorer explorer) throws Exception {
        validarAspirar();
    }

    @Override
    public void recogerMuestra(Explorer explorer) throws Exception {
        validarRecogerMuestra();
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
        // No validation needed
    }

    @Override
    protected void validarAbrirEscotillaInferior() throws Exception {
        // No validation needed
    }

    @Override
    protected void validarCerrarEscotillas() throws Exception {
        // No validation needed
    }

    @Override
    protected void validarAspirar() throws Exception {
        // No validation needed
    }

    @Override
    protected void validarRecogerMuestra() throws Exception {
        // No validation needed
    }
}
