package Explorer;

public abstract class Direccion {
    public abstract void moverAdelante(Explorer explorer);
    public abstract void moverAtras(Explorer explorer);
    public abstract Direccion rotarIzquierda();
    public abstract Direccion rotarDerecha();
    public abstract String getNombre();
    public abstract void abrirEscotillaInferior(Explorer explorer);
    public abstract void abrirEscotillaSuperior(Explorer explorer);
    public abstract void cerrarEscotillas(Explorer explorer);
    public abstract void aspirar(Explorer explorer);
    public abstract void recogerMuestra(Explorer explorer);
    public abstract boolean isEscotillaSuperiorAbierta();
    public abstract boolean isEscotillaInferiorAbierta();
    protected abstract void validarAbrirEscotillaSuperior();
    protected abstract void validarAbrirEscotillaInferior();
    protected abstract void validarCerrarEscotillas();
    protected abstract void validarAspirar();
    protected abstract void validarRecogerMuestra();
}
