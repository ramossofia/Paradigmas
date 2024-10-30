package Explorer;

public abstract class Direccion {
    public abstract void moverAdelante(Explorer explorer);
    public abstract void moverAtras(Explorer explorer);
    public abstract Direccion rotarIzquierda();
    public abstract Direccion rotarDerecha();
    public abstract String getNombre();
    public abstract void abrirEscotillaInferior(Explorer explorer) throws EscotillaException ;
    public abstract void abrirEscotillaSuperior(Explorer explorer) throws EscotillaException ;
    public abstract void cerrarEscotillas(Explorer explorer) throws EscotillaException ;
    public abstract void aspirar(Explorer explorer) throws EscotillaException ;
    public abstract void recogerMuestra(Explorer explorer) throws EscotillaException ;
    public abstract boolean isEscotillaSuperiorAbierta();
    public abstract boolean isEscotillaInferiorAbierta();
    protected abstract void validarAbrirEscotillaSuperior() throws Exception;
    protected abstract void validarAbrirEscotillaInferior() throws Exception;
    protected abstract void validarCerrarEscotillas() throws Exception;
    protected abstract void validarAspirar() throws Exception;
    protected abstract void validarRecogerMuestra() throws Exception;
}
