package tree;

import java.util.LinkedList;
import java.util.Queue;

public class NodoConHijoDerecho extends Nodo {
    private Nodo right;

    public NodoConHijoDerecho(Object carga, Nodo right) {
        super(carga);
        this.right = right;
    }

    @Override
    public Nodo atLeft(Nodo left) {
        return new NodoConHijos(carga, left, this.right);
    }

    @Override
    public Nodo atRight(Nodo right) {
        return new NodoConHijos(carga, null, right);
    }

    @Override
    public Nodo left() {
        throw new UnsupportedOperationException("Nada a la siniestra!");
    }

    @Override
    public Nodo right() {
        return right;
    }

    @Override
    public void dfs(LinkedList<Object> result) {
        result.add(carga);
        right.dfs(result);
    }

    @Override
    public void addChildrenToQueue(Queue<Nodo> queue) {
        queue.add(right);
    }
}
