package tree;

import java.util.LinkedList;
import java.util.Queue;

public class NodoConHijoDerecho extends Nodo {
    private Nodo right;

    public NodoConHijoDerecho(Object carga, Nodo right) {
        super(carga);
        this.right = right;
    }

    public Nodo atLeft(Nodo left) {
        return new NodoConHijos(carga, left, this.right);
    }


    public Nodo atRight(Nodo right) {
        return new NodoConHijos(carga, null, right);
    }

    public Nodo left() {
        throw new UnsupportedOperationException("Nada a la siniestra!");
    }


    public Nodo right() {
        return right;
    }


    public void dfs(LinkedList<Object> result) {
        result.add(carga);
        right.dfs(result);
    }


    public void addChildrenToQueue(Queue<Nodo> queue) {
        queue.add(right);
    }
}
