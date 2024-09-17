package tree;

import java.util.LinkedList;
import java.util.Queue;

public class NodoConHijos extends Nodo {
    private Nodo left;
    private Nodo right;

    public NodoConHijos(Object carga, Nodo left, Nodo right) {
        super(carga);
        this.left = left;
        this.right = right;
    }


    public Nodo atLeft(Nodo left) {
        return new NodoConHijos(carga, left, this.right);
    }


    public Nodo atRight(Nodo right) {
        return new NodoConHijos(carga, this.left, right);
    }

    public Nodo left() {
        return left;
    }


    public Nodo right() {
        return right;
    }


    public void dfs(LinkedList<Object> result) {
        result.add(carga);
        left.dfs(result);
        right.dfs(result);
    }


    public void addChildrenToQueue(Queue<Nodo> queue) {
        queue.add(left);
        queue.add(right);
    }
}
