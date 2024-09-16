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

    @Override
    public Nodo atLeft(Nodo left) {
        return new NodoConHijos(carga, left, this.right);
    }

    @Override
    public Nodo atRight(Nodo right) {
        return new NodoConHijos(carga, this.left, right);
    }

    @Override
    public Nodo left() {
        return left;  // Polymorphism ensures this will be called correctly
    }

    @Override
    public Nodo right() {
        return right;  // Polymorphism ensures this will be called correctly
    }

    @Override
    public void dfs(LinkedList<Object> result) {
        result.add(carga);
        left.dfs(result);  // Delegate to left child
        right.dfs(result);  // Delegate to right child
    }

    @Override
    public void addChildrenToQueue(Queue<Nodo> queue) {
        queue.add(left);
        queue.add(right);
    }
}
