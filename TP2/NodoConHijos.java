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
        return left;
    }

    @Override
    public Nodo right() {
        return right;
    }

    @Override
    public void dfs(LinkedList<Object> result) {
        result.add(carga);
        if (left != null) {
            left.dfs(result);
        }
        if (right != null) {
            right.dfs(result);
        }
    }

    @Override
    public void addChildrenToQueue(Queue<Nodo> queue) {
        if (left != null) {
            queue.add(left);
        }
        if (right != null) {
            queue.add(right);
        }
    }
}
