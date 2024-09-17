package tree;

import java.util.LinkedList;
import java.util.Queue;

public abstract class Nodo {

    protected Object carga;

    public Nodo(Object carga) {
        this.carga = carga;
    }

    public Object getCarga() {

        return carga;
    }

    public abstract Nodo atLeft(Nodo left);
    public abstract Nodo atRight(Nodo right);
    public abstract Nodo left();
    public abstract Nodo right();
    public abstract void dfs(LinkedList<Object> result);
    public abstract void addChildrenToQueue(Queue<Nodo> queue);
}
