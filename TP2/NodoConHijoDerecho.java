package tree;

import java.util.LinkedList;
import java.util.Queue;

public class AristaExistente extends Arista {
    private Object carga;
    private Arista left;
    private Arista right;

    public AristaExistente(Object carga) {
        this.carga = carga;
        this.left = new AristaNula("Nada a la siniestra!");
        this.right = new AristaNula("Nada a la diestra!");
    }

    @Override
    public void dfs(LinkedList<Object> result) {
        result.add(carga);
        left.dfs(result);
        right.dfs(result);
    }

    @Override
    public void addChildrenToQueue(Queue<Arista> queue) {
        left.addToQueueIfNotNull(queue);
        right.addToQueueIfNotNull(queue);
    }

    @Override
    public Object getCarga() {
        return carga;
    }

    @Override
    public Arista atLeft(Arista left) {
        this.left = left;
        return this;
    }

    @Override
    public Arista atRight(Arista right) {
        this.right = right;
        return this;
    }

    @Override
    public Arista left() {
        return left;
    }

    @Override
    public Arista right() {
        return right;
    }

    @Override
    protected void addToQueueIfNotNull(Queue<Arista> queue) {
        queue.add(this);
    }
}
