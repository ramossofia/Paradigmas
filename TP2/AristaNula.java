package tree;

import java.util.LinkedList;
import java.util.Queue;

public class AristaNula extends Arista {
    private final String errorMessage;

    public AristaNula(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    @Override
    public void dfs(LinkedList<Object> result) {}

    @Override
    public void addChildrenToQueue(Queue<Arista> queue) {}

    @Override
    public Object getCarga() {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public Arista atLeft(Arista left) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public Arista atRight(Arista right) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public Arista left() {
        return this;
    }

    @Override
    public Arista right() {
        return this;
    }

    @Override
    protected void addToQueueIfNotNull(Queue<Arista> queue) {}
}
