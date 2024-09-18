package tree;

import java.util.LinkedList;
import java.util.Queue;

public abstract class Arista {
    public abstract void dfs(LinkedList<Object> result);
    public abstract void addChildrenToQueue(Queue<Arista> queue);
    public abstract Object getCarga();
    public abstract Arista atLeft(Arista left);
    public abstract Arista atRight(Arista right);
    public abstract Arista left();
    public abstract Arista right();
    protected abstract void addToQueueIfNotNull(Queue<Arista> queue);
}
