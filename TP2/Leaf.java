package tree;

import java.util.ArrayList;
import java.util.Queue;

public abstract class Leaf {
    public abstract void dfs(ArrayList<Object> result);
    public abstract void addChildrenToQueue(Queue<Leaf> queue);
    public abstract Object getCarga();
    public abstract Leaf addChildAt(int index, Leaf child);
    public abstract Leaf getChildAt(int index);
    protected abstract void addToQueueIfNotNull(Queue<Leaf> queue);
}
