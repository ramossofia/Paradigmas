package tree;

import java.util.ArrayList;
import java.util.Queue;

public class NullLeaf extends Leaf {
    private final String errorMessage;

    public NullLeaf(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    @Override
    public void dfs(ArrayList<Object> result) {}

    @Override
    public void addChildrenToQueue(Queue<Leaf> queue) {}

    @Override
    public Object getCarga() {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public Leaf addChildAt(int index, Leaf child) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public Leaf getChildAt(int index) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    protected void addToQueueIfNotNull(Queue<Leaf> queue) {}
}
