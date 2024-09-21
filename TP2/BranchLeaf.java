package tree;

import java.util.ArrayList;
import java.util.List;
import java.util.Queue;

public class BranchLeaf extends Leaf {
    private Object carga;
    private List<Leaf> children;

    public BranchLeaf(Object carga) {
        this.carga = carga;
        this.children = new ArrayList<>();
        this.children.add(new NullLeaf("Nada a la siniestra!"));
        this.children.add(new NullLeaf("Nada a la diestra!"));
    }

    @Override
    public void dfs(ArrayList<Object> result) {
        result.add(carga);
        for (Leaf child : children) {
            child.dfs(result);
        }
    }

    @Override
    public void addChildrenToQueue(Queue<Leaf> queue) {
        for (Leaf child : children) {
            child.addToQueueIfNotNull(queue);
        }
    }

    @Override
    public Object getCarga() {
        return carga;
    }

    @Override
    public Leaf addChildAt(int index, Leaf child) {
        children.set(index, child);
        return this;
    }

    @Override
    public Leaf getChildAt(int index) {
        return children.get(index);
    }

    @Override
    protected void addToQueueIfNotNull(Queue<Leaf> queue) {
        queue.add(this);
    }
}
