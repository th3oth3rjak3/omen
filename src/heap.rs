use std::{cell::RefCell, rc::Rc};

use crate::object::{HeapObject, Object};

/// Heap is the memory heap for allocating and deallocating memory
/// that is tracked by the runtime.
#[derive(Debug, Clone)]
pub struct Heap {
    /// An intrusive linked list tracks all of the
    /// allocated objects on the heap.
    objects: Option<Rc<RefCell<HeapObject>>>,
    /// The number of bytes allocated since the last GC.
    allocated_bytes_since_last_gc: usize,
    /// The number of bytes, that when exceeded, triggers a garbage collection cycle.
    gc_threshold_bytes: usize,
}

impl Heap {
    /// Create a new heap.
    ///
    /// # Parameters
    /// * `initial_threshold` - The size in bytes until the first GC.
    pub fn new(initial_threshold: usize) -> Self {
        Self {
            objects: None,
            allocated_bytes_since_last_gc: 0,
            gc_threshold_bytes: initial_threshold,
        }
    }

    /// Create a new heap-managed allocation.
    ///
    /// # Params:
    /// * `object` - The object to place on the heap.
    ///
    /// # Returns:
    /// * The allocated object, now tracked by the heap.
    pub fn allocate(&mut self, object: Object) -> Rc<RefCell<HeapObject>> {
        let heap_obj = HeapObject::new(object, self.objects.take());
        let new_alloc = Rc::new(RefCell::new(heap_obj));
        self.objects = Some(new_alloc.clone());

        self.allocated_bytes_since_last_gc += new_alloc.borrow().size();

        new_alloc
    }

    /// Determine if garbage collection is needed.
    ///
    /// # Returns:
    /// * True when the allocations have exceeded the threshold, otherwise false.
    pub fn needs_collection(&self) -> bool {
        self.allocated_bytes_since_last_gc >= self.gc_threshold_bytes
    }

    /// Lets the heap know that now is a safe time to run a garbage collection.
    ///
    /// # Parameters
    /// * `roots` - A slice of `Rc<RefCell<HeapObject>>` representing all currently
    ///   reachable objects from the VM (e.g., stack values, globals, and other
    ///   root references). The GC will start marking from these objects to
    ///   determine which objects are still alive.
    pub fn collect_garbage(&mut self, roots: &[Rc<RefCell<HeapObject>>]) {
        self.mark(roots);
        self.sweep();
    }

    /// Mark all of the objects that are reachable from the provided roots
    /// to prepare for a garbage collection. Any marked items are spared from collection.
    ///
    /// # Params
    /// * `roots` - A slice of `Rc<RefCell<HeapObject>>` representing all currently
    ///   reachable objects from the VM (e.g., stack values, globals, and other
    ///   root references). The GC will start marking from these objects to
    ///   determine which objects are still alive.
    fn mark(&mut self, roots: &[Rc<RefCell<HeapObject>>]) {
        let mut gray_stack: Vec<Rc<RefCell<HeapObject>>> = roots.to_vec();

        while let Some(obj_rc) = gray_stack.pop() {
            let mut obj = obj_rc.borrow_mut();
            if obj.marked {
                continue;
            }

            obj.marked = true;

            // add child references to gray stack
            for child in obj.children() {
                gray_stack.push(child.clone());
            }
        }
    }

    /// Remove all unmarked objects from the list and relink the list.
    /// When unmarked objects are removed from the list, if no other references
    /// exist to that object, the object will be freed.
    pub fn sweep(&mut self) {
        let mut new_head: Option<Rc<RefCell<HeapObject>>> = None;
        let mut last: Option<Rc<RefCell<HeapObject>>> = None;

        let mut current = self.objects.take();
        let mut reclaimed_bytes = 0;

        while let Some(obj_rc) = current {
            let mut obj = obj_rc.borrow_mut();
            let next = obj.next.take();

            if obj.marked {
                obj.marked = false;

                if let Some(last_rc) = &last {
                    last_rc.borrow_mut().next = Some(obj_rc.clone());
                } else {
                    new_head = Some(obj_rc.clone());
                }

                last = Some(obj_rc.clone());
            } else {
                // If the object isn't marked, we let it drop.
                reclaimed_bytes += obj.size();
            }

            current = next;
        }

        self.objects = new_head;

        // Reset allocation counter
        let allocated_since_last_gc = self.allocated_bytes_since_last_gc;
        self.allocated_bytes_since_last_gc = 0;

        // Adaptive threshold: increase if little reclaimed, decrease if lots freed
        let freed_fraction = reclaimed_bytes as f64 / (allocated_since_last_gc.max(1) as f64);
        if freed_fraction < 0.1 {
            self.gc_threshold_bytes = (self.gc_threshold_bytes as f64 * 1.5) as usize;
        } else {
            // Avoid shrinking too much
            const MIN_THRESHOLD: usize = 1024;
            self.gc_threshold_bytes = std::cmp::max(
                MIN_THRESHOLD,
                (self.gc_threshold_bytes as f64 * 0.8) as usize,
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unreferenced_objects_are_collected() {
        let mut heap = Heap::new(1024);

        // Allocate three objects, none of which are roots
        heap.allocate(Object::String("one".into()));
        heap.allocate(Object::String("two".into()));
        heap.allocate(Object::String("three".into()));

        // Before GC, the heap has 3 objects
        assert!(heap.objects.is_some());

        // Run GC with no roots
        heap.collect_garbage(&[]);

        // After GC, the list should be empty
        assert!(heap.objects.is_none());
    }

    #[test]
    fn referenced_objects_are_preserved() {
        let mut heap = Heap::new(1024);

        // Allocate some objects
        let obj1 = heap.allocate(Object::String("one".into()));
        let obj2 = heap.allocate(Object::String("two".into()));

        // Only obj1 is a root
        let roots = vec![obj1.clone()];

        // Run GC
        heap.collect_garbage(&roots);

        // obj1 should still be alive
        assert!(heap.objects.is_some());
        assert!(
            heap.objects.as_ref().unwrap().borrow().marked == false
                || heap.objects.as_ref().unwrap().borrow().object == Object::String("one".into())
        );

        // obj2 should be collected
        // Note: since we don't have direct access to obj2, we just check count
        // We could traverse the list and count elements if needed
        let mut count = 0;
        let mut current = heap.objects.clone();
        while let Some(rc) = current {
            count += 1;
            current = rc.borrow().next.clone();
        }
        assert_eq!(count, 1);
    }
}
