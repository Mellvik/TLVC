// Kernel library
// Local heap management

#include <linuxmt/kernel.h>
#include <linuxmt/heap.h>
#include <linuxmt/string.h>
//#include <linuxmt/lock.h>

// Minimal block size to hold heap header
// plus enough space in body to be useful
// (= size of the smallest allocation)

#define HEAP_MIN_SIZE (sizeof (heap_s) + 16)

// Heap root

// locks not needed unless SMP or reentrant kernel
//static lock_t _heap_lock;
#define WAIT_LOCK(lockp)
#define EVENT_UNLOCK(lockp)

list_s _heap_all;
static list_s _heap_free;
static heap_s *high_free;	/* keep track of the free segment at the high end of the heap */
static heap_s *heap_rsplit(word_t);

// allocate a SEG descriptor from the end of the free block at the end of the heap
// to keep these descriptors from fragmenting the main part of the heap.

static heap_s *heap_rsplit(word_t size0)
{
	heap_s *h1 = high_free;
	heap_s *h2 = (heap_s *)((byte_t *)(h1 + 1) + h1->size - (size0 + sizeof(heap_s)));

	h2->size = size0;
	h1->size -= size0 + sizeof(heap_s);
	list_insert_after(&(h1->all), &(h2->all));
	
	printk("rsplit 1:%x/%d - 2:%x/%d\n", h1, h1->size, h2, h2->size);
	return h2;
}

// Split block if large enough

static void heap_split(heap_s *h1, word_t size0)
{
	word_t size2 = h1->size - size0;

	if (size2 >= HEAP_MIN_SIZE) {
		h1->size = size0;

		heap_s *h2 = (heap_s *) ((byte_t *) (h1 + 1) + h1->size);
		h2->size = size2 - sizeof(heap_s);
		h2->tag = HEAP_TAG_FREE;

		list_insert_after(&(h1->all), &(h2->all));
		list_insert_after(&(h1->free), &(h2->free));
		if (h1 == high_free) high_free = h2;
		printk("split 1:%x/%d - 2:%x/%d hf %x\n", h1, h1->size, h2, h2->size, high_free);
	}
}


// Get free block

static heap_s *free_get(word_t size0, byte_t tag)
{
	// First get the smallest suitable free block

	heap_s *best_h  = 0;
	word_t best_size = 0xFFFF;
	list_s *n = _heap_free.next;

	while (n != &_heap_free) {
		heap_s * h = structof (n, heap_s, free);
		word_t size1  = h->size;

		if ((h->tag == HEAP_TAG_FREE) && (size1 >= size0) && (size1 < best_size)) {
			best_h  = h;
			best_size = size1;
			if (size1 == size0) break;
		}

		n = h->free.next;
	}

	// Then allocate that free block

	if (best_h) {
		if (tag == HEAP_TAG_SEG && best_h == high_free) {
			best_h = heap_rsplit(size0);
		} else {
			heap_split(best_h, size0);
			list_remove(&(best_h->free));
		}
		best_h->tag = HEAP_TAG_USED | tag;
	}
	printk("highfree: %x/%d\n", high_free, high_free->size);

	return best_h;
}

// Merge two contiguous blocks

static void heap_merge (heap_s * h1, heap_s * h2)
{
	h1->size = h1->size + sizeof (heap_s) + h2->size;
	list_remove (&(h2->all));
}


// Allocate block

void *heap_alloc(word_t size, byte_t tag)
{
	WAIT_LOCK(&_heap_lock);
	heap_s *h = free_get(size, tag);
	if (h) {
		h++;						// skip header
		if (tag & HEAP_TAG_CLEAR)
			memset(h, 0, size);
	}
	if (!h) printk("HEAP: no memory (%u bytes)\n", size);
	EVENT_UNLOCK(&_heap_lock);
	return h;
}


// Free block

void heap_free (void * data)
{
	WAIT_LOCK (&_heap_lock);

	heap_s * h = ((heap_s *) (data)) - 1;  // back to header

	// Free block will be inserted to free list:
	//   - tail if merged to previous or next free block
	//   - head if still alone to increase 'exact hit'
	//     chance on next allocation of same size

	list_s * i = &_heap_free;

	// Try to merge with previous block if free

	list_s * p = h->all.prev;
	if (&_heap_all != p) {
		heap_s * prev = structof (p, heap_s, all);
		if (prev->tag == HEAP_TAG_FREE) {
			list_remove (&(prev->free));
			heap_merge (prev, h);
			i = _heap_free.prev;
			h = prev;
		} else {
			h->tag = HEAP_TAG_FREE;
		}
	}

	// Try to merge with next block if free

	list_s * n = h->all.next;
	if (n != &_heap_all) {
		heap_s * next = structof (n, heap_s, all);
		if (next->tag == HEAP_TAG_FREE) {
			list_remove (&(next->free));
			heap_merge (h, next);
			i = _heap_free.prev;
			if (high_free == next) high_free = h;
		}
	}

	// Insert to free list head or tail

	list_insert_after (i, &(h->free));
	printk("freed %x (new %x)\n", h, high_free);

	EVENT_UNLOCK (&_heap_lock);
}


// Add space to heap

void heap_add (void * data, word_t size)
{
	if (size >= HEAP_MIN_SIZE) {
		WAIT_LOCK (&_heap_lock);
		heap_s * h = (heap_s *) data;
		h->size = size - sizeof (heap_s);
		h->tag = HEAP_TAG_FREE;

		// Add large block to tails of both lists
		// as almost no chance for 'exact hit'

		list_insert_before (&_heap_all, &(h->all));
		list_insert_before (&_heap_free, &(h->free));
		high_free = h;

		EVENT_UNLOCK (&_heap_lock);
	}
}

// Initialize heap

void heap_init ()
{
	list_init (&_heap_all);
	list_init (&_heap_free);
}

// Dump heap

#ifdef HEAP_DEBUG

void heap_iterate (void (* cb) (heap_s *))
{
	list_s * n = _heap_all.next;

	while (n != &_heap_all) {
		heap_s * h = structof (n, heap_s, all);
		(*cb) (h);
		n = h->all.next;
	}
}

#endif /* HEAP_DEBUG */

