// Kernel library
// Local heap management

#include <linuxmt/kernel.h>
#include <linuxmt/heap.h>
#include <linuxmt/string.h>
#include <autoconf.h>

// Minimal block size to hold heap header
// plus enough space in body to be useful
// (= size of the smallest allocation)

#define HEAP_MIN_SIZE (sizeof(heap_s) + 16)
//#define HEAP_SEG_OPT	/* Allocate small SEG descriptors from the upper */
			/* end of the heap to reduce fragmentation. 
			 * Not useful if small memory chuncks are added to 
			 * the heap after the initial 'big heap' as small
			 * allocations will gravitate towards the smaller
			 * block atomagically and not contribute to fragmentation. */
#ifndef CONFIG_BOOTOPTS
#define HEAP_SEG_OPT
#endif

#define VALIDATE_HEAPFREE
#define HEAP_CANARY	0xA5U	/* for header validation */

#define HEAP_DEBUG 0
#if HEAP_DEBUG
#define debug_heap printk
#else
#define debug_heap(...)
#endif

// Heap root

list_s _heap_all;
static list_s _heap_free;

#ifdef HEAP_SEG_OPT
static heap_s *high_free;	/* keep track of the free segment at the high end of the heap */

// allocate a SEG descriptor from the end of the free block at the high end of the heap
// to keep these descriptors from fragmenting the 'main' part of the heap.

static heap_s *heap_rsplit(word_t size0)
{
	heap_s *h1 = high_free;
	heap_s *h2 = (heap_s *)((byte_t *)(h1 + 1) + h1->size - (size0 + sizeof(heap_s)));

	h2->size = size0;
	h1->size -= size0 + sizeof(heap_s);
	list_insert_after(&(h1->all), &(h2->all));
	
	debug_heap("rsplit 1:%x/%u - 2:%x/%u\n", h1, h1->size, h2, h2->size);
	return h2;
}
#endif

// Split block if large enough

static void heap_split(heap_s *h1, word_t size0)
{
	word_t size2 = h1->size - size0;

	if (size2 >= HEAP_MIN_SIZE) {
		h1->size = size0;

		heap_s *h2 = (heap_s *)((byte_t *)(h1 + 1) + size0);
		h2->size = size2 - sizeof(heap_s);
		h2->tag = HEAP_TAG_FREE;

		list_insert_after(&(h1->all), &(h2->all));
		list_insert_after(&(h1->free), &(h2->free));
#ifdef HEAP_SEG_OPT
		if (h1 == high_free) high_free = h2;
		debug_heap("heap_split 1:%x/%u - 2:%x/%u hf %x\n", h1, h1->size, h2, h2->size, high_free);
#endif
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
		heap_s *h = structof(n, heap_s, free);
		word_t size1 = h->size;

		if ((h->tag == HEAP_TAG_FREE) && (size1 >= size0) && (size1 < best_size)) {
			best_h  = h;
			best_size = size1;
			debug_heap("get: %x/%u/%u; ", h, size0, size1);
			if (size1 == size0) break;
		}

		n = h->free.next;
	}

	// Then allocate that free block

	if (best_h) {
		//debug_heap("got: %x/%u\n", best_h, size0);
#ifdef HEAP_SEG_OPT
		if (tag == HEAP_TAG_SEG && best_h == high_free) {
			best_h = heap_rsplit(size0);
		} else
#endif
		{
			heap_split(best_h, size0);
			list_remove(&(best_h->free));
		}
		best_h->tag = HEAP_TAG_USED | tag;
		best_h->canary = HEAP_CANARY;
	}
#ifdef HEAP_SEG_OPT
	debug_heap("highfree: %x/%u, tag 0x%x\n", high_free, high_free->size, tag);
#endif

	return best_h;
}

// Merge two contiguous blocks

static void heap_merge(heap_s *h1, heap_s *h2)
{
	h1->size = h1->size + sizeof(heap_s) + h2->size;
	h2->canary = 0;
	list_remove(&(h2->all));
}


// Allocate block

void *heap_alloc(word_t size, byte_t tag)
{
	heap_s *h;

	if (!size) return NULL;

	h = free_get(size, tag);
	if (h) {
		h++;					// skip header
		if (tag & HEAP_TAG_CLEAR)
			memset(h, 0, size);
	} else
		printk("HEAP: no memory (%u bytes)\n", size);
	return h;
}


// Free block

void heap_free(void *data)
{

	heap_s *h = ((heap_s *)(data)) - 1;  // back to header
	
	if (!data) return;

	// Free block will be inserted to free list:
	//   - tail if merged to previous or next free block
	//   - head if still alone to increase 'exact hit'
	//     chance on next allocation of same size

	list_s *i = &_heap_free;
	debug_heap("free: 0x%x/%u; ", h, h->size);

#ifdef  VALIDATE_HEAPFREE
	if (h->canary != HEAP_CANARY) {
		printk("WARNING: bogus heap_free");
		return;
	}
#endif
	h->tag = HEAP_TAG_FREE;
		
	// Try to merge with previous block if free

	list_s *p = h->all.prev;
	if (&_heap_all != p) {
		heap_s *prev = structof(p, heap_s, all);
		if (prev->tag == HEAP_TAG_FREE) {
			list_remove(&(prev->free));
			heap_merge(prev, h);
			i = _heap_free.prev;
			h = prev;
		}
	}

	// Try to merge with next block if free

	list_s *n = h->all.next;
	if (n != &_heap_all) {
		heap_s *next = structof(n, heap_s, all);
		if (next->tag == HEAP_TAG_FREE) {
			list_remove(&(next->free));
			heap_merge(h, next);
			i = _heap_free.prev;
#ifdef HEAP_SEG_OPT
			if (high_free == next) high_free = h;
#endif
		}
	}

	// Insert to free list head or tail

	list_insert_after(i, &(h->free));

}


// Add space to heap

void heap_add(void *data, word_t size)
{
	if (size >= HEAP_MIN_SIZE) {
		heap_s *h = (heap_s *)data;
		h->size = size - sizeof(heap_s);
		h->tag = HEAP_TAG_FREE;

		// Add large block to tails of both lists
		// as almost no chance for 'exact hit'

		list_insert_before(&_heap_all, &(h->all));
		list_insert_before(&_heap_free, &(h->free));
#ifdef HEAP_SEG_OPT
		if (!high_free) high_free = h;	/* only when heap is created */
					/* ie. ignore later additions to the heap */
		debug_heap("new hf @ %x size %u\n", h, size);
#endif
	}
}

// Initialize heap

void heap_init()
{
	list_init(&_heap_all);
	list_init(&_heap_free);
}

#ifdef HEAP_DEBUG_UNUSED

static void heap_cb(heap_s *h)
{
        printk ("heap:%Xh:%u:%hxh\n",h, h->size, h->tag);
}

void heap_iterate(void (*cb)(heap_s *))
{
	list_s *n = _heap_all.next;

	while (n != &_heap_all) {
		heap_s *h = structof(n, heap_s, all);
		(*cb)(h);
		n = h->all.next;
	}
}

#endif /* HEAP_DEBUG */

