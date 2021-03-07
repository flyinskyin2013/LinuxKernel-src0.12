
#include "linux/mm.h"

inline volatile void oom(void)
{
	printk("out of memory\n\r");
	do_exit(SIGSEGV);
}