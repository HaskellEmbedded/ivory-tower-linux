/* This file has been autogenerated by Ivory
 * Compiler version  0.1.0.8
 */

#ifndef __KERNEL__
#  define __KERNEL__
#endif
#ifndef MODULE
#  define MODULE
#endif

#include "tower_init.h"
#include <linux/module.h>
#include <linux/printk.h>

static int __init linux_main(void)
{
    printk(KERN_CRIT "MOD_LOLDING");
    tower_main();
    printk(KERN_CRIT "MOD DONE");
    return (uint32_t) 0U;
}

static void __exit linux_exit(void) {
    printk(KERN_CRIT "DESTROYING");
    tower_exit();
    printk(KERN_CRIT "/DESTROYING");
}

module_init(linux_main);
module_exit(linux_exit);

#define AUTHOR "Haskell"
#define DESC   "Ivory Tower generated"

MODULE_LICENSE("Dual BSD/GPL");
MODULE_AUTHOR(AUTHOR);
MODULE_DESCRIPTION(DESC);