#include "ivory.h"
#include <linux/hrtimer.h>

void hrtimer_set_callback(
    struct hrtimer *timer
  , uint32_t (*function)(struct hrtimer *waat)
  ) {
  timer->function = function;
};

void fops_set_read_write(
    struct file_operations *fops
  , ssize_t (*read) (struct file *, char __user *, size_t, loff_t *),
  , ssize_t (*write) (struct file *, const char __user *, size_t, loff_t *)
  ) {
  fops->read = read;
  fops->write = write;
};
