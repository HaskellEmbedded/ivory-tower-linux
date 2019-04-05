#include <linux/ktime.h>
#include <ivory.h>

#define UINT64_MAX (18446744073709551615ULL)

#define isnan(x) __builtin_isnan (x)

extern void hrtimer_set_callback(struct hrtimer *timer, uint32_t (*function)(struct hrtimer *waat));
extern void fops_set_read_write(
    struct file_operations *fops
  , ssize_t (*read) (struct file *, char __user *, size_t, loff_t *),
  , ssize_t (*write) (struct file *, const char __user *, size_t, loff_t *)
  )
