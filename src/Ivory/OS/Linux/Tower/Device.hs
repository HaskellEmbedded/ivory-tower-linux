{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.OS.Linux.Tower.Device where

import Ivory.Language
import Ivory.Tower

[ivory|
abstract struct inode "linux/fs.h"
abstract struct file "linux/fs.h"
abstract struct file_operations "linux/fs.h"
|]

fs_header = "linux/fs.h"
dev_header = "linux/device.h"
uaccess_header = "linux/uaccess.h"

type Size = Uint64
type SSize = Sint64

register_chrdev :: Def ('[Uint64, ConstRef s1 ('CArray ('Stored Uint8)), ConstRef s2 ('Struct "file_operations")] ':-> Sint64)
register_chrdev major name fops = importProc "register_chrdev" fs_header

unregister_chrdev :: Def ('[Uint64, ConstRef s ('CArray ('Stored Uint8))] ':-> ())
unregister_chrdev major name = importProc "unregister_chrdev" fs_header

copy_to_user :: Def ('[Ref s1 ('CArray ('Stored Uint8)),  Ref s2 ('CArray ('Stored Uint8)), Uint64] ':-> ())
copy_to_user to from len = importProc "copy_to_user" uaccess_header

copy_from_user :: Def ('[Ref s1 ('CArray ('Stored Uint8)),  Ref s2 ('CArray ('Stored Uint8)), Uint64] ':-> ())
copy_from_user to from len = importProc "copy_from_user" uaccess_header

--static ssize_t device_read(struct file *, char *, size_t, loff_t *);
--static ssize_t device_write(struct file *, const char *, size_t, loff_t *);

type DevReadPtr s = ProcPtr ('[Ref s1 ('Struct "file"), Ref s2 ('CArray ('Stored Uint8)), Offset] ':-> SSize)
type DevWritePtr s = ProcPtr ('[Ref s1 ('Struct "file"), ConstRef s2 ('CArray ('Stored Uint8)), Size, Offset] ':-> SSize)

fops_set_read_write :: Def ('[Ref s1 ('Struct "file_operations"), DevReadPtr s2, DevWritePtr s3] ':-> ())
fops_set_read_write fops readFn writeFn = importProc "fops_set_read_write" "linux_support.h"

uses_device = do
  incl register_chrdev
  incl unregister_chrdev
  incl copy_to_user
  incl copy_from_user
  incl fops_set_read_write
