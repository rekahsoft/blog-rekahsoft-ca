---
title: Guix North America 2025-09 Outage Postmortem
author: Collin J. Doering
date: 2025-09-11
description: Postmortem analysis of the September 2025 cuirass.genenetwork.org outage caused by btrfs metadata exhaustion.
tags: general, guix
---

On September 3rd, 2025, the Guix North America build server (`cuirass.genenetwork.org`) was
taken down for maintenance due to Btrfs metadata issues. By September 4th, the filesystem had
degraded to an unbootable state. This postmortem is long overdue - apologies for the delay.
Here's what happened, how we recovered, and what we've done to prevent it from happening
again.

<!--more-->

# Summary

**Impact:** Cuirass builds and substitute publishing were unavailable from September 3–9,
2025. Signing keys were preserved, so existing substitutes remained valid. Builds that hadn't
yet been cached by guix-publish were lost; the primary user impact was downtime.

**Root cause:** Btrfs ran out of metadata space, leading to an unrecoverable read-only state.

**Contributing factors:**

- Metadata (not data) space exhaustion.
- Automatic resume of a partially completed `btrfs balance` during boot.
- Operator attempts to rebalance under constrained space.

**Resolution:** Filesystem was deemed unrecoverable; Guix was reinstalled from the server's
Debian partition (used for bootstrapping) while preserving critical state (Cuirass DB,
caches, signing keys, etc.).

**Prevention:** Periodic `btrfs balance` and `btrfs scrub` and Guix garbage collection jobs
have been added.

# Timeline

| Date       | Event                                                                                                                                                                      |
|------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 2025-09-03 | System taken down for maintenance due to Btrfs metadata exhaustion and rebalance issues.                                                                                   |
| 2025-09-04 | Root filesystem degraded; system unable to boot. Recovery via Debian planned.                                                                                              |
| 2025-09-07 | Partial recovery attempted: guix-publish, nginx caches, Cuirass database state, signing keys, and SSH host keys preserved. System expected to return online within 24–48h. |
| 2025-09-09 | `cuirass.genenetwork.org` back online (17:00 EST). Cached substitutes recovered; older builds lost.                                                                        |

# Root Cause Analysis

Btrfs had exhausted its available metadata space. The filesystem wasn't immediately
read-only - some operations still worked, others failed unpredictably. I'd recovered from
`ENOSPC` ("No space left on device") on personal systems before, but those were **data**
space issues. This time it was **metadata** - the filesystem's internal structures (extent
trees, checksums, etc.). Btrfs needs unallocated device space to create new metadata chunks,
and there was none left.

The underlying problem was that all unallocated space on the device had been assigned to data
chunks. Data had plenty of free space *within* its chunks (~224 GB), but that space was
allocated to data and couldn't be reassigned to metadata. Btrfs needs unallocated device
space to create new metadata chunks, and there was none left.

The initial symptom was PostgreSQL (used by Cuirass) failing to start. To verify the
underlying issue, I tested basic file operations in the PostgreSQL data directory:

``` {.bash .code-term}
# sudo -u postgres cp /var/lib/postgresql/data/pg_logical/replorigin_checkpoint.tmp /var/lib/postgresql/data/pg_logical/test.tmp
# sudo -u postgres mv /var/lib/postgresql/data/pg_logical/test.tmp /var/lib/postgresql/data/pg_logical/test_final
mv: cannot move '/var/lib/postgresql/data/pg_logical/test.tmp' to '/var/lib/postgresql/data/pg_logical/test_final': No space left on device
```

Even though data usage appeared moderate, metadata allocation had reached its limit.

# Recovery Attempts

*Note: Some of my debugging notes from this period were lost, and it's been a while since the
outage, so some specifics may be missing.*

After identifying the issue, I temporarily reallocated Debian's swap partition and added it
to the Btrfs pool to extend available device space, then attempted to reclaim metadata via
`btrfs balance`. I initiated a partial rebalance with:

``` {.bash .code-term}
btrfs balance start -dusage=5 /mnt
```

This reclaims underutilized data chunks to free space for new metadata. It didn't help.

After several more attempts, I rebooted. Bad move. Btrfs [automatically
resumes](https://github.com/kdave/btrfs-progs/issues/550) incomplete balance operations on
mount, and the resume mechanism applies default high-threshold filters - adding `-musage=90
-susage=90` to my original `-dusage=5`. This effectively ordered a near-complete [rewrite of
metadata and system chunks](https://wiki.tnonline.net/w/Btrfs/ENOSPC), which immediately
failed with ENOSPC on boot:

``` {.bash .code-term}
BTRFS: error (device sdb2 state A) in __btrfs_free_extent:3211: errno=-28 No space left
BTRFS: Transaction aborted (error -28)
BTRFS info (device sdb2 state EA): forced readonly
BTRFS: error (device sdb2 state EA) in btrfs_run_delayed_refs:2160: errno=-28 No space left
BTRFS info (device sdb2 state EA): balance: resume -dusage=5 -musage=90 -susage=90
BTRFS info (device sdb2 state EA): 2 enospc errors during balance
BTRFS info (device sdb2 state EA): balance: ended with status: -28
...
BTRFS info (device sdb2 state EA): dumping space info:
BTRFS info (device sdb2 state EA): space_info DATA has 240883466240 free, is not full
BTRFS info (device sdb2 state EA): space_info METADATA has 0 free, is full
BTRFS info (device sdb2 state EA): space_info SYSTEM has 8175616 free, is not full
```

Note the critical line: **`METADATA has 0 free, is full`**. The ~224 GB "free" in DATA refers
to unused space *within* data chunks - space that was allocated to data and couldn't be
reassigned to metadata. There was no unallocated device space left for Btrfs to create new
metadata chunks. Register dumps and call trace omitted for brevity.

From here the filesystem was stuck in a circular failure: no unallocated device space meant
no new metadata chunks could be created, which forced read-only mode, which prevented the
balance operation that would have freed data chunks back to unallocated space. Every mount
triggered a balance resume, which immediately failed with ENOSPC and forced read-only. Rescue
mount options (`rescue=usebackuproot,skip_balance`) didn't help - `skip_balance` failed to
prevent the resume (for reasons I wasn't able to determine), and the volume couldn't be
mounted read-write regardless.

I used iDRAC (Dell's out-of-band remote management) to boot into Debian and assess further.
The same balance resume behavior persisted when mounting the Btrfs volume. I attempted adding
Debian's swap partition to the pool to provide unallocated space for metadata allocation, but
this didn't resolve the issue either. Rather than risk making things worse, and given the
downtime already incurred, I opted to proceed with re-installation.

# Resolution

With recovery off the table, I reinstalled Guix from Debian - the same bootstrapping approach
used during original setup (`guix system init` from a running Debian). Before wiping the
filesystem, I preserved critical state under Debian's `/home`:

- `/var/lib/guix` signing key
- `/var/log` and `/var/cache/nginx`
- `/var/lib/postgresql` (Cuirass database)
- `/gnu/store` cache (partial)
- SSH host keys

Guix was reinstalled on a fresh Btrfs filesystem and Cuirass restored from these assets.
Service resumed on September 9.

# Discussion: Btrfs vs Ext4

Ext4 is the safer default for most workloads. But Btrfs compression lets us store
significantly more derivations in `/gnu/store` without adding disks, which is valuable.

The catch is that Guix workloads are hard on filesystem metadata - `/gnu/store` accumulates
large numbers of files, and garbage collection creates significant churn. Without proactive
maintenance, [metadata exhaustion](https://btrfs.readthedocs.io/en/latest/trouble-index.html)
is a real risk.

We chose to retain Btrfs after the reinstall, relying on the preventive measures below.

# Preventive Actions

1. **Regular Maintenance Jobs** (done) Periodic [Btrfs balance and
   scrub](https://wiki.archlinux.org/title/Btrfs#Maintenance) operations have been [added as
   Shepherd
   timers](https://git.genenetwork.org/guix-north-america/commit/?id=4b46281409776a28c451dd245473760366f3ff7a),
   following the pattern used by the [GNU Guix project's own build
   farm](https://cgit.git.savannah.gnu.org/cgit/guix/maintenance.git/tree/hydra/hatysa.scm#n186).

2. **Improved Guix GC Scheduling** (done) `guix gc` now runs daily via a Shepherd timer to
   reduce store pressure and prevent unchecked metadata growth.

3. **Enhanced Monitoring** (planned) Add alerts for metadata usage (`btrfs filesystem df /`)
   to detect low free space early.

# Lessons Learned

- Metadata exhaustion is distinct from data exhaustion and more difficult to recover from.
- The `btrfs balance resume` mechanism can be dangerous on boot; it should be explicitly
  controlled.

# Closing Thoughts

While critical state was preserved (Cuirass database, signing keys, caches), builds that
hadn't yet been cached by guix-publish were lost. The extended downtime could have been
avoided with earlier intervention. The lesson is straightforward: Btrfs needs regular
maintenance, and that's now in place.

Apologies for the extended outage, and thank you for your patience. And thanks to everyone
using `cuirass.genenetwork.org` as a substitute server - it's great to see the community
benefiting from the build farm.
