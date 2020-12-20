# Quicklisp Stats

A system that fetches and performs basic operations on the Quicklisp download statistics.

## Warning

> 14:51 \<Xach\> there may be errors - this is the first effort. i am using a mix of shell and sql and stuff and something may have failed to query properly.
>
> 14:53 \<Xach\> I would caution against inferring too much from this data. many are caused by bots, builds, etc. it's not a true measure of popularity or utility or whatever

## API

* `(fetch-all &optional verbosep)`
  * Downloads Quicklisp stats for a given month and returns them.
* `(month year month &optional verbosep)`
  * Downloads Quicklisp stats for a given month and returns them.
* `(system-downloads system year month)`
  * Returns the number of times `system` was downloaded from Quicklisp during the `month` of `year`, or `nil` if the system was not found in Quicklisp stats for that month.

All results are cached in `*cache*` whose value is an `equal`-tested hash table, so if data for a given month was already downloaded, it is not fetched again. The keys of `*cache*` are conses of year and month, and values are alists of system names and download counts.

If data for a given month is unavailable, an error of type `no-data-yet` is signaled, with accessors `no-data-yet-month` and `no-data-yet-year` for retrieving the problematic month and year combination.

## License

MIT
