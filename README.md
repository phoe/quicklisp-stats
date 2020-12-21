# Quicklisp Stats

A system that fetches and performs basic operations on the Quicklisp download statistics.

## Warning

> 14:51 \<Xach\> there may be errors - this is the first effort. i am using a mix of shell and sql and stuff and something may have failed to query properly.
>
> 14:53 \<Xach\> I would caution against inferring too much from this data. many are caused by bots, builds, etc. it's not a true measure of popularity or utility or whatever

## Another warning

At the moment of writing these words, the CSV files provided by Quicklisp only show the top 1000 projects of each month. In case of any questions about the data rather than the tool used to fetch and parse it, please ask @quicklisp for assistance.

## API

The main API function:

* `(system-downloads system year month)`
  * Returns the number of times `system` was downloaded from Quicklisp during the `month` of `year`, or `nil` if the system was not found in Quicklisp stats for that month.

Helper functions:

* `(all &optional verbosep)`
  * Downloads Quicklisp stats for a given month and returns them.
* `(month year month &optional verbosep)`
  * Downloads all Quicklisp stats and returns them.

All results are cached in `*cache*` whose value is an `equal`-tested hash table, so if data for a given month was already downloaded, it is not fetched again. The keys of `*cache*` are conses of year and month, and values are alists of system names and download counts.

If data for a given month is unavailable, an error of type `no-data-yet` is signaled, with accessors `no-data-yet-month` and `no-data-yet-year` for retrieving the problematic month and year combination.

## Examples

```lisp
QUICKLISP-STATS> (system-downloads :alexandria 2020 11)
13731

QUICKLISP-STATS> (loop with stats = (month 2020 4)
                       with filtered-stats 
                         = (remove-if-not (lambda (x) (< 10000 (cdr x))) stats)
                       for (system . count) in filtered-stats 
                       do (format t ";; ~20A : ~5D~%" system count))
;; alexandria           : 19938
;; cl-ppcre             : 15636
;; bordeaux-threads     : 14974
;; trivial-features     : 14569
;; split-sequence       : 14510
;; closer-mop           : 14482
;; trivial-gray-streams : 14259
;; babel                : 14254
;; cffi                 : 12365
;; flexi-streams        : 11940
;; iterate              : 11924
;; named-readtables     : 11205
;; cl-fad               : 10996
;; usocket              : 10859
;; anaphora             : 10783
;; trivial-backtrace    : 10693
NIL

QUICKLISP-STATS> (loop for ((year month) . data) in (all)
                       for result = (a:assoc-value data "bordeaux-threads"
                                                   :test #'equal)
                       do (format t ";; ~4,'0D-~2,'0D: ~D~%" year month result))
;; 2020-01: 16059
;; 2020-02: 12701
;; 2020-03: 17123
;; 2020-04: 14974
;; 2020-05: 14489
;; 2020-06: 13851
;; 2020-07: 14130
;; 2020-08: 10843
;; 2020-09: 13757
;; 2020-10: 13444
;; 2020-11: 15825
NIL
```

## License

MIT
