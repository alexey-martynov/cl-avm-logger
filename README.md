Logging Facility
================

This package provides universal logging facility. The primary goal is
to have an API which is portable between platforms. 

The frontend API is platform independent and designed to look native
in Common Lisp system. The backends use different platform dependent
technologies to log data.

After loading `*FEATURES*` will contain keyword `:CL-AVM-LOGGER`.

The Linux distribution might not use systemd at all. This means that
"libsystemd.so" is missing and `sd_journal_sendv()` missed too. To
prevent possible failures in this case the ASDF systems split. The
`CL-AVM-LOGGER` loads fail-safe backends (console and syslog on UNIX)
and `CL-AVM-LOGGER+JOURNAL` loads all other backends and journal too.

Client API
----------

The main API consists of:

* function `(LOG-MESSAGE PRIORITY MESSAGE &REST ARGS)` 

  The message is a format string suitable to pass to the `FORMAT`
  function and `ARGS` all values referenced in this format string. It
  is not a C style format string!
  
  The `PRIORITY` is one value of `:EMERG`, `:ALERT`, `:CRITICAL`,
    `:ERROR`, `:WARNING`, `:NOTICE`, `:INFO` or `:DEBUG`.
    
* condition `INVALID-PRIORITY`

  Raised when incorrect priority value specified.

* function `IDENTIFIER`

  The program identifier which is used by backends to separate
  logs. Assignment to this place sets new identifier.

* variable `*PROPERTIES*`

  Association list with extra properties that might be added to log
  entry. Currently used only by journald backend.

* variable `*AVAILABLE-BACKENDS*`

  List of symbols of loaded backends.
  
* variable `*ACTIVE-BACKENDS*`

  List of active backends. The default value is determined
  automatically during loading:
  
  * after loading Console backend it activates;
  
  * if Journal backend loaded it is activated and all other backends
    are deactived.
    
  This allows to select the most advanced backend.

Backends
--------

### Console

The console backend uses a stream in `*LOG-STREAM*` dynamic variable
to write log messages. This variable is initialized to
`*ERROR-OUTPUT*` on start and can be changed to any other stream. No
log rotation is performed. And log data is not split between files.

Message time stamp is omitted.

This backend adds `CL-AVM-LOGGER-CONSOLE` keyword to `*FEATURES*`.

The message priority is written in line prefix. There 3 styles
supported and current style selected according to `*PRIORITY-STYLE*`
variable:

* `NONE`

  The priority prefix will be omitted.

* `PLAIN` (default)

  The priority will be converted to word ("EMERG", "ALERT",
  "CRITICAL", "ERROR", "WARN", "NOTICE", "INFO" or "DEBUG") and placed
  in brackets at the beginning of the line.

* `JOURNAL`

  The priority will be converted to its numerical value (as in Syslog)
  and written in angle brackets at the beginning of the line. This
  allows simple output redirection in systemd units and/or by
  `systemd-cat` utility.

### Journal

The journal backend uses journald to store messages. It allows to
select identifier for program (`SYSLOG_IDENTIFIER` property) via
generalized variable `(IDENTIFIER)`. The form
`(setf (identifier) "new-id")` return new identifier.

Any additional property can be specified via association list
`*PROPERTIES*` where the key is property name and value is property
value. This value will be finally formatted via `~A` specifier.

This backend adds `CL-AVM-LOGGER-JOURNAL` keyword to `*FEATURES*`.

Internally this backend uses `sd_journal_sendv()` to deliver data to
journal.

### Syslog

Not implemented yet.
