Logging Facility
================

This package provides universal logging facility. The primary goal is
to have an API which is portable between platforms. 

The frontend API is platform independent and designed to look native
in Common Lisp system. The backends use different platform dependent
technologies to log data.

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

* variable `*IDENTIFIER*`

  The program identifier which is used by backends to separate logs. 

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
variable `*IDENTIFIER*` (direct assignment of this variable is a
temporary interface and will be removed soon).

Any additional property can be specified via association list
`*PROPERTIES*` where the key is property name and value is property
value. This value will be finally formatted via `~A` specifier.

Internally this backend uses `sd_journal_sendv` to deliver data to
journal.

### Syslog

Not implemented yet.
