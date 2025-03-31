# slash-tmp

`slash-tmp` is an Emacs package that provides utilities for creating temporary 
files and directories.

## Installation

To install, add the following to your Emacs configuration:


## Usage

### /tmp/let

Creates some temporary files and directories and binds their paths into 
specific symbols.

```elisp
(require 'slash-tmp)

;; symbol with suffix "/" will
(/tmp/let (foo bar/ bazz)
  )
  

;; 
```

### /tmp/with-temp-dir

```elisp
(require 'slash-tmp)
```

### /tmp/weird-magic-spell



## Copyright
