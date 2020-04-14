---
name: "\U0001F41B Bug Report"
about: Create a report to help us improve
title: ''
labels: bug
assignees: hatheroldev

---

**Describe the bug**
A clear and concise description of what the bug is.

**Expected behavior**
A clear and concise description of what you expected to happen:

1. what behavior you expected
2. what behavior you observed
3. and how we can reproduce the issue.

**Please complete the following information:**
 - OS: [e.g. Ubuntu 19.10]
 - Emacs version [e.g. 26.3]
 - multiple-cursors version [e.g. 1.4.0]

**Your Configuration of mc-calc**
Please provide your configuration (if relevant to the bug report):

```elisp
(require 'mc-calc')
```

**Additional context**
Please include a backtrace in your report.  In most cases doing:

    M-x toggle-debug-on-error RET

and then going through the steps again should result in a backtrace.

Before reporting a defect please try to reproduce it using an Emacs instance in which only mc-calc and its dependencies (multiple-colors and calc) have been loaded. Other packages or your configuration should not be loaded. This makes it easier to determine whether the issue lays with mc-calc or something else.

**Possible Solution**
<!--- Only if you have suggestions on a fix for the bug -->
