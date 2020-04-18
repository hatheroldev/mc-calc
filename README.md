# Emacs Package mc-calc

This [Emacs](https://www.gnu.org/software/emacs/) package allows the use of calc in regions with [multiple-cursors](https://github.com/magnars/multiple-cursors.el).

## Installation

Someday mc-calc can be installed through `package.el`.

It will be available on [MELPA](http://melpa.org/) and probably [MELPA Stable](http://stable.melpa.org):

```emacs.desktop
M-x package-install mc-calc
```

The package depends on the `multiple-cursors` package, so if you do not use
`package.el`, you would need to install that too.

## Documentation

Please use `M-x finder-commentary mc-calc` for documentation.

## Usage Examples

All following examples assume you know how to use `multiple-cursors`.
But so that you can follow without knowing `multiple-cursors`, I use the verb *to mc*, which means the following operations:

- move the cursor to the beginning of the first element of interest (number or expression),
- mark each line with `M-x mc/mark-next-lines`,
- activate the region for each cursor over the element of interest by issuing `C-SPC` and moving the cursor right.

You can disable `multiple-cursors` after using it by entering `RET`.

### Simple Calculations

Say you write code in `C` and want to evaluate the following formulas:

```c
#defun BITMASK_BIT1 2^1
#defun BITMASK_BIT2 2^2
#defun BITMASK_BIT6 2^6
```

All you have to do is to *mc* the `2^*` parts and issue `M-x mc-calc-eval` to get:

```c
#defun BITMASK_BIT1 2
#defun BITMASK_BIT2 4
#defun BITMASK_BIT6 64
```

### Vectors

Consider you have the following org table and quickly want to calculate 2 to the power of those values:

```org
| 1 |
| 2 |
| 6 |
```

You simply *mc* each number and use `M-x mc-calc-grab`:

```text
--- Emacs Calculator Mode ---
1:  [1, 2, 6]
    .
```

Then in the calc buffer you enter `2` and `TAB` followed by `VM^` to get the desired values:

```text
--- Emacs Calculator Mode ---
1:  [2, 4, 64]
    .
```

To get these values back into the table you use `M-x mc-calc-copy-to-buffer`, disable `multiple-cursors` with `RET` and re-align the table with `TAB`:

```org
|  2 |
|  4 |
| 64 |
```

## Testing

First install [Cask](https://github.com/cask/cask).

Then execute unit tests:

```shell
./ut.sh
```

and behavior tests:

```shell
./br.sh
```

Useful links for test development:

-   [Tuxicity: Unit testing in Emacs](https://rejeep.github.io/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html)
-   <https://github.com/ecukes/ecukes>
-   <https://github.com/ecukes/espuds>

***
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Compile Test](https://github.com/hatheroldev/mc-calc/workflows/Compile%20Test/badge.svg)](https://github.com/hatheroldev/mc-calc/actions?query=workflow%3A%22Compile+Test%22)
[![Lint](https://github.com/hatheroldev/mc-calc/workflows/Lint/badge.svg)](https://github.com/hatheroldev/mc-calc/actions?query=workflow%3A%22Lint%22)
[![Unit Test](https://github.com/hatheroldev/mc-calc/workflows/Unit%20Test/badge.svg)](https://github.com/hatheroldev/mc-calc/actions?query=workflow%3A%22Unit+Test%22)
[![Behaviour Test](https://github.com/hatheroldev/mc-calc/workflows/Behaviour%20Test/badge.svg)](https://github.com/hatheroldev/mc-calc/actions?query=workflow%3A%22Behaviour+Test%22)
