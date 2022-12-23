# Change Log:

### 0.3.3
- Avoid redundant theme-loading: checking whether a desired theme is already
  loaded in `circadian-enable-theme`. `run-at-time` did fire when
  `circadian-setup` has already been overdue (e.g. during system hibernation). 
  Big kudos to @tangxinfa for the pull request.
- Fix #25: fixes wrong behavior when using time strings
- Fix #27: prompts error when (solar-sunrise-sunset (calendar-current-date))
  returns nil for requsted sunrise/sunset times

### 0.3.2
- Error messages in `circadian-match-sun` if sunset or sunrise could not be
  parsed due to time zone settings. Timezone issues should be checked/evaluated in future
  development and versions.

### 0.3.1
- Add theme loading hooks: `circadian-before-load-theme-hook` and
  `circadian-after-load-theme-hook`

### 0.3.0
- Implement configuration with keywords `:sunrise` and `:sunset`
- Rename `circadian-mapcar` to `circadian-mapc`
- Fixed time-comparison bug in
  in `circadian-compare-time-strings`
- Renamed `circadian-compare-time-strings` to more appropriate name:
  `circadian-a-earlier-b-p`

### 0.2.3
- Use -*- lexical-binding: t -*-
- Requiring cl-lib
- Prefixed cl function like `cl-first', `cl-remove-if'
- `mapcar' -> `mapc'
- Swapped argument order for `circadian-filter-inactivate-themes'
- Bugfix: load the last theme from `circadian-themes', when the first
time slot lies in the future

### 0.2.2
- Added testing (+ configuration for travis CI)
- Changed arguments of `circadian-filter-inactivate-themes' to accept
the current time string + themes asoc list due to gain testability
of that function with various time strings (see `tests/').

### 0.2.1
- Add function to load the latest overdue theme to `circadian-setup'

### 0.2.0
- nyx-theme and hemera-theme live in their own repos from now on:
nyx: https://github.com/GuidoSchmidt/emacs-nyx-theme
hemera: https://github.com/GuidoSchmidt/emacs-hemera-theme
- Use default themes for default configuration of `circadian-themes'
- Re-implemented configuration using associated list and timers
(thanks to Steve Purcell for pointing me into this direction)

### 0.1.0
- Initial release
- Variables for day/night hour
- Themes included: hemera-theme, nyx-theme
