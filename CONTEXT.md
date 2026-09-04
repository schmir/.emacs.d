# Emacs Configuration

This context defines the editor behavior assembled by this configuration.

## Language

**Language profile**:
The mode selection and tooling setup that applies to one programming or
markup language. One installer function owns each profile, and profiles are
independent of one another.
_Avoid_: Tooling profile, language policy

**Shared policy**:
The Eglot and Flymake settings that apply across every language. Installed
once, before any profile, because the profiles build on it. "Policy" names
this cross-language layer only, which is why a single language's setup is a
profile rather than a policy.
_Avoid_: Global config, defaults
