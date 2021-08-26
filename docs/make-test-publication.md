# Test transaction creation

This document describes step by step how to make a test publication.

## Simple test run

1. Launch an [educator with bot](/educator/README.md#scripts).
2. Run [publications spammer](/scripts/test/student-submissions-spam.sh).
3. Wait for your publication to appear in block.
4. You can check publications with simple call:
```sh
http :8091/api/witness/v1/publications | jq ".publications[].publication"
```
