#! /bin/bash

RESULTS = "{"
RESULTS = "$RESULTS}"
submission = "{
    'assignmentKey': assignment_key,
    'submitterEmail': email_address,
    'secret': token,
    'parts': $RESULTS
}"
