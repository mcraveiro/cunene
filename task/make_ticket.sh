#!/bin/bash

ticket_number=`uuid`
ticket_description="$*"

echo "** [[file:objects/${ticket_number}.org][${ticket_description}]]" >> index.org
echo "* Title: ${ticket_description}" > objects/${ticket_number}.org
cat template.org | sed 1,2d >> objects/${ticket_number}.org

echo "created ticket '${ticket_description}' (${ticket_number})"
