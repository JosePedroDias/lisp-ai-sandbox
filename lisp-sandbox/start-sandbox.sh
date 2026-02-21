#!/bin/bash
# Start the Common Lisp sandbox with Swank server

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ENV_FILE="$SCRIPT_DIR/../node-repl/.env"

# Load environment variables from .env if it exists
if [ -f "$ENV_FILE" ]; then
  echo "Loading configuration from $ENV_FILE"
  set -a
  source "$ENV_FILE"
  set +a
fi

# Use defaults if not set
SWANK_PORT="${SWANK_PORT:-4006}"

echo "Starting Common Lisp sandbox with Swank server..."
echo "Port: $SWANK_PORT"
echo ""

sbcl --load "$SCRIPT_DIR/start-swank.lisp"

