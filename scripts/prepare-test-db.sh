#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

export DATABASE_PASSWORD="${DATABASE_PASSWORD:-${POSTGRES_PASSWORD:-yaptape-dev}}"
export DATABASE_NAME="${TEST_DATABASE_NAME:-yaptape_test}"

if [[ ! "$DATABASE_NAME" =~ ^[a-zA-Z0-9_]+$ ]]; then
  echo "TEST_DATABASE_NAME may contain only letters, numbers, and underscores." >&2
  exit 2
fi

docker compose up -d --wait db

database_exists=$(docker compose exec -T db psql -U yaptape -d postgres -tAc "SELECT 1 FROM pg_database WHERE datname = '$DATABASE_NAME'")
if [[ "$database_exists" != "1" ]]; then
  docker compose exec -T db createdb -U yaptape "$DATABASE_NAME"
fi

for migration in db/migrations/*.sql; do
  docker compose exec -T db psql -v ON_ERROR_STOP=1 -U yaptape -d "$DATABASE_NAME" < "$migration"
done
