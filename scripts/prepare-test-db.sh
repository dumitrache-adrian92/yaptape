#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

export DATABASE_PASSWORD="${DATABASE_PASSWORD:-${POSTGRES_PASSWORD:-yaptape-dev}}"

docker compose up -d --wait db

for migration in db/migrations/*.sql; do
  docker compose exec -T db psql -v ON_ERROR_STOP=1 -U yaptape -d yaptape < "$migration"
done
