# Yaptape

Yaptape lets people make mixtapes from YouTube tracks, add a note to each track, and share a short link.

## Run locally

Requirements: GHC/Stack and Docker Compose.

1. Start PostgreSQL with `docker compose up -d db`.
2. Apply the migrations in order:

   ```sh
   for migration in db/migrations/*.sql; do
     docker compose exec -T db psql -U yaptape -d yaptape < "$migration"
   done
   ```

3. Start the app with `stack run`.
4. Open <http://localhost:3000>.

The development database defaults are `yaptape` / `yaptape-dev`. Set `POSTGRES_PASSWORD` for Compose and `DATABASE_PASSWORD` for the app to use a different password. The app also reads `DATABASE_HOST` (default `localhost`), `DATABASE_PORT` (`5432`), `DATABASE_USER` (`yaptape`), and `DATABASE_NAME` (`yaptape`).

`PORT` selects the HTTP port (default `3000`). The executable resolves its bundled static files regardless of the current directory. Set `STATIC_DIR` to serve an alternate directory. `compose.yaml` only starts the database; run the app separately or provide an equivalent production deployment.

## Input limits

Create requests are limited to 1 MiB and 100 tracks. Mixtape titles are limited to 120 characters, descriptions to 500, track titles to 300, artist names to 200, and notes to 2,000. These rules are checked by both the HTML and JSON paths and enforced in PostgreSQL by migration `002_add_field_limits.sql`.

## API

- `GET /health` returns a plain-text health response.
- `POST /api/mixtapes` creates a mixtape from JSON.
- `GET /api/mixtapes/{uuid}` returns a stored mixtape.
- `GET /m/{share-code}` renders the listening page.

The browser uses YouTube’s IFrame API and oEmbed service and Google Fonts. htmx 2.0.8 is bundled locally with Subresource Integrity and its license at `static/js/HTMX-LICENSE.txt`. The app emits a Content Security Policy that permits those external services and the YouTube player.

## Browser tests

The Playwright browser tests exercise the create page in Chromium. They cover adding, reordering, and removing tracks, URL feedback, and YouTube title lookup. The oEmbed response is stubbed so the test does not depend on YouTube being available.

Start the database and app using the local setup steps above. In another terminal, install the JavaScript test dependencies and Chromium once:

```sh
npm install
npx playwright install chromium
```

Then run the browser suite:

```sh
npm run test:e2e
```

The app must be reachable at `http://127.0.0.1:3000`. Set `YAPTAPE_URL` to use a different local app URL.
