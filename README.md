# Yaptape

Someone's got to talk about the music man.

## Description

Yaptape lets people make mixtapes from YouTube tracks, annotate each track with their thoughts, and share a short link to the mixtape.
Use it to introduce someone to a new genre, confess your love, reminisce through music or tell a story separated by tracks.

## Development

```bash
docker compose up -d db
stack run
```

## Testing

```bash
stack test
./scripts/test-unit.sh
./scripts/test-integration.sh
./scripts/test-e2e.sh
```
