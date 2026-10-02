DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'mixtapes_title_length') THEN
    ALTER TABLE mixtapes ADD CONSTRAINT mixtapes_title_length CHECK (length(title) <= 120);
  END IF;

  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'mixtapes_description_length') THEN
    ALTER TABLE mixtapes ADD CONSTRAINT mixtapes_description_length CHECK (description IS NULL OR length(description) <= 500);
  END IF;

  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'tracks_title_length') THEN
    ALTER TABLE tracks ADD CONSTRAINT tracks_title_length CHECK (title IS NULL OR length(title) <= 300);
  END IF;

  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'tracks_artist_length') THEN
    ALTER TABLE tracks ADD CONSTRAINT tracks_artist_length CHECK (artist IS NULL OR length(artist) <= 200);
  END IF;

  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'tracks_note_length') THEN
    ALTER TABLE tracks ADD CONSTRAINT tracks_note_length CHECK (length(note) <= 2000);
  END IF;

  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'tracks_order_limit') THEN
    ALTER TABLE tracks ADD CONSTRAINT tracks_order_limit CHECK (track_order < 100);
  END IF;
END $$;
