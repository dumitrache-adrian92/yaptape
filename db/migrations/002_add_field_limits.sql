ALTER TABLE mixtapes
  ADD CONSTRAINT mixtapes_title_length CHECK (length(title) <= 120),
  ADD CONSTRAINT mixtapes_description_length CHECK (description IS NULL OR length(description) <= 500);

ALTER TABLE tracks
  ADD CONSTRAINT tracks_title_length CHECK (title IS NULL OR length(title) <= 300),
  ADD CONSTRAINT tracks_artist_length CHECK (artist IS NULL OR length(artist) <= 200),
  ADD CONSTRAINT tracks_note_length CHECK (length(note) <= 2000),
  ADD CONSTRAINT tracks_order_limit CHECK (track_order < 100);
