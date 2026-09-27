CREATE TABLE mixtapes (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  title text NOT NULL CHECK (length(trim(title)) > 0),
  description text,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE tracks (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  mixtape_id uuid NOT NULL REFERENCES mixtapes (id) ON DELETE CASCADE,
  track_order integer NOT NULL CHECK (track_order >= 0),
  video_id text NOT NULL CHECK (video_id ~ '^[A-Za-z0-9_-]{11}$'),
  title text,
  artist text,
  note text NOT NULL CHECK (length(trim(note)) > 0),
  UNIQUE (mixtape_id, track_order)
);
