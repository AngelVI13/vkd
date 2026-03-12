-- @create_leagues
CREATE TABLE IF NOT EXISTS leagues (
    id INTEGER PRIMARY KEY,
    league_year INTEGER NOT NULL
);

-- @create_events
CREATE TABLE IF NOT EXISTS events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    event_date TEXT NOT NULL,
    location TEXT NOT NULL
);

-- @create_courses
CREATE TABLE IF NOT EXISTS courses (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    course_id INTEGER NOT NULL,
    course_name TEXT NOT NULL,
    distance FLOAT NOT NULL,
    num_controls TEXT NOT NULL,
    controls TEXT NOT NULL
);

-- @create_results
CREATE TABLE IF NOT EXISTS results (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    course_id INTEGER NOT NULL,
    runner_id INTEGER NOT NULL,
    -- TODO: continue from here
);


