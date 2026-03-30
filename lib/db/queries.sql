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

-- @create_event_stats
CREATE TABLE IF NOT EXISTS event_stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,

    num_men INTEGER NOT NULL,
    num_women INTEGER NOT NULL
);

-- @create_courses
CREATE TABLE IF NOT EXISTS courses (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    course_id INTEGER NOT NULL,

    course_name TEXT NOT NULL,
    distance FLOAT NOT NULL,
    num_controls TEXT NOT NULL,
    controls TEXT NOT NULL
);


-- @create_course_stats
CREATE TABLE IF NOT EXISTS course_stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    course_id INTEGER NOT NULL,

    num_men  INTEGER NOT NULL,
    num_women  INTEGER NOT NULL,
    tilt_overall  INTEGER NOT NULL,
    tilt_men  INTEGER NOT NULL,
    tilt_women  INTEGER NOT NULL,
    mistake_time_overall  INTEGER NOT NULL,
    mistake_time_men  INTEGER NOT NULL,
    mistake_time_women  INTEGER NOT NULL,
    blunder_perc_overall  INTEGER NOT NULL,
    blunder_perc_men  INTEGER NOT NULL,
    blunder_perc_women  INTEGER NOT NULL,
    big_mistake_perc_overall  INTEGER NOT NULL,
    big_mistake_perc_men  INTEGER NOT NULL,
    big_mistake_perc_women  INTEGER NOT NULL,
    small_mistake_perc_overall  INTEGER NOT NULL,
    small_mistake_perc_men  INTEGER NOT NULL,
    small_mistake_perc_women  INTEGER NOT NULL,
    most_tricky_overall  INTEGER,
    most_tricky_men  INTEGER,
    most_tricky_women  INTEGER,
    avg_time_for_mistake_overall  INTEGER NOT NULL,
    avg_time_for_mistake_men  INTEGER NOT NULL,
    avg_time_for_mistake_women  INTEGER NOT NULL,
    avg_mistake_num_overall  INTEGER NOT NULL,
    avg_mistake_num_men  INTEGER NOT NULL,
    avg_mistake_num_women  INTEGER NOT NULL
);

-- @create_age_groups
CREATE TABLE IF NOT EXISTS age_groups (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    runner_id INTEGER NOT NULL,

    group_id TEXT NOT NULL
);

-- @create_results
CREATE TABLE IF NOT EXISTS results (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    course_id INTEGER NOT NULL,
    runner_id INTEGER NOT NULL,

    time_sec INTEGER,
    start INTEGER,
    points INTEGER NOT NULL,
    pace TEXT,
    finished INTEGER NOT NULL
);

-- @create_result_stats
CREATE TABLE IF NOT EXISTS result_stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    course_id INTEGER NOT NULL,
    runner_id INTEGER NOT NULL,

    mistake_time INTEGER NOT NULL,
    mistake_num INTEGER NOT NULL,

    small_mistake_time INTEGER NOT NULL,
    small_mistake_num INTEGER NOT NULL,
    small_mistake_time_ratio INTEGER NOT NULL,
    small_mistake_num_ratio INTEGER NOT NULL,

    big_mistake_time INTEGER NOT NULL,
    big_mistake_num INTEGER NOT NULL,
    big_mistake_time_ratio INTEGER NOT NULL,
    big_mistake_num_ratio INTEGER NOT NULL,

    blunder_mistake_time INTEGER NOT NULL,
    blunder_mistake_num INTEGER NOT NULL,
    blunder_mistake_time_ratio INTEGER NOT NULL,
    blunder_mistake_num_ratio INTEGER NOT NULL,

    consecutive_mistakes INTEGER NOT NULL,
    tilt_rate INTEGER NOT NULL,

    mistake_cluster TEXT,
    mistakes_impact TEXT,
    race_execution TEXT,

    best_splits INTEGER NOT NULL,
    top5_splits INTEGER NOT NULL,
    top10_splits INTEGER NOT NULL,
    performance INTEGER NOT NULL,

    overall_position INTEGER,
    position_gender INTEGER,
    position_group INTEGER,

    potential_time INTEGER,
    potential_position INTEGER
);

-- @create_splits
CREATE TABLE IF NOT EXISTS splits (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    course_id INTEGER NOT NULL,
    runner_id INTEGER NOT NULL,
    split_idx INTEGER NOT NULL,

    time_sec INTEGER,
    position INTEGER,
    overall_time INTEGER,
    overall_position INTEGER,
    split_timestamp INTEGER,
    mistake_time INTEGER
);

-- @create_ratings
CREATE TABLE IF NOT EXISTS ratings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    event_date TEXT NOT NULL,
    course_id INTEGER NOT NULL,
    runner_id INTEGER NOT NULL,

    rating FLOAT NOT NULL,
    rating_diff FLOAT NOT NULL,
    rd FLOAT NOT NULL,
    vol FLOAT NOT NULL
)
