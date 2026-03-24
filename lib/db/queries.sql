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

-- TODO: create a table for the rating info for each runner and maybe a separate one for the rating,rd,vol changes

-- @create_course_stats
CREATE TABLE IF NOT EXISTS course_stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    course_id INTEGER NOT NULL

    -- TODO: add the following stats
    -- TODO: fist calculate them from file just to see if these course stats tell a story or not
    -- most tricky control overall
    -- most tricky control for men
    -- most tricky control for women
    -- avg cum mistake time overall (this is about total time of mistakes)
    -- avg cum mistake time for men
    -- avg cum mistake time for women
    -- avg mistake time overall
    -- avg mistake time for men
    -- avg mistake time for women
    -- blunder % overall
    -- blunder % for men
    -- blunder % for women
    -- tilt rate overall
    -- tilt rate for men
    -- tilt rate for women
    -- mistake cluster overall
    -- mistake cluster for men
    -- mistake cluster for women
    -- num_women
    -- num_men
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
