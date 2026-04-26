-- @create_leagues
CREATE TABLE IF NOT EXISTS leagues (
    id INTEGER PRIMARY KEY,
    league_year INTEGER NOT NULL,
    name TEXT NOT NULL
);

-- @add_league
INSERT INTO leagues VALUES;

-- @leagues_for_year
SELECT * FROM leagues
WHERE league_year == @year
ORDER BY id;

-- @all_leagues
SELECT * FROM leagues ORDER BY league_year DESC, name DESC;

-- @create_league_events
CREATE TABLE IF NOT EXISTS league_events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,

    event_date TEXT NOT NULL,
    location TEXT NOT NULL
);

-- @add_league_event
INSERT INTO league_events VALUES;

-- @create_event_details 
CREATE TABLE IF NOT EXISTS event_details (
    id INTEGER PRIMARY KEY AUTOINCREMENT,

    event_link TEXT NOT NULL,
    -- NOTE: currently event date is the lookup key because both leagues are
    -- done by the same organizers so they can't have 2 events at the same
    -- date. If this changes then we have BIG PROBLEMS
    event_date TEXT NOT NULL,
    location TEXT NOT NULL,
    thumbnail TEXT NOT NULL,
    map_info TEXT NOT NULL
);

-- @add_event_details
INSERT INTO event_details VALUES;

-- @create_event_map_links
CREATE TABLE IF NOT EXISTS event_map_links (
    id INTEGER PRIMARY KEY AUTOINCREMENT,

    event_date TEXT NOT NULL,
    -- NOTE: this is a comma separated base64 encoded urls
    links TEXT NOT NULL
);

-- @add_event_map_link
INSERT INTO event_map_links VALUES;

-- @league_event_before
SELECT le.*, l.name AS league_name, ed.event_link, ed.thumbnail, ed.map_info, ls.links
FROM league_events le
JOIN leagues l ON le.league_id = l.id
LEFT JOIN event_details ed ON le.event_date = ed.event_date
LEFT JOIN event_map_links ls ON le.event_date = ls.event_date
WHERE le.event_date < @input_date
ORDER BY le.event_date DESC
LIMIT 1;

-- @league_event_after_or_eq
SELECT le.*, l.name AS league_name, ed.event_link, ed.thumbnail, ed.map_info, ls.links
FROM league_events le
JOIN leagues l ON le.league_id = l.id
LEFT JOIN event_details ed ON le.event_date = ed.event_date
LEFT JOIN event_map_links ls ON le.event_date = ls.event_date
WHERE le.event_date >= @input_date
ORDER BY le.event_date ASC
LIMIT 1;

-- @event_details_for_year
SELECT 
    e.*,
    l.links
FROM event_details e
LEFT JOIN event_map_links l
ON e.event_date = l.event_date
-- comparing over range is better since it doesn't have to
-- apply a function like strftime on each row
-- WHERE e.event_date >= printf('%04d-01-01', @year)
--   AND e.event_date <  printf('%04d-01-01', @year + 1)
WHERE strftime('%Y', e.event_date) == @year
ORDER BY e.event_date ASC;

-- @event_details_for_date
SELECT 
    e.*,
    l.links
FROM event_details e
LEFT JOIN event_map_links l
ON e.event_date = l.event_date
WHERE e.event_date == @event_date;

-- @create_event_maps
CREATE TABLE IF NOT EXISTS event_maps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,

    event_date TEXT NOT NULL,
    map_key TEXT NOT NULL,
    title TEXT NOT NULL,
    lat FLOAT NOT NULL,
    lon FLOAT NOT NULL,
    image TEXT NOT NULL,
    for_bikes INTEGER NOT NULL
);

-- @add_event_map
INSERT INTO event_maps VALUES;

-- @create_event_stats
CREATE TABLE IF NOT EXISTS event_stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    event_date TEXT NOT NULL,

    num_men INTEGER NOT NULL,
    num_women INTEGER NOT NULL
);

-- @add_event_stats
INSERT INTO event_stats VALUES;

-- NOTE: this includes events from both leagues
-- @events_to_be_processed
SELECT le.*
FROM league_events le
LEFT JOIN event_stats es ON le.event_date = es.event_date
WHERE es.id IS NULL;

-- NOTE: this includes events from both leagues
-- @events_to_be_processed_for_year
SELECT le.*
FROM league_events le
JOIN leagues l ON le.league_id = l.id
LEFT JOIN event_stats es ON le.event_date = es.event_date
WHERE l.league_year = @league_year
  AND es.id IS NULL;

-- @create_courses
CREATE TABLE IF NOT EXISTS courses (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    event_date TEXT NOT NULL,
    course_id TEXT NOT NULL,

    distance FLOAT NOT NULL,
    num_controls INTEGER NOT NULL,
    controls TEXT NOT NULL
);

-- @add_course
INSERT INTO courses VALUES;

-- @courses_for_event
-- TODO: should this also join with course stats 
-- and results etc ?
SELECT * FROM courses 
WHERE event_date = @event_date
ORDER BY course_id ASC;

-- @create_course_stats
CREATE TABLE IF NOT EXISTS course_stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    event_date TEXT NOT NULL,
    course_id TEXT NOT NULL,

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

-- @add_course_stats
INSERT INTO course_stats VALUES;

-- @stats_for_course
SELECT * FROM course_stats
WHERE league_id = @league_id
    AND event_date = @event_date
    AND course_id = @course_id;

-- @create_age_groups
CREATE TABLE IF NOT EXISTS age_groups (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    runner_id INTEGER NOT NULL,

    age_group_id TEXT NOT NULL
);

-- @add_age_group
INSERT INTO age_groups VALUES;

-- @create_results
CREATE TABLE IF NOT EXISTS results (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    event_date TEXT NOT NULL,
    course_id TEXT NOT NULL,
    runner_id INTEGER NOT NULL,

    time_sec INTEGER,
    start_time INTEGER,
    points INTEGER NOT NULL,
    pace TEXT,
    dsq INTEGER NOT NULL
);

-- @results_for_course
SELECT * FROM results
WHERE league_id = @league_id
    AND event_date = @event_date
    AND course_id = @course_id
ORDER BY
    dsq ASC,
    time_sec ASC;

-- @add_result
INSERT INTO results VALUES;

-- TODO: add query for results

-- @create_result_stats
CREATE TABLE IF NOT EXISTS result_stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    event_date TEXT NOT NULL,
    course_id TEXT NOT NULL,
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

-- @add_result_stats
INSERT INTO result_stats VALUES;

-- @create_splits
CREATE TABLE IF NOT EXISTS splits (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    event_date TEXT NOT NULL,
    course_id TEXT NOT NULL,
    runner_id INTEGER NOT NULL,
    split_idx INTEGER NOT NULL,

    time_sec INTEGER,
    position INTEGER,
    overall_time INTEGER,
    overall_position INTEGER,
    split_timestamp INTEGER,
    mistake_time INTEGER
);

-- @add_splits
INSERT INTO splits VALUES;

-- @create_runners
CREATE TABLE IF NOT EXISTS runners (
    id INTEGER PRIMARY KEY,
    name TEXT,
    club TEXT,
    gender TEXT NOT NULL CHECK (gender IN ('M', 'V'))
);

-- @add_runner
INSERT INTO runners VALUES;

-- NOTE: ratings are only ever created for VKD
-- @create_ratings
CREATE TABLE IF NOT EXISTS ratings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    league_id INTEGER NOT NULL,
    event_nr INTEGER NOT NULL,
    event_date TEXT NOT NULL,
    course_id TEXT NOT NULL,
    runner_id INTEGER NOT NULL,

    rating FLOAT NOT NULL,
    rating_diff FLOAT NOT NULL,
    rd FLOAT NOT NULL,
    vol FLOAT NOT NULL
);

-- TODO: how to calculate position change due to rating loss/gain
-- @ratings_for_course
SELECT r.*
FROM ratings r
INNER JOIN (
    -- NOTE: inner join is needed to find the latest rating based on event_date
    -- for a runner
    SELECT runner_id, MAX(event_date) AS max_date
    FROM ratings
    GROUP BY runner_id
) latest ON r.runner_id = latest.runner_id AND r.event_date = latest.max_date
-- NOTE: we take latest ratings irrespective of league since we only track the main VKD league
WHERE r.course_id = @course_id
ORDER BY r.rating DESC;

-- @ratings_for_course_by_gender
SELECT r.*
FROM ratings r
INNER JOIN (
    -- NOTE: inner join is needed to find the latest rating based on event_date
    -- for a runner
    SELECT runner_id, MAX(event_date) AS max_date
    FROM ratings
    GROUP BY runner_id
) latest ON r.runner_id = latest.runner_id AND r.event_date = latest.max_date
INNER JOIN runners rn ON r.runner_id = rn.id
-- NOTE: we take latest ratings irrespective of league since we only track the main VKD league
WHERE rn.gender = @gender AND r.course_id = @course_id
ORDER BY r.rating DESC;

-- @rating_history_for_league_and_course
SELECT *
FROM ratings
WHERE runner_id = @runner_id 
    AND league_id = @league_id 
    AND course_id = @course_id
ORDER BY event_date ASC;

-- @rating_history_for_course
SELECT *
FROM ratings
WHERE runner_id = @runner_id 
    AND course_id = @course_id
ORDER BY event_date ASC;

-- @add_rating
INSERT INTO ratings VALUES;

-- @create_medals
CREATE TABLE IF NOT EXISTS medals (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    event_date TEXT NOT NULL,
    course_id TEXT NOT NULL,
    runner_id INTEGER NOT NULL,
    medal_type TEXT NOT NULL CHECK (medal_type IN ('gold', 'silver', 'bronze'))
);

-- @medals_for_runner_for_league
SELECT 
    l.name AS league_name,

    SUM(CASE WHEN rm.medal_type = 'gold' THEN 1 ELSE 0 END) AS gold_count,
    SUM(CASE WHEN rm.medal_type = 'silver' THEN 1 ELSE 0 END) AS silver_count,
    SUM(CASE WHEN rm.medal_type = 'bronze' THEN 1 ELSE 0 END) AS bronze_count

FROM medals rm
JOIN league_events le ON rm.event_date = le.event_date
JOIN leagues l ON le.league_id = l.id
WHERE rm.runner_id = @runner_id AND l.id = @league_id;

-- @medals_for_runner_overall
SELECT 
    SUM(CASE WHEN medal_type = 'gold' THEN 1 ELSE 0 END) AS gold,
    SUM(CASE WHEN medal_type = 'silver' THEN 1 ELSE 0 END) AS silver,
    SUM(CASE WHEN medal_type = 'bronze' THEN 1 ELSE 0 END) AS bronze
FROM medals
WHERE runner_id = @runner_id;

-- @medal_history_for_runner
SELECT 
    rm.medal_type,
    le.event_date,
    le.event_nr,
    l.name AS league_name
FROM medals rm
JOIN league_events le ON rm.event_date = le.event_date
JOIN leagues l ON le.league_id = l.id
WHERE rm.runner_id = @runner_id
ORDER BY le.event_date ASC;
