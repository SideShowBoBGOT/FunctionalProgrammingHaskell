CREATE TABLE authors (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE cities (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE publishers (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE conferences (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE journals (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE books (
    id SERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    city_id INTEGER NOT NULL,
    publisher_id INTEGER NOT NULL,
    year INTEGER NOT NULL,
    FOREIGN KEY (city_id) REFERENCES cities(id),
    FOREIGN KEY (publisher_id) REFERENCES publishers(id)
);

CREATE TABLE book_authors (
    book_id INTEGER NOT NULL,
    author_id INTEGER NOT NULL,
    FOREIGN KEY (book_id) REFERENCES books(id) ON DELETE CASCADE,
    FOREIGN KEY (author_id) REFERENCES authors(id),
    PRIMARY KEY (book_id, author_id)
);

CREATE TABLE articles (
    id SERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    journal_id INTEGER NOT NULL,
    issue INTEGER NOT NULL,
    year INTEGER NOT NULL,
    pages_start INTEGER NOT NULL,
    pages_end INTEGER NOT NULL,
    FOREIGN KEY (journal_id) REFERENCES journals(id)
);

CREATE TABLE article_authors (
    article_id INTEGER NOT NULL,
    author_id INTEGER NOT NULL,
    FOREIGN KEY (article_id) REFERENCES articles(id) ON DELETE CASCADE,
    FOREIGN KEY (author_id) REFERENCES authors(id),
    PRIMARY KEY (article_id, author_id)
);

CREATE TABLE theses (
    id SERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    city_id INTEGER NOT NULL,
    conference_id INTEGER NOT NULL,
    year INTEGER NOT NULL,
    pages_start INTEGER NOT NULL,
    pages_end INTEGER NOT NULL,
    FOREIGN KEY (city_id) REFERENCES cities(id),
    FOREIGN KEY (conference_id) REFERENCES conferences(id)
);

CREATE TABLE thesis_authors (
    thesis_id INTEGER NOT NULL,
    author_id INTEGER NOT NULL,
    FOREIGN KEY (thesis_id) REFERENCES theses(id) ON DELETE CASCADE,
    FOREIGN KEY (author_id) REFERENCES authors(id),
    PRIMARY KEY (thesis_id, author_id)
);

CREATE OR REPLACE FUNCTION delete_author_books()
RETURNS TRIGGER AS $$
DECLARE
    r RECORD;
BEGIN
    FOR r IN SELECT book_id FROM book_authors WHERE author_id = OLD.id
    LOOP
        IF (SELECT COUNT(*) FROM book_authors WHERE book_id = r.book_id) = 1 THEN
            DELETE FROM books WHERE id = r.book_id;
        ELSE
            DELETE FROM book_authors WHERE book_id = r.book_id AND author_id = OLD.id;
        END IF;
    END LOOP;

    FOR r IN SELECT thesis_id FROM thesis_authors WHERE author_id = OLD.id
    LOOP
        IF (SELECT COUNT(*) FROM thesis_authors WHERE thesis_id = r.thesis_id) = 1 THEN
            DELETE FROM theses WHERE id = r.thesis_id;
        ELSE
            DELETE FROM thesis_authors WHERE thesis_id = r.thesis_id AND author_id = OLD.id;
        END IF;
    END LOOP;

    FOR r IN SELECT article_id FROM article_authors WHERE author_id = OLD.id
    LOOP
        IF (SELECT COUNT(*) FROM article_authors WHERE article_id = r.article_id) = 1 THEN
            DELETE FROM articles WHERE id = r.article_id;
        ELSE
            DELETE FROM article_authors WHERE article_id = r.article_id AND author_id = OLD.id;
        END IF;
    END LOOP;

    RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_delete_author_books
BEFORE DELETE ON authors
FOR EACH ROW EXECUTE FUNCTION delete_author_books();

CREATE OR REPLACE FUNCTION insert_book_with_authors(
    book_title TEXT,
    book_city_id INTEGER,
    book_publisher_id INTEGER,
    book_year INTEGER,
    author_ids INTEGER[]
) RETURNS INTEGER AS $$
DECLARE
    new_book_id INTEGER;
    author_id INTEGER;
    author_count INTEGER;
BEGIN
    IF array_length(author_ids, 1) IS NULL OR array_length(author_ids, 1) = 0 THEN
        RAISE EXCEPTION 'The array of author IDs must not be empty';
    END IF;

    FOREACH author_id IN ARRAY author_ids
    LOOP
        IF NOT EXISTS (SELECT 1 FROM authors WHERE id = author_id) THEN
            RAISE EXCEPTION 'Author with ID % does not exist', author_id;
        END IF;
    END LOOP;

    INSERT INTO books (title, city_id, publisher_id, year)
    VALUES (book_title, book_city_id, book_publisher_id, book_year)
    RETURNING id INTO new_book_id;

    FOREACH author_id IN ARRAY author_ids
    LOOP
        INSERT INTO book_authors (book_id, author_id)
        VALUES (new_book_id, author_id);
    END LOOP;

    RETURN new_book_id;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION insert_thesis_with_authors(
    thesis_title TEXT,
    thesis_city_id INTEGER,
    thesis_conference_id INTEGER,
    thesis_year INTEGER,
    thesis_pages_start INTEGER,
    thesis_pages_end INTEGER,
    author_ids INTEGER[]
) RETURNS INTEGER AS $$
DECLARE
    new_thesis_id INTEGER;
    author_id INTEGER;
    author_count INTEGER;
BEGIN
    IF array_length(author_ids, 1) IS NULL OR array_length(author_ids, 1) = 0 THEN
        RAISE EXCEPTION 'The array of author IDs must not be empty';
    END IF;

    FOREACH author_id IN ARRAY author_ids
    LOOP
        IF NOT EXISTS (SELECT 1 FROM authors WHERE id = author_id) THEN
            RAISE EXCEPTION 'Author with ID % does not exist', author_id;
        END IF;
    END LOOP;

    INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)
    VALUES (thesis_title, thesis_city_id, thesis_conference_id, thesis_year, thesis_pages_start, thesis_pages_end)
    RETURNING id INTO new_thesis_id;

    FOREACH author_id IN ARRAY author_ids
    LOOP
        INSERT INTO thesis_authors (thesis_id, author_id)
        VALUES (new_thesis_id, author_id);
    END LOOP;

    RETURN new_thesis_id;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION insert_article_with_authors(
    article_title TEXT,
    article_journal_id INTEGER,
    article_issue INTEGER,
    article_year INTEGER,
    article_pages_start INTEGER,
    article_pages_end INTEGER,
    author_ids INTEGER[]
) RETURNS INTEGER AS $$
DECLARE
    new_article_id INTEGER;
    author_id INTEGER;
    author_count INTEGER;
BEGIN
    IF array_length(author_ids, 1) IS NULL OR array_length(author_ids, 1) = 0 THEN
        RAISE EXCEPTION 'The array of author IDs must not be empty';
    END IF;

    FOREACH author_id IN ARRAY author_ids
    LOOP
        IF NOT EXISTS (SELECT 1 FROM authors WHERE id = author_id) THEN
            RAISE EXCEPTION 'Author with ID % does not exist', author_id;
        END IF;
    END LOOP;

    INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)
    VALUES (article_title, article_journal_id, article_issue, article_year, article_pages_start, article_pages_end)
    RETURNING id INTO new_article_id;

    FOREACH author_id IN ARRAY author_ids
    LOOP
        INSERT INTO article_authors (article_id, author_id)
        VALUES (new_article_id, author_id);
    END LOOP;

    RETURN new_article_id;
END;
$$ LANGUAGE plpgsql;INSERT INTO authors (name) VALUES ('Xenodochial Chebyshev');
INSERT INTO authors (name) VALUES ('Quirky Kirch');
INSERT INTO authors (name) VALUES ('Pensive Jemison');
INSERT INTO authors (name) VALUES ('Pensive Chandrasekhar');
INSERT INTO authors (name) VALUES ('Elated Wozniak');
INSERT INTO authors (name) VALUES ('Angry Austin');
INSERT INTO authors (name) VALUES ('Amazing Aryabhata');
INSERT INTO authors (name) VALUES ('Agitated Montalcini');
INSERT INTO authors (name) VALUES ('Quizzical Benz');
INSERT INTO authors (name) VALUES ('Wonderful Banzai');
INSERT INTO cities (name) VALUES ('Camdenhampton');
INSERT INTO cities (name) VALUES ('Ellenberlow');
INSERT INTO cities (name) VALUES ('Elmborough');
INSERT INTO cities (name) VALUES ('Marley');
INSERT INTO cities (name) VALUES ('Nortwood');
INSERT INTO cities (name) VALUES ('Handlesborne');
INSERT INTO cities (name) VALUES ('Southby');
INSERT INTO cities (name) VALUES ('Emhampton');
INSERT INTO cities (name) VALUES ('Ellerley');
INSERT INTO cities (name) VALUES ('Durside');
INSERT INTO publishers (name) VALUES ('boring_neumann');
INSERT INTO publishers (name) VALUES ('sad_ride');
INSERT INTO publishers (name) VALUES ('upbeat_bose');
INSERT INTO publishers (name) VALUES ('sharp_ritchie');
INSERT INTO publishers (name) VALUES ('quizzical_wescoff');
INSERT INTO publishers (name) VALUES ('quirky_haibt');
INSERT INTO publishers (name) VALUES ('condescending_perlman');
INSERT INTO publishers (name) VALUES ('suspicious_kare');
INSERT INTO publishers (name) VALUES ('silly_jackson');
INSERT INTO publishers (name) VALUES ('awesome_brattain');
INSERT INTO journals (name) VALUES ('lucid_mccarthy');
INSERT INTO journals (name) VALUES ('jolly_lalande');
INSERT INTO journals (name) VALUES ('friendly_jones');
INSERT INTO journals (name) VALUES ('confident_hugle');
INSERT INTO journals (name) VALUES ('amazing_cray');
INSERT INTO journals (name) VALUES ('affectionate_meitner');
INSERT INTO journals (name) VALUES ('musing_liskov');
INSERT INTO journals (name) VALUES ('fervent_nightingale');
INSERT INTO journals (name) VALUES ('brave_mccarthy');
INSERT INTO journals (name) VALUES ('vibrant_euler');
INSERT INTO conferences (name) VALUES ('angry_leavitt');
INSERT INTO conferences (name) VALUES ('sad_darwin');
INSERT INTO conferences (name) VALUES ('upbeat_morse');
INSERT INTO conferences (name) VALUES ('sharp_morse');
INSERT INTO conferences (name) VALUES ('festive_meitner');
INSERT INTO conferences (name) VALUES ('upbeat_spence');
INSERT INTO conferences (name) VALUES ('modest_swanson');
INSERT INTO conferences (name) VALUES ('hopeful_lamarr');
INSERT INTO conferences (name) VALUES ('infallible_newton');
INSERT INTO conferences (name) VALUES ('fervent_knuth');
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('festive_ritchie',1,2,2016);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('reverent_thompson',7,3,2009);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('dazzling_mayer',7,6,1980);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('vibrant_chandrasekhar',3,2,2021);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('affectionate_lovelace',3,4,2002);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('vigorous_kepler',8,4,2023);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('serene_bell',2,8,2016);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('inspiring_bhabha',2,2,2000);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('elated_varahamihira',6,2,1984);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('thirsty_mccarthy',7,6,2000);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('reverent_aryabhata',7,2,2023);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('friendly_haibt',2,1,1993);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('optimistic_mcnulty',2,5,1995);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('optimistic_shirley',10,4,1989);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('relaxed_volhard',2,6,1986);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('blissful_euler',1,7,1989);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('focused_saha',9,1,2001);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('gracious_tesla',7,6,2019);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('thirsty_bardeen',5,6,2012);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('goofy_goldwasser',10,7,1984);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('happy_raman',5,3,2000);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('clever_darwin',5,6,1992);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('pedantic_thompson',8,6,2013);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('practical_franklin',2,5,1995);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('flamboyant_shirley',9,8,2023);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('laughing_varahamihira',2,3,2008);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('xenodochial_cori',1,4,1980);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('condescending_mayer',2,10,2020);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('naughty_joliot',3,9,2009);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('wizardly_euclid',9,6,2011);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('musing_bell',5,9,2013);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('jovial_borg',9,4,1978);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cocky_mcclintock',5,7,2011);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('objective_nightingale',5,6,1977);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('sharp_lamarr',8,4,2013);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('competent_bartik',3,10,2019);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('fervent_cray',5,10,1990);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('sharp_leavitt',3,10,2001);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('stoic_newton',8,1,1979);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('nifty_mclean',3,2,1978);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(7, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(15, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(20, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(10, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(10, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(21, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(34, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 8);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('friendly_bose',6,91,1990,79,89);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('upbeat_northcutt',1,65,1997,71,150);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('sharp_jackson',4,76,2017,7,32);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('cranky_jackson',10,50,1989,71,163);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nifty_darwin',5,64,2010,20,116);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('clever_fermat',9,35,1984,41,95);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('determined_lichterman',4,47,2020,16,41);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('festive_elion',1,24,1994,86,87);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('quirky_spence',9,90,1990,76,169);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('jolly_jang',2,96,1989,43,94);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nervous_roentgen',7,86,1983,65,136);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('practical_lewin',2,34,1995,56,90);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('eager_engelbart',9,13,2015,63,71);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('elated_curie',2,53,2023,35,115);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('quirky_leakey',6,29,1984,14,113);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('youthful_montalcini',5,37,1985,32,93);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('wizardly_turing',6,24,2001,97,112);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('dreamy_agnesi',7,13,1983,70,153);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('cranky_chandrasekhar',6,92,2019,61,104);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('vigilant_ride',9,31,1981,63,148);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('ecstatic_dijkstra',4,13,2013,97,136);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('hardcore_blackwell',4,78,2005,19,92);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('youthful_heyrovsky',3,2,2012,71,162);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('cranky_roentgen',5,67,2010,56,117);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('determined_hermann',10,100,2006,69,160);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nostalgic_volhard',9,62,2017,68,89);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('flamboyant_pasteur',2,52,1977,6,69);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('modest_einstein',10,18,2009,10,108);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('lucid_allen',5,79,1982,71,159);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('elastic_dijkstra',4,97,2000,27,119);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('cocky_fermat',4,83,1998,8,50);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('festive_meitner',1,101,2013,46,47);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('fervent_mahavira',10,86,2002,52,101);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('youthful_swartz',10,99,2011,7,71);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nostalgic_stonebraker',2,58,1988,82,121);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nifty_jang',2,82,2019,27,119);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('festive_montalcini',10,26,1986,36,63);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('ecstatic_almeida',5,42,2021,79,109);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('focused_mirzakhani',10,71,2013,30,61);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('optimistic_mestorf',9,100,1981,44,116);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(24, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(1, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(10, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 7);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('naughty_almeida',4,4,2003,57,127);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('friendly_lalande',6,7,1983,21,50);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('confident_lewin',7,1,1977,37,95);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('modest_goldberg',3,5,1980,98,199);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('ecstatic_dijkstra',7,2,2008,95,171);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('eager_bassi',4,10,1990,17,69);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('ecstatic_lewin',10,2,2007,12,56);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('unruffled_mayer',1,5,2011,6,26);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('dazzling_northcutt',7,2,2016,50,120);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('adoring_hypatia',9,10,1995,58,143);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('adoring_lewin',7,6,1997,52,75);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('vigorous_williams',10,5,1997,94,126);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('agitated_poitras',7,6,2018,14,84);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('unruffled_beaver',1,1,2012,77,80);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('pedantic_lovelace',3,1,1981,43,86);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('stoic_liskov',7,2,1979,5,19);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('naughty_murdock',9,7,2004,97,125);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('sharp_heyrovsky',6,9,1980,5,18);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('nervous_allen',4,7,2022,37,60);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('musing_noether',9,8,1989,31,130);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('blissful_booth',7,4,2008,96,109);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('serene_payne',7,8,1983,36,76);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('cocky_knuth',7,8,1992,33,96);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('eloquent_dubinsky',3,10,1996,23,39);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('festive_curran',6,3,1991,56,84);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('peaceful_chandrasekhar',3,6,1982,38,43);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('jolly_mahavira',7,8,1998,35,40);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('peaceful_kalam',5,6,2001,75,126);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('vibrant_wozniak',4,3,2002,29,39);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('suspicious_archimedes',6,6,2022,43,105);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('agitated_heisenberg',2,3,1999,65,152);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('quirky_jackson',7,10,1984,5,48);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('hardcore_golick',8,7,1979,48,51);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('flamboyant_knuth',9,10,2019,91,162);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('romantic_stonebraker',7,2,2010,19,45);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('quizzical_wescoff',6,9,2006,47,88);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('flamboyant_mccarthy',7,5,2015,18,70);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('elated_bose',4,10,2010,52,112);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('distracted_goodall',2,2,1986,69,109);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('keen_colden',1,1,2004,49,51);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(2, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(4, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(1, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(28, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(35, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(37, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(37, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(37, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 5);
