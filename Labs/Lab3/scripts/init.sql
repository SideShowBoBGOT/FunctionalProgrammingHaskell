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

INSERT INTO authors (name) VALUES ('Quirky Bhaskara');
INSERT INTO authors (name) VALUES ('Strange Montalcini');
INSERT INTO authors (name) VALUES ('Compassionate Mirzakhani');
INSERT INTO authors (name) VALUES ('Bold Mclean');
INSERT INTO authors (name) VALUES ('Gracious Davinci');
INSERT INTO authors (name) VALUES ('Amazing Grothendieck');
INSERT INTO authors (name) VALUES ('Ecstatic Cerf');
INSERT INTO authors (name) VALUES ('Serene Perlman');
INSERT INTO authors (name) VALUES ('Kind Ardinghelli');
INSERT INTO authors (name) VALUES ('Unruffled Cartwright');
INSERT INTO cities (name) VALUES ('Ciringstanborough');
INSERT INTO cities (name) VALUES ('Ashover');
INSERT INTO cities (name) VALUES ('Falden');
INSERT INTO cities (name) VALUES ('Skelcompbourne');
INSERT INTO cities (name) VALUES ('Bassenmansby');
INSERT INTO cities (name) VALUES ('Cansbrough');
INSERT INTO cities (name) VALUES ('Stapringbrough');
INSERT INTO cities (name) VALUES ('Chillenton');
INSERT INTO cities (name) VALUES ('Horsford');
INSERT INTO cities (name) VALUES ('Ciringgrove');
INSERT INTO publishers (name) VALUES ('romantic_jang');
INSERT INTO publishers (name) VALUES ('lucid_lamarr');
INSERT INTO publishers (name) VALUES ('suspicious_kepler');
INSERT INTO publishers (name) VALUES ('mystifying_shirley');
INSERT INTO publishers (name) VALUES ('elastic_volhard');
INSERT INTO publishers (name) VALUES ('fervent_pare');
INSERT INTO publishers (name) VALUES ('hardcore_fermat');
INSERT INTO publishers (name) VALUES ('serene_lichterman');
INSERT INTO publishers (name) VALUES ('upbeat_benz');
INSERT INTO publishers (name) VALUES ('wizardly_hoover');
INSERT INTO journals (name) VALUES ('tender_meitner');
INSERT INTO journals (name) VALUES ('determined_snyder');
INSERT INTO journals (name) VALUES ('lucid_torvalds');
INSERT INTO journals (name) VALUES ('determined_bohr');
INSERT INTO journals (name) VALUES ('mystifying_elion');
INSERT INTO journals (name) VALUES ('vigilant_beaver');
INSERT INTO journals (name) VALUES ('quizzical_brown');
INSERT INTO journals (name) VALUES ('vigorous_kirch');
INSERT INTO journals (name) VALUES ('ecstatic_curie');
INSERT INTO journals (name) VALUES ('thirsty_kepler');
INSERT INTO conferences (name) VALUES ('angry_borg');
INSERT INTO conferences (name) VALUES ('keen_chandrasekhar');
INSERT INTO conferences (name) VALUES ('jovial_goldstine');
INSERT INTO conferences (name) VALUES ('romantic_mclean');
INSERT INTO conferences (name) VALUES ('keen_swartz');
INSERT INTO conferences (name) VALUES ('upbeat_spence');
INSERT INTO conferences (name) VALUES ('competent_bardeen');
INSERT INTO conferences (name) VALUES ('hungry_jackson');
INSERT INTO conferences (name) VALUES ('agitated_hawking');
INSERT INTO conferences (name) VALUES ('stupefied_curie');
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('practical_panini',1,3,2007);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cranky_goodall',1,6,1979);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('blissful_rosalind',5,6,1999);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('loving_goldstine',5,1,1979);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('gallant_neumann',7,6,2024);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('priceless_goldberg',1,1,2017);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('amazing_sammet',6,6,1989);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('happy_mestorf',4,7,2004);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('determined_lalande',5,8,1985);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('tender_morse',7,6,1989);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('zealous_aryabhata',3,5,2006);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('mystifying_khorana',4,8,2019);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('quizzical_elion',7,5,1997);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('brave_lovelace',7,5,2013);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('trusting_stonebraker',3,4,1998);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('frosty_liskov',9,2,1980);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('distracted_payne',6,3,2017);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('gallant_nobel',1,1,1999);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cranky_meninsky',9,8,2017);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('hopeful_rosalind',10,1,1994);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('amazing_knuth',9,3,1989);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('focused_keller',4,3,1994);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('suspicious_sammet',3,4,2010);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('dazzling_bartik',8,5,2017);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('vigorous_mccarthy',10,8,1982);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('modest_newton',6,1,1996);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('stoic_leakey',4,2,1980);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('stupefied_goldberg',9,7,1989);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('hardcore_fermi',9,1,2013);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('relaxed_kare',1,1,2008);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('lucid_cori',9,10,1997);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cocky_knuth',8,10,2007);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('nervous_carson',2,5,2007);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('zealous_mayer',6,3,1999);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('nervous_goldberg',4,2,2019);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('sleepy_mcnulty',9,9,2007);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('gracious_pare',10,7,2009);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('pedantic_volhard',4,8,1985);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('blissful_ramanujan',2,9,1985);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('determined_goldberg',6,3,1985);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(20, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(7, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(10, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(15, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(21, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(34, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(34, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 9);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('fervent_khorana',8,50,1980,19,77);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('silly_clarke',4,17,2008,7,33);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('affectionate_pike',8,62,2018,42,138);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('distracted_wing',9,74,1978,68,145);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('elated_sinoussi',2,101,2015,2,31);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('zen_blackwell',1,19,2019,9,82);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('objective_yalow',6,41,2023,35,77);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('vigilant_liskov',10,83,1999,13,19);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('sleepy_torvalds',3,95,2003,21,73);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('goofy_visvesvaraya',5,28,2012,55,106);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('vibrant_colden',1,77,2016,33,62);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('eloquent_banach',5,39,1995,62,114);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('distracted_keller',9,18,1997,34,74);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('gracious_almeida',5,15,1994,27,111);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('focused_fermi',10,94,1991,51,145);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('cranky_spence',8,24,2000,2,91);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('reverent_nobel',2,92,2024,96,158);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('happy_northcutt',2,78,2017,19,35);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('hungry_mestorf',1,74,2002,17,82);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nervous_lumiere',5,86,1994,43,59);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nifty_shannon',10,10,1977,40,95);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('practical_albattani',5,54,1996,32,99);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('vigorous_hawking',1,67,2020,39,80);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('mystifying_einstein',7,46,2015,3,86);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('clever_noyce',8,55,2000,84,150);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('zealous_easley',6,8,2001,1,91);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('youthful_meninsky',2,68,1977,58,89);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('ecstatic_kalam',7,30,2024,22,81);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('cranky_hoover',7,76,2007,12,17);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('zen_benz',9,28,1991,33,79);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('musing_jennings',6,80,2024,98,159);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('youthful_lalande',9,96,2003,91,156);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('jovial_volhard',4,60,1999,89,106);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nifty_poitras',5,39,2021,22,27);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('determined_goldwasser',2,41,1996,27,84);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('vibrant_poincare',6,44,2011,69,102);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('xenodochial_yalow',8,101,1991,54,74);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('jolly_visvesvaraya',2,54,2018,75,142);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('happy_newton',4,16,2005,5,103);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('eloquent_bose',1,78,2015,28,39);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(1, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(10, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(10, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(10, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(10, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(24, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 7);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('distracted_sammet',6,8,2012,64,74);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('distracted_gates',4,8,2011,36,70);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('practical_joliot',4,2,2010,18,47);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('frosty_bose',2,4,2022,96,147);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('dreamy_curie',4,5,1984,67,107);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('affectionate_jennings',5,2,1994,78,158);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('heuristic_stonebraker',9,2,1977,86,185);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('brave_neumann',9,6,1980,1,16);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('zen_shockley',7,10,2009,16,98);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('clever_shannon',3,10,1981,34,96);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('competent_allen',10,9,1978,9,15);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('festive_hodgkin',6,7,1992,42,93);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('agitated_ardinghelli',8,9,2009,4,61);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('vigorous_neumann',9,1,1998,80,124);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('loving_beaver',5,1,2008,63,118);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('modest_cori',4,9,1990,99,186);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('wizardly_spence',4,10,1993,11,26);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('angry_panini',7,8,1989,11,31);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('gifted_kare',7,1,2005,12,66);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('happy_brahmagupta',8,5,2017,41,42);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('hardcore_lichterman',9,9,2021,85,150);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('focused_boyd',3,4,2003,96,120);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('gracious_ritchie',2,1,1978,54,61);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('romantic_torvalds',7,8,1981,35,132);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('thirsty_morse',6,4,2007,32,129);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('compassionate_blackwell',9,1,2006,8,39);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('determined_jepsen',4,2,1989,74,88);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('modest_ride',6,3,1992,88,103);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('jolly_joliot',5,4,2009,76,128);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('eloquent_torvalds',1,7,1980,20,97);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('affectionate_bassi',5,9,2021,61,85);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('suspicious_stallman',9,6,1992,3,65);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('eloquent_leavitt',9,9,2010,3,47);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('affectionate_volhard',3,2,1984,4,82);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('goofy_cori',8,6,2007,92,110);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('ecstatic_dubinsky',2,7,2017,95,102);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('heuristic_meninsky',10,7,2004,87,148);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('stoic_wing',8,6,2024,77,132);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('practical_euclid',3,6,2007,33,97);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('adoring_goodall',1,7,1996,81,177);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(4, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(28, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(1, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(1, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(1, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(2, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(2, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(35, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(37, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(37, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 2);
