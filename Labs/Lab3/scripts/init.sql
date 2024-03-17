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
    FOREIGN KEY (article_id) REFERENCES articles(id),
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
    FOREIGN KEY (thesis_id) REFERENCES theses(id),
    FOREIGN KEY (author_id) REFERENCES authors(id),
    PRIMARY KEY (thesis_id, author_id)
);INSERT INTO authors (name) VALUES ('Ecstatic Gauss');
INSERT INTO authors (name) VALUES ('Competent Austin');
INSERT INTO authors (name) VALUES ('Serene Darwin');
INSERT INTO authors (name) VALUES ('Epic Hoover');
INSERT INTO authors (name) VALUES ('Jolly Kepler');
INSERT INTO authors (name) VALUES ('Sweet Ishizaka');
INSERT INTO authors (name) VALUES ('Happy Austin');
INSERT INTO authors (name) VALUES ('Wizardly Shannon');
INSERT INTO authors (name) VALUES ('Epic Wu');
INSERT INTO authors (name) VALUES ('Charming Lamport');
INSERT INTO cities (name) VALUES ('Hansmere');
INSERT INTO cities (name) VALUES ('Hamtree');
INSERT INTO cities (name) VALUES ('Redborough');
INSERT INTO cities (name) VALUES ('Blandlescompbourne');
INSERT INTO cities (name) VALUES ('Watchshot');
INSERT INTO cities (name) VALUES ('Hamdennell');
INSERT INTO cities (name) VALUES ('Chilwich');
INSERT INTO cities (name) VALUES ('Cirerhead');
INSERT INTO cities (name) VALUES ('Rockport');
INSERT INTO cities (name) VALUES ('Flitlake');
INSERT INTO publishers (name) VALUES ('trusting_knuth');
INSERT INTO publishers (name) VALUES ('trusting_shaw');
INSERT INTO publishers (name) VALUES ('compassionate_beaver');
INSERT INTO publishers (name) VALUES ('pedantic_heyrovsky');
INSERT INTO publishers (name) VALUES ('determined_curran');
INSERT INTO publishers (name) VALUES ('hardcore_mestorf');
INSERT INTO publishers (name) VALUES ('modest_allen');
INSERT INTO publishers (name) VALUES ('epic_poitras');
INSERT INTO publishers (name) VALUES ('priceless_lalande');
INSERT INTO publishers (name) VALUES ('xenodochial_hamilton');
INSERT INTO journals (name) VALUES ('relaxed_lovelace');
INSERT INTO journals (name) VALUES ('quizzical_visvesvaraya');
INSERT INTO journals (name) VALUES ('xenodochial_shirley');
INSERT INTO journals (name) VALUES ('goofy_newton');
INSERT INTO journals (name) VALUES ('boring_tesla');
INSERT INTO journals (name) VALUES ('dreamy_yalow');
INSERT INTO journals (name) VALUES ('boring_ritchie');
INSERT INTO journals (name) VALUES ('hardcore_visvesvaraya');
INSERT INTO journals (name) VALUES ('gallant_pare');
INSERT INTO journals (name) VALUES ('musing_shannon');
INSERT INTO conferences (name) VALUES ('infallible_boyd');
INSERT INTO conferences (name) VALUES ('quirky_perlman');
INSERT INTO conferences (name) VALUES ('romantic_tesla');
INSERT INTO conferences (name) VALUES ('jolly_goldstine');
INSERT INTO conferences (name) VALUES ('frosty_shockley');
INSERT INTO conferences (name) VALUES ('peaceful_wescoff');
INSERT INTO conferences (name) VALUES ('flamboyant_fermat');
INSERT INTO conferences (name) VALUES ('dreamy_bartik');
INSERT INTO conferences (name) VALUES ('elastic_swanson');
INSERT INTO conferences (name) VALUES ('hopeful_liskov');
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('mystifying_mclean',8,6,2021);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('hardcore_curie',7,4,2016);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('nervous_euclid',9,9,2007);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('tender_albattani',2,6,2001);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('loving_pasteur',5,9,2014);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('goofy_mahavira',4,6,1980);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('youthful_bassi',2,7,2012);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('lucid_ritchie',9,4,1977);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('mystifying_babbage',7,1,2016);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('eager_golick',1,8,2013);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('elegant_mclean',4,1,2013);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('gracious_mcnulty',4,8,1979);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('agitated_yalow',1,6,2000);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('sad_bartik',3,4,1985);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('loving_heisenberg',2,3,1998);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('nostalgic_tesla',10,5,1996);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('practical_elion',3,3,2001);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('quirky_jennings',7,8,2010);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('fervent_curran',8,5,2001);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('zen_brown',4,1,2016);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('stupefied_kirch',10,5,1979);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('sleepy_albattani',2,7,2006);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cocky_wozniak',10,2,2015);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cocky_shockley',8,3,2004);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('elated_mayer',4,10,2005);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('mystifying_brattain',7,9,1993);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('goofy_curran',9,7,1997);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('goofy_morse',10,9,1985);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('nifty_austin',9,9,2011);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('hopeful_wing',3,10,2016);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('modest_neumann',5,4,1989);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('eager_wiles',4,9,1977);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('admiring_newton',9,8,2020);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('lucid_newton',6,8,2019);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('naughty_shaw',6,8,2014);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('agitated_kowalevski',2,4,2017);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('heuristic_clarke',9,4,2017);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('unruffled_kare',9,6,2013);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('compassionate_murdock',2,5,2018);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('hardcore_jones',7,1,1991);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(10, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(20, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(7, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(7, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(7, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(15, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(15, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(15, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(21, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(21, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(34, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(34, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 9);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('suspicious_booth',6,73,2004,15,88);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('kind_lamarr',9,79,1998,91,117);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('wonderful_nightingale',5,93,2017,40,44);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('unruffled_noyce',4,62,2002,8,38);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('gallant_einstein',10,16,2000,43,72);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('amazing_nobel',8,69,1990,36,60);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('modest_yonath',7,19,1978,43,60);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('amazing_mayer',5,55,1986,71,169);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('musing_murdock',2,61,1978,93,189);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('zealous_hermann',1,65,2018,6,42);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('zen_jang',8,52,1986,84,158);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('infallible_poitras',6,92,2021,44,123);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('stupefied_clarke',1,8,2024,81,173);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('hungry_meitner',8,3,2012,20,24);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('compassionate_hamilton',7,11,1994,71,96);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('compassionate_shockley',10,15,1994,94,144);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('zen_feynman',10,42,2024,95,164);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('eloquent_hypatia',3,9,2009,2,25);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('amazing_jennings',9,12,2020,16,59);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('trusting_almeida',3,13,1982,61,97);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('jolly_jones',5,73,2009,90,130);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('xenodochial_poitras',10,93,2006,48,57);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('hardcore_brown',3,2,1994,14,52);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('pedantic_turing',4,43,2019,61,75);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('kind_ardinghelli',1,68,2011,92,127);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('tender_morse',6,74,1978,43,47);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('romantic_almeida',3,1,1979,97,122);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('affectionate_newton',1,97,1984,84,151);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('laughing_lumiere',5,13,1986,12,58);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('peaceful_golick',7,48,1990,75,136);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('pedantic_lalande',1,9,2018,85,141);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('gifted_kowalevski',5,66,1996,34,128);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('modest_heyrovsky',10,31,1999,48,142);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('pensive_torvalds',4,65,2021,35,97);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('keen_ritchie',2,8,2013,93,143);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('gifted_lamarr',2,3,1983,85,125);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('dazzling_bohr',4,92,2001,55,144);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nostalgic_brown',10,65,1984,84,102);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nifty_leavitt',10,35,2011,28,69);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('relaxed_archimedes',8,85,1985,45,93);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(1, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(1, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(10, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(24, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(24, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(24, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(24, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 2);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('pedantic_keller',6,5,2017,4,20);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('inspiring_visvesvaraya',1,4,2016,68,163);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('festive_curran',4,5,2000,73,145);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('nervous_kalam',9,5,2023,35,78);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('musing_goodall',3,6,2018,33,93);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('focused_aryabhata',8,3,2019,18,53);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('nifty_hugle',5,8,2014,63,131);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('silly_poitras',10,1,2002,82,127);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('heuristic_swartz',8,10,1978,71,85);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('eager_ptolemy',3,6,1984,72,116);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('adoring_swanson',8,7,1978,46,129);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('youthful_ritchie',5,1,2014,90,176);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('unruffled_brahmagupta',2,9,2019,51,58);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('xenodochial_visvesvaraya',9,6,1987,43,135);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('sleepy_kepler',8,9,1999,1,24);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('compassionate_yonath',1,4,1989,26,40);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('frosty_sammet',4,9,1988,46,86);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('ecstatic_panini',7,4,1978,67,150);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('serene_liskov',5,2,1989,49,113);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('serene_davinci',3,7,1997,22,97);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('musing_lamarr',2,10,1987,85,143);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('compassionate_gates',10,4,1982,27,99);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('wonderful_ritchie',9,9,2020,39,129);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('upbeat_aryabhata',4,8,1978,99,126);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('goofy_volhard',5,4,1998,39,135);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('gifted_curie',10,5,2023,29,100);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('inspiring_pasteur',10,3,2010,49,122);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('sharp_franklin',2,9,1989,42,121);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('hungry_goldstine',7,6,1984,88,98);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('dreamy_hermann',3,5,1985,9,89);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('brave_williams',8,5,2010,78,170);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('reverent_euclid',1,8,1998,75,157);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('flamboyant_meninsky',5,9,2001,71,94);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('priceless_euler',8,2,1993,29,112);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('brave_torvalds',4,5,2021,82,102);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('dreamy_mcclintock',10,2,1996,71,171);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('trusting_franklin',10,9,1987,52,126);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('boring_aryabhata',3,7,2015,68,101);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('determined_heyrovsky',2,8,1986,94,160);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('boring_lichterman',10,8,2005,68,143);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(1, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(2, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(4, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(28, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(28, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(28, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(28, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(35, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(35, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(37, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(37, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 4);
