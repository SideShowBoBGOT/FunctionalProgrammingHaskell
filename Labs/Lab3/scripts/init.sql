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
    FOREIGN KEY (book_id) REFERENCES books(id),
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

CREATE TABLE article_journals (
    article_id INTEGER NOT NULL,
    journal_id INTEGER NOT NULL,
    FOREIGN KEY (article_id) REFERENCES articles(id),
    FOREIGN KEY (journal_id) REFERENCES journals(id),
    PRIMARY KEY (article_id, journal_id)
);

CREATE TABLE conferences (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL
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
);INSERT INTO authors (name) VALUES ('Busy Bouman');
INSERT INTO authors (name) VALUES ('Wizardly Morse');
INSERT INTO authors (name) VALUES ('Peaceful Grothendieck');
INSERT INTO authors (name) VALUES ('Hardcore Knuth');
INSERT INTO authors (name) VALUES ('Eloquent Nash');
INSERT INTO authors (name) VALUES ('Compassionate Shannon');
INSERT INTO authors (name) VALUES ('Modest Yalow');
INSERT INTO authors (name) VALUES ('Ecstatic Keller');
INSERT INTO authors (name) VALUES ('Modest Villani');
INSERT INTO authors (name) VALUES ('Fervent Colden');
INSERT INTO cities (name) VALUES ('Milstoke');
INSERT INTO cities (name) VALUES ('Chiptree');
INSERT INTO cities (name) VALUES ('Rotherey');
INSERT INTO cities (name) VALUES ('Rot on Smith');
INSERT INTO cities (name) VALUES ('Hartbasbage');
INSERT INTO cities (name) VALUES ('Egrensavon');
INSERT INTO cities (name) VALUES ('Warringavon');
INSERT INTO cities (name) VALUES ('Endleham');
INSERT INTO cities (name) VALUES ('Faldinghall');
INSERT INTO cities (name) VALUES ('Wychney');
INSERT INTO publishers (name) VALUES ('epic_hodgkin');
INSERT INTO publishers (name) VALUES ('suspicious_kalam');
INSERT INTO publishers (name) VALUES ('brave_lumiere');
INSERT INTO publishers (name) VALUES ('stupefied_benz');
INSERT INTO publishers (name) VALUES ('hopeful_saha');
INSERT INTO publishers (name) VALUES ('ecstatic_davinci');
INSERT INTO publishers (name) VALUES ('confident_beaver');
INSERT INTO publishers (name) VALUES ('reverent_visvesvaraya');
INSERT INTO publishers (name) VALUES ('admiring_franklin');
INSERT INTO publishers (name) VALUES ('clever_brown');
INSERT INTO journals (name) VALUES ('dreamy_feynman');
INSERT INTO journals (name) VALUES ('festive_wescoff');
INSERT INTO journals (name) VALUES ('zealous_archimedes');
INSERT INTO journals (name) VALUES ('distracted_bose');
INSERT INTO journals (name) VALUES ('boring_nightingale');
INSERT INTO journals (name) VALUES ('blissful_williams');
INSERT INTO journals (name) VALUES ('focused_swanson');
INSERT INTO journals (name) VALUES ('adoring_shockley');
INSERT INTO journals (name) VALUES ('hopeful_payne');
INSERT INTO journals (name) VALUES ('musing_mcnulty');
INSERT INTO conferences (name) VALUES ('friendly_chandrasekhar');
INSERT INTO conferences (name) VALUES ('jovial_minsky');
INSERT INTO conferences (name) VALUES ('eloquent_mahavira');
INSERT INTO conferences (name) VALUES ('trusting_hawking');
INSERT INTO conferences (name) VALUES ('romantic_benz');
INSERT INTO conferences (name) VALUES ('pedantic_mcnulty');
INSERT INTO conferences (name) VALUES ('dreamy_murdock');
INSERT INTO conferences (name) VALUES ('zealous_bhaskara');
INSERT INTO conferences (name) VALUES ('romantic_mestorf');
INSERT INTO conferences (name) VALUES ('optimistic_yalow');
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('youthful_banach',9,10,1981);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('modest_johnson',5,1,2009);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('boring_northcutt',5,9,2008);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('nostalgic_borg',7,2,1979);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('distracted_hawking',8,1,1987);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('agitated_poitras',7,7,2015);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('mystifying_liskov',2,6,2014);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('suspicious_bell',4,3,1990);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('boring_easley',6,3,1997);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('heuristic_albattani',1,4,2002);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('infallible_lewin',5,7,1991);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cocky_raman',7,9,2004);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('gallant_lumiere',9,2,2019);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('vigorous_visvesvaraya',9,6,1999);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('affectionate_darwin',8,9,2021);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('angry_thompson',10,7,2018);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('wizardly_goodall',4,3,2022);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('sharp_williams',5,2,2022);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('wonderful_archimedes',1,3,2008);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('serene_snyder',2,7,2016);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('upbeat_jepsen',6,8,2000);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('jovial_kalam',3,2,1984);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('gifted_bassi',3,6,2009);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('jolly_allen',3,2,1980);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('modest_heisenberg',4,9,1986);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('festive_boyd',6,5,2002);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('silly_jackson',4,3,1993);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('trusting_carson',10,9,2014);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('condescending_mcnulty',8,7,1988);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('adoring_pare',10,9,2000);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('vigorous_visvesvaraya',9,5,2004);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('jolly_benz',10,3,1992);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('musing_mccarthy',7,5,2022);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('determined_chandrasekhar',1,1,2010);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cocky_curran',3,3,1989);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('fervent_thompson',1,8,1999);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('serene_noyce',8,3,1999);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('infallible_benz',7,3,1977);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('agitated_rosalind',8,9,1996);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('festive_goldstine',4,10,2009);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(7, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(7, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(10, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(10, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(15, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(15, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(20, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(21, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(21, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(34, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(34, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(34, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 7);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 2);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('happy_pare',5,21,1998,89,185);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('peaceful_blackwell',5,12,1985,91,110);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nifty_ptolemy',7,65,2007,21,118);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('happy_carson',10,97,2016,99,191);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('confident_archimedes',6,14,2019,31,47);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('dreamy_brattain',7,57,2005,49,147);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('vigilant_ride',2,40,1998,65,128);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('relaxed_kalam',10,82,1978,38,99);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('flamboyant_payne',10,86,2018,64,91);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('goofy_meitner',10,46,1986,90,93);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nostalgic_jennings',4,22,2014,2,27);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('epic_darwin',8,82,2012,22,83);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('brave_benz',9,18,1978,34,94);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('modest_noether',3,25,2020,81,153);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nostalgic_raman',7,18,2017,48,116);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nifty_hamilton',3,43,2022,33,82);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('keen_kirch',10,69,2020,11,96);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('elegant_bartik',3,75,2010,1,86);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('epic_curie',5,96,1990,27,61);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('musing_wilson',7,95,2000,23,104);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('stupefied_nightingale',10,12,2022,94,195);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('sharp_leavitt',6,34,1999,34,122);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('clever_kalam',1,27,1999,46,126);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('quirky_yalow',4,83,1990,94,156);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('nostalgic_johnson',6,66,1996,23,100);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('quirky_ramanujan',6,73,2003,99,120);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('sharp_murdock',9,27,2020,32,35);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('quirky_mahavira',2,60,1990,84,181);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('jolly_bhaskara',7,24,1993,85,140);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('affectionate_lamarr',2,81,1990,82,116);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('adoring_elion',6,85,2019,55,80);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('brave_banach',1,24,1990,84,113);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('elastic_yalow',3,59,1977,63,109);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('determined_mirzakhani',1,57,2003,21,36);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('hopeful_leavitt',6,74,1988,55,56);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('admiring_haibt',4,90,1980,99,188);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('epic_ride',6,58,2020,6,37);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('competent_shockley',8,70,1997,79,99);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('goofy_darwin',5,86,2011,71,152);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('zen_davinci',4,90,2012,87,106);
INSERT INTO article_authors (article_id, author_id) VALUES(1, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(10, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(24, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 5);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('suspicious_shannon',8,4,1995,70,169);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('flamboyant_lamarr',6,8,2020,71,163);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('silly_beaver',8,10,1986,80,157);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('friendly_kalam',9,3,1990,40,53);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('awesome_euclid',8,3,1980,43,93);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('nostalgic_sinoussi',1,4,1987,95,164);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('practical_clarke',6,1,2012,62,137);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('zealous_euclid',1,7,2006,7,94);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('serene_goldstine',7,1,2012,52,74);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('zealous_lamport',10,10,2005,95,149);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('naughty_poitras',3,2,2010,21,51);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('brave_wing',9,2,2016,45,80);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('jovial_yalow',5,1,1990,94,184);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('confident_edison',6,2,1988,98,116);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('zealous_ardinghelli',7,5,1979,76,139);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('cocky_hamilton',9,4,1986,18,98);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('trusting_franklin',4,6,1979,71,86);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('hungry_kalam',3,8,2006,31,49);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('vibrant_turing',7,5,1997,55,130);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('vigorous_newton',8,5,1999,61,98);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('tender_stonebraker',10,3,2013,56,123);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('eager_brattain',1,5,1987,48,138);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('condescending_heisenberg',7,5,2023,13,108);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('eager_engelbart',10,1,2020,68,127);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('competent_minsky',1,2,1977,22,24);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('eager_payne',2,9,1980,11,86);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('vigorous_wiles',1,10,2010,20,78);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('dazzling_edison',1,9,1988,100,124);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('quirky_albattani',1,3,1982,98,164);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('loving_goodall',9,5,2014,61,65);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('festive_galileo',3,1,2023,49,148);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('adoring_montalcini',3,8,1996,74,114);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('sad_wiles',10,9,2015,40,75);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('clever_fermat',2,2,1988,64,112);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('stupefied_williams',4,9,2021,33,133);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('amazing_lumiere',6,3,2020,24,77);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('wizardly_hugle',6,7,1988,30,52);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('pedantic_swanson',7,4,1995,100,101);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('priceless_bohr',10,1,1986,15,110);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('awesome_engelbart',7,9,1991,2,4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(4, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(1, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(2, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(2, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(2, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(2, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(28, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(28, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(35, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(35, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(37, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 1);
