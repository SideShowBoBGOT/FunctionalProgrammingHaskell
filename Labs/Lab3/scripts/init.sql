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
);INSERT INTO authors (name) VALUES ('Sleepy Chaplygin');
INSERT INTO authors (name) VALUES ('Pensive Nash');
INSERT INTO authors (name) VALUES ('Gracious Volhard');
INSERT INTO authors (name) VALUES ('Dreamy Pike');
INSERT INTO authors (name) VALUES ('Blissful Driscoll');
INSERT INTO authors (name) VALUES ('Unruffled Feistel');
INSERT INTO authors (name) VALUES ('Peaceful Chaum');
INSERT INTO authors (name) VALUES ('Boring Lovelace');
INSERT INTO authors (name) VALUES ('Trusting Vaughan');
INSERT INTO authors (name) VALUES ('Stoic Hodgkin');
INSERT INTO cities (name) VALUES ('Bassinghall');
INSERT INTO cities (name) VALUES ('Endleworth');
INSERT INTO cities (name) VALUES ('Wolcompwick');
INSERT INTO cities (name) VALUES ('Warney');
INSERT INTO cities (name) VALUES ('Dribnell');
INSERT INTO cities (name) VALUES ('Skellingmere');
INSERT INTO cities (name) VALUES ('Chipperney');
INSERT INTO cities (name) VALUES ('Wolvingbage');
INSERT INTO cities (name) VALUES ('Warringmerbridge');
INSERT INTO cities (name) VALUES ('Suddendthorne');
INSERT INTO publishers (name) VALUES ('epic_nobel');
INSERT INTO publishers (name) VALUES ('peaceful_ptolemy');
INSERT INTO publishers (name) VALUES ('inspiring_minsky');
INSERT INTO publishers (name) VALUES ('kind_roentgen');
INSERT INTO publishers (name) VALUES ('goofy_wiles');
INSERT INTO publishers (name) VALUES ('blissful_haibt');
INSERT INTO publishers (name) VALUES ('focused_yonath');
INSERT INTO publishers (name) VALUES ('mystifying_bell');
INSERT INTO publishers (name) VALUES ('thirsty_mcnulty');
INSERT INTO publishers (name) VALUES ('quirky_blackwell');
INSERT INTO journals (name) VALUES ('sleepy_elion');
INSERT INTO journals (name) VALUES ('upbeat_goldwasser');
INSERT INTO journals (name) VALUES ('zealous_mclean');
INSERT INTO journals (name) VALUES ('eloquent_lichterman');
INSERT INTO journals (name) VALUES ('loving_khorana');
INSERT INTO journals (name) VALUES ('zen_goodall');
INSERT INTO journals (name) VALUES ('heuristic_kalam');
INSERT INTO journals (name) VALUES ('naughty_boyd');
INSERT INTO journals (name) VALUES ('priceless_varahamihira');
INSERT INTO journals (name) VALUES ('awesome_leakey');
INSERT INTO conferences (name) VALUES ('flamboyant_goldwasser');
INSERT INTO conferences (name) VALUES ('ecstatic_darwin');
INSERT INTO conferences (name) VALUES ('nifty_darwin');
INSERT INTO conferences (name) VALUES ('awesome_mccarthy');
INSERT INTO conferences (name) VALUES ('priceless_pike');
INSERT INTO conferences (name) VALUES ('epic_brown');
INSERT INTO conferences (name) VALUES ('serene_lalande');
INSERT INTO conferences (name) VALUES ('serene_swirles');
INSERT INTO conferences (name) VALUES ('admiring_hopper');
INSERT INTO conferences (name) VALUES ('sad_chandrasekhar');
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('happy_chandrasekhar',9,6,2020);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('compassionate_kalam',2,3,1994);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('elastic_davinci',3,6,2022);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('ecstatic_poitras',4,10,2016);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cranky_lamarr',10,1,1989);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('quizzical_cray',8,3,2015);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('upbeat_turing',6,3,1998);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('goofy_mayer',5,2,2010);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('awesome_cray',7,9,2024);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('infallible_kirch',6,2,2001);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('frosty_mcclintock',5,1,1977);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('awesome_edison',3,6,2012);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('thirsty_carson',7,4,1984);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('kind_hodgkin',7,7,2009);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('amazing_meninsky',9,10,1991);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('wonderful_kowalevski',1,7,2015);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('hardcore_brattain',5,5,2009);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('fervent_snyder',4,5,2010);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('focused_williams',3,4,2019);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('determined_galileo',9,6,2006);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('tender_varahamihira',1,9,1990);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('relaxed_blackwell',3,5,2005);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('condescending_agnesi',4,8,2000);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('jovial_hodgkin',2,5,2012);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('vibrant_stonebraker',1,4,1992);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('kind_bell',9,9,1986);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('upbeat_davinci',5,9,2018);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('agitated_leavitt',10,4,1988);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('agitated_swirles',5,8,2020);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('practical_cray',8,7,2023);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('jolly_thompson',6,7,2013);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('keen_boyd',7,4,2012);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('clever_bose',2,3,2011);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cocky_mahavira',10,10,2017);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('modest_wozniak',5,10,2011);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('silly_bassi',5,7,1982);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('tender_noether',4,10,1982);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('serene_lumiere',8,3,1991);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('relaxed_allen',9,4,2012);
INSERT INTO books (title, city_id, publisher_id, year) VALUES ('cocky_lumiere',10,2,2022);
INSERT INTO book_authors (book_id, author_id) VALUES(34, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(7, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(10, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(15, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(24, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(1, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(2, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(3, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(4, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(5, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(6, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(8, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(9, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(11, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(12, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(13, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(14, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(16, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(17, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(18, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(19, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(20, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(20, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(21, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(21, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(22, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(23, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(25, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(26, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(27, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(28, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(29, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(30, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(31, 5);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 4);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(32, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(33, 1);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(35, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(36, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(37, 8);
INSERT INTO book_authors (book_id, author_id) VALUES(38, 6);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 2);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 3);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 9);
INSERT INTO book_authors (book_id, author_id) VALUES(39, 4);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('hardcore_ritchie',6,74,1994,52,74);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('reverent_blackwell',4,16,2013,99,130);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('hungry_tesla',1,101,2010,70,92);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('elated_jennings',4,70,2022,57,106);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('optimistic_visvesvaraya',4,29,2016,88,152);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('jovial_bassi',2,81,2018,29,122);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('boring_wescoff',3,12,1998,31,99);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('zealous_lewin',9,43,2019,30,38);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('laughing_dubinsky',3,100,2003,46,91);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('hardcore_nobel',3,48,2000,82,112);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('flamboyant_lichterman',1,75,1996,72,123);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('compassionate_brown',5,91,1979,37,62);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('mystifying_stallman',5,62,1985,5,40);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('tender_lichterman',2,39,1990,17,39);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('unruffled_hawking',7,81,1977,85,93);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('cocky_meninsky',3,78,2007,94,159);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('serene_brahmagupta',6,82,2000,8,100);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('thirsty_mestorf',6,30,1989,81,158);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('upbeat_brattain',4,75,1989,10,42);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('angry_curie',5,20,2014,88,138);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('vibrant_hopper',6,35,2006,81,152);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('unruffled_goldberg',6,66,1985,39,49);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('tender_bose',7,23,2006,56,105);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('youthful_mcnulty',7,56,1998,12,109);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('xenodochial_mestorf',2,77,1990,54,135);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('quizzical_lichterman',6,33,1996,75,118);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('quirky_visvesvaraya',7,70,1997,57,94);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('suspicious_volhard',3,100,2007,98,112);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('heuristic_tesla',2,28,1992,76,118);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('boring_cray',10,35,1991,70,138);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('pedantic_meitner',7,101,2005,45,134);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('compassionate_nobel',2,63,2005,23,100);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('objective_aryabhata',6,70,2015,15,60);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('fervent_benz',1,61,2011,86,121);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('quirky_turing',2,83,1984,3,62);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('quizzical_mcclintock',8,98,2013,48,113);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('peaceful_bose',8,25,2016,59,105);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('wizardly_joliot',4,64,2001,16,96);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('gracious_curran',4,23,1979,31,122);
INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)VALUES ('awesome_ramanujan',7,83,2010,95,138);
INSERT INTO article_authors (article_id, author_id) VALUES(32, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(36, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(39, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(20, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(29, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(1, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(2, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(3, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(4, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(5, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(6, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(7, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(8, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(9, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(10, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(10, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(11, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(12, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(13, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(14, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(15, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(16, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(17, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(18, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(19, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(21, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(22, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(23, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(24, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(25, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(26, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(27, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 3);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(28, 4);
INSERT INTO article_authors (article_id, author_id) VALUES(30, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(31, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 7);
INSERT INTO article_authors (article_id, author_id) VALUES(33, 8);
INSERT INTO article_authors (article_id, author_id) VALUES(34, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 2);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(35, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(37, 1);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 6);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 5);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 9);
INSERT INTO article_authors (article_id, author_id) VALUES(38, 8);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('flamboyant_booth',1,10,1989,83,113);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('elated_goodall',7,4,2019,94,181);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('wonderful_kare',10,5,1987,61,118);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('vigilant_lewin',3,7,1999,64,157);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('inspiring_ride',4,3,1988,64,125);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('stupefied_ardinghelli',10,3,1997,7,67);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('zen_minsky',5,2,1977,84,158);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('admiring_fermat',9,8,1992,99,181);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('focused_nobel',10,7,2018,91,161);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('amazing_noether',9,10,2016,51,108);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('thirsty_leakey',5,4,2011,29,124);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('elastic_wozniak',5,10,2008,56,104);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('pensive_newton',7,8,1998,7,29);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('compassionate_hypatia',1,4,1995,36,72);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('stoic_euler',1,2,2023,70,161);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('objective_golick',9,1,2011,16,68);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('jolly_williams',10,3,1981,36,63);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('affectionate_shockley',1,2,1984,4,6);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('agitated_euclid',2,8,2018,78,106);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('modest_roentgen',4,5,2009,55,81);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('relaxed_pasteur',1,3,2022,36,112);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('eloquent_nightingale',2,2,2011,48,87);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('agitated_kalam',2,5,2019,77,79);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('competent_jepsen',2,9,1983,27,105);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('inspiring_ardinghelli',9,9,2010,61,101);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('hungry_khorana',10,10,2000,28,64);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('wonderful_booth',3,1,1997,12,108);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('infallible_mestorf',2,10,2009,49,108);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('boring_nobel',3,7,1982,73,103);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('wizardly_jepsen',5,3,1987,4,71);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('ecstatic_meitner',9,8,2011,66,85);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('unruffled_perlman',6,3,1989,81,85);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('cranky_knuth',10,1,2019,47,124);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('frosty_cori',4,5,1978,28,76);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('optimistic_saha',3,5,1979,76,132);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('agitated_jackson',5,3,2024,15,58);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('lucid_hugle',4,9,1990,16,64);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('affectionate_sammet',1,1,2005,76,135);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('adoring_rosalind',9,6,2020,90,186);
INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)VALUES ('wizardly_goldwasser',7,2,1989,76,135);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(2, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(37, 2);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(38, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(16, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(23, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(1, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(1, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(1, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(3, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(4, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(4, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(4, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(5, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(6, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(7, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(8, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(9, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(10, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(11, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(12, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(13, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(14, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(15, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(17, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(18, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(19, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(20, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(21, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(22, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(24, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(25, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(26, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(27, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(28, 5);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(29, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(30, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(31, 7);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(32, 8);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(33, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(34, 4);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(35, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 9);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(36, 1);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 6);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 3);
INSERT INTO thesis_authors (thesis_id, author_id) VALUES(39, 9);
