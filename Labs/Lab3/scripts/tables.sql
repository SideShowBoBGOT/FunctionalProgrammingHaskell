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
$$ LANGUAGE plpgsql;