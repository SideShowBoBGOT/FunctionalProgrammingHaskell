from names_generator import generate_name
from city_name import generate_city_name
from some_name import get_random_name

import random
import os


def give_authors(authors_num: int, val_num: int, table_name: str):
    if authors_num < 10:
        raise ValueError
    if val_num < 10:
        raise ValueError

    single_auth_len = int(authors_num * 0.5)
    author_ids = set([i for i in range(1, authors_num)])
    single_author_ids = random.sample(author_ids, single_auth_len)

    val_ids = set([i for i in range(1, books_num)])
    single_val_ids = set(random.sample(val_ids, single_auth_len))

    for (val_id, author_id) in zip(single_val_ids, single_author_ids):
        file.write(f'INSERT INTO {table_name}_authors ({table_name}_id, author_id) VALUES({val_id}, {author_id});\n')

    other_author_ids = author_ids - set(single_val_ids)
    other_book_ids = val_ids - single_val_ids

    for val_id in other_book_ids:
        sample_other_ids = random.sample(other_author_ids, random.randint(1, 4))
        for auth_id in sample_other_ids:
            file.write(f'INSERT INTO {table_name}_authors ({table_name}_id, author_id) VALUES({val_id}, {auth_id});\n')


with open("out.sql", 'w') as file:
    authors_num = 10
    for _ in range(0, authors_num):
        file.write(f'INSERT INTO authors (name) VALUES (\'{generate_name(style="capital")}\');\n')

    cities_num = 10
    publishers_num = 10
    journals_num = 10
    conferences_num = 10

    for _ in range(0, cities_num):
        file.write(f'INSERT INTO cities (name) VALUES (\'{generate_city_name()}\');\n')
    for _ in range(0, publishers_num):
        file.write(f'INSERT INTO publishers (name) VALUES (\'{get_random_name()}\');\n')
    for _ in range(0, journals_num):
        file.write(f'INSERT INTO journals (name) VALUES (\'{get_random_name()}\');\n')
    for _ in range(0, conferences_num):
        file.write(f'INSERT INTO conferences (name) VALUES (\'{get_random_name()}\');\n')

    books_num = 40

    for _ in range(0, books_num):
        file.write(f'INSERT INTO books (title, city_id, publisher_id, year) '
                   f'VALUES (\'{get_random_name()}\','
                   f'{random.randint(1, cities_num)},'
                   f'{random.randint(1, publishers_num)},'
                   f'{random.randint(1977, 2024)});\n')

    give_authors(authors_num, books_num, 'book')

    articles_len = 40

    for _ in range(0, articles_len):
        pages_start = random.randint(1, 101)
        pages_end = pages_start + random.randint(1, 101)
        file.write(f'INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end)'
                   f'VALUES (\'{get_random_name()}\','
                   f'{random.randint(1, journals_num)},'
                   f'{random.randint(1, 101)},'
                   f'{random.randint(1977, 2024)},'
                   f'{pages_start},'
                   f'{pages_end});\n')

    give_authors(authors_num, articles_len, 'article')

    these_len = 40

    for _ in range(0, articles_len):
        pages_start = random.randint(1, 101)
        pages_end = pages_start + random.randint(1, 101)
        file.write(f'INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end)'
                   f'VALUES (\'{get_random_name()}\','
                   f'{random.randint(1, cities_num)},'
                   f'{random.randint(1, conferences_num)},'
                   f'{random.randint(1977, 2024)},'
                   f'{pages_start},'
                   f'{pages_end});\n')

    give_authors(authors_num, these_len, 'thesis')

os.system('cat tables.sql out.sql > init.sql')
