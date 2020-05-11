create table shortened (
  id serial primary key,
  short varchar (50) unique not null,
  url text unique not null,
  visits integer default 0
);

create table hash_counter (
  id serial PRIMARY KEY,
  counter integer default 0
);

insert into hash_counter (counter) values (0);
