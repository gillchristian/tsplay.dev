alter table if exists shortened
  add column if not exists created_at timestamp not null default now(),
  add column if not exists last_visit timestamp not null default now(),
  add column if not exists expires boolean not null default TRUE;

update shortened set expires = FALSE;

create table if not exists stats (
  id serial primary key,
  links_created int,
  links_visited int,
  created_on_client int,
  created_on_plugin int,
  created_on_api int,
  created_on_other int
);

insert into stats (
  links_created,
  links_visited,
  created_on_client,
  created_on_plugin,
  created_on_api,
  created_on_other
) select count(*), sum(visits), 0, 0, 0, count(*) from shortened;
