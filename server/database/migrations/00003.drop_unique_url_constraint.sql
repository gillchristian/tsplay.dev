-- No need to index the urls. Since they might be too long,
-- it fails to add them due to this constraint
alter table if exists shortened
  drop constraint shortened_url_key;
