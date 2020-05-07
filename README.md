
<p>
  <h3 align="center"> ABStock </h3>
  <h2 align="center"> Your catalogue online </h2>
</p>

<p align="center">
  <a href="https://gitlab.com/vindarel/abstock"><b>Homepage</b></a> |
  <a href="https://gitlab.com/vindarel/abstock#install"><b>Install</b></a> |
  <a href="https://framasphere.org/people/4ac5fae0bed90133a3ed2a0000053625"><b>Blog</b></a> |
  <a href="https://framavox.org/g/V6oiDr8Y/abelujo"><b>Forum</b></a> |
  <a href="https://liberapay.com/vindarel/donate"><b>Support us</b></a> |
  <a href="/README_fr.md">Français</a>

  Clients can now see your books and pass command.

</p>

Here's how searching the stock looks like:

![welcome screen](search.png "welcome screen")


The website features:

- a welcome screen, with:
  - the bookshop's information,
  - a search form,
  - a pre-selection of the books to showcase,
- a form to start searching books. A visitor can search by title, authors, publisher, shelf and ISBN(s).
- a shopping basket, for visitors to add books in
- a confirmation form, which sends the command by email to the shop owner,
- a special page to showcase a selection.

ABStock connects by default to the [Abelujo](http://abelujo.cc/)
database. Abelujo is a free software for bookshops, that manages
inventories, sells, and the like.

The database is loaded in memory at startup, doesn't access it
afterwards, and is synced several times a day.

Interested? Please get in touch.



## Issues and feature requests

Issue tracker: https://gitlab.com/vindarel/abstock/-/issues

GitHub mirror: https://github.com/vindarel/ABStock

Known TODOs:

* online Stripe payments
* admin panel
* read another DB / read products data from a CSV or a JSON file
* i18n / remove a few still hardcoded words

## Licence

GPLv3
