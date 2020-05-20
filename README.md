
<p>
  <h3 align="center"> ABStock </h3>
  <h2 align="center"> Catalogue and commands online </h2>
</p>

<p align="center">
  <a href="http://abstock.gitlab.io"><b>Homepage</b></a> |
  <a href="https://abstock.gitlab.io/#/en/install"><b>Install</b></a> |
  <a href="https://framasphere.org/people/4ac5fae0bed90133a3ed2a0000053625"><b>Blog</b></a> |
  <a href="https://framavox.org/g/V6oiDr8Y/abelujo"><b>Forum</b></a> |
  <a href="https://www.patreon.com/vindarel"><b>Support us on Patreno</b></a> |
  <a href="https://liberapay.com/vindarel/donate"><b>Support us on Liberapay</b></a> |
  <a href="/README_fr.md">Français</a>

  Clients can now discover your stock and shop online.

</p>

ABStock was developed during the global lock-down to help a bookshop
keep an activity and a link with its clients. It proved 100%
useful. You can have a site on the same model.

[Install it yourself](/docs/en/install.md) or ask us. Contact us at `contact@abelujo.cc`.

The website features by default the following pages, all customizable:

- a welcome screen, with:
  - the bookshop's information,
  - a search form,
  - a pre-selection of the books to showcase,
- a form to start searching books. A visitor can search by title, authors, publisher, shelf and ISBN(s).
- a shopping basket, for visitors to add books in
- a confirmation form, which sends the command by email to the shop owner,
- a special page to showcase a selection.

Here's how searching the stock looks like:

![welcome screen](search.png "welcome screen")


ABStock can load data from several places:

- by default, it connects to the [Abelujo](http://abelujo.cc/)
database. Abelujo is a free software for bookshops, that manages
inventories, sells, and the like. The database is loaded in memory at
startup, doesn't access it afterwards, and is synced several times a
day.
- it can load data stored in a simple txt format (see
  `cards-example.txt`). You can define books, or other products:

![](other-data.png)

Interested? Please get in touch.

### Other features

- translate URLs ("/book/<slug>" or "/livre/<slug>")
- redefine anything with the pre- and post-config files, written in the programming language of the application (Lisp)


## Issues and feature requests

Issue tracker: https://gitlab.com/vindarel/abstock/-/issues

GitHub mirror: https://github.com/vindarel/ABStock

Known TODOs:

* admin panel
* [X] read products data from a TXT, CSV or a JSON file
* i18n / remove a few still hardcoded words
* online Stripe payments
* simple stats

## Licence

GPLv3

---

Lisp?! Oh yes, Lisp.

* https://lisp-journey.gitlab.io/pythonvslisp/
* https://lisp-lang.org/success/
* https://common-lisp.net/
* https://github.com/CodyReichert/awesome-cl
* https://lispcookbook.github.io/cl-cookbook/

<a href="https://www.patreon.com/bePatron?u=35783903" data-patreon-widget-type="become-patron-button">Become a Patron!</a><script async src="https://c6.patreon.com/becomePatronButton.bundle.js"></script>
