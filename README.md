[![Gitter](https://badges.gitter.im/openbookstore-developers/community.svg)](https://gitter.im/openbookstore-developers/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

<p>
  <h2 align="center"> ABStock </h2>
  <h3 align="center"> Catalogue and commands online </h3>
</p>

<p align="center">
  <a href="http://abstock.gitlab.io"><b>Homepage</b></a> |
  <a href="https://abstock.gitlab.io/#/en/install"><b>Install</b></a> |
  <a href="https://framasphere.org/people/4ac5fae0bed90133a3ed2a0000053625"><b>Blog</b></a> |
  <a href="https://gitter.im/openbookstore-developers/community?utm_source=share-link&utm_medium=link&utm_campaign=share-link"><b>Gitter chat</b></a> |
  <a href="https://www.patreon.com/abelujo"><b>Support us on Patreon</b></a> |
  <a href="https://liberapay.com/vindarel/donate"><b>Support us on Liberapay</b></a> |
  <a href="https://ko-fi.com/vindarel"><b>Buy me a coffee!</b></a> |
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

<a href='https://ko-fi.com/K3K828W0V' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://cdn.ko-fi.com/cdn/kofi2.png?v=2' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>

### Other features

- translate URLs ("/book/<slug>" or "/livre/<slug>")
- redefine anything with the pre- and post-config files, written in the programming language of the application (Lisp)

## Install

ABStock is known to work on:

- SBCL
- CCL


### Quick install on Debian Buster 10

update: you can try a self-contained binary for Debian Buster. [Download it here](https://gitlab.com/vindarel/abstock/-/jobs/artifacts/master/raw/abstock?job=build). Download it and run it with

    ./abstock

You do *not* need to install a Lisp implementation.
If you use it, please give us feedback [here](https://gitlab.com/vindarel/abstock/-/issues/8). Thanks!


Here's the universal recipe to run ABStock from sources:

```bash
# create a user for abstock or reuse the user of abelujo
apt install rlwrap sbcl cl-quicklisp git make
git clone https://gitlab.com/vindarel/abstock.git
cd abstock
# Copy or ln you db.db sqlite from abelujo
ln -s /home/abelujo/repo/db.db db.db

sbcl --load /usr/share/cl-quicklisp/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --eval '(quit)'
# or /usr/share/common-lisp/source/quicklisp/quicklisp.lisp on Debian 10.
make deps
# install it and run it with
make run
# aka
# rlwrap sbcl --load run.lisp --eval '(in-package :abstock)'
# use ctrl d to exit
```

### Quick install on a Raspberry Pi

SBCL lacks thread support on ARM 32 bits so we'll use Clozure Common
Lisp. It's easy to install and its compilation times are stellar.

```bash
# in root:
cd /usr/local/src/
wget https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-linuxarm.tar.gz
tar -xvze ccl-1.11.5-linuxarm.tar.gz
cp ccl/scripts/ccl /usr/local/bin/ccl
rm ccl-1.11.5-linuxarm.tar.gz

# in normal (abstock) user:
# ensure Quicklisp is installed:
ccl -l /usr/share/common-lisp/source/quicklisp/quicklisp.lisp -e '(quicklisp-quickstart:install)(ql:add-to-init-file)(ccl:quit)' -b
ccl --load run.lisp -e '(in-package :abstock)'
# to quit: (ccl:quit) or C-d
```

## Theme

You can theme ABStock by writing your own CSS rules into `src/static/theme.css`. This file is always loaded by the base template and is out of source control. You can create a symlink to it.

**important**: for the good of the project, think about sharing your
work! Link it [in the wiki](https://gitlab.com/vindarel/abstock/-/wikis/home), send us an email or open an issue. Many thanks in advance.


## Deployment

You can run the app as a script:

    rlwrap sbcl --load run.lisp --eval '(in-package :abstock)'

or run the binary:

    ./abstock

If you use the script, you are landed into the Lisp REPL. You can
inspect and update the application from there. See `(help)`. You can
actually do anything, including installing new Quicklisp
libraries. You can connect to the running instance from home through
SSH.

HTML changes are automatically taken up by the server. (you can switch
this off, this Djula's documentation).

### Reloading the shelves and the cards in the Lisp shell

When you are in lisp shell and Abstock is runing you can reload the shelves or the cards:

#### Reload cards
```lisp
(get-all-cards)
```
#### Reload shelves
```lisp
(get-all-shelves)
```

### Systemd

```
[Unit]
Description=Abstock

[Service]
Restart=on-failure
WorkingDirectory=/home/abstock/repo
ExecStart=/usr/bin/sbcl --load run.lisp --eval '(in-package :abstock)'
User=abstock  # or an existing user

[Install]
WantedBy=multi-user.target
```

then:

    systemctl start abstock

to see the logs:

    journalctl -u abstock.service [--since today] [--no-pager] [-o json-pretty] [-f]

use `-f` to follow the logs as they are written.


## Develop

You can contribute HTML, CSS, JavaScript, testing, documentation, and Common Lisp code. Thanks in advance!

Install the application locally.

### Static assets. Themes.

CSS and JavaScript files are served from the `src/static` directory under
the `/static` prefix (see `*default-static-directory*` in web.lisp).

You must reference them like this:

    <link rel="stylesheet" href="/static/style.css">

You can use `theme.js` and `theme.css` for your own code.

You can use Bulma's mechanism to create new themes: https://bulma.io/documentation/customize/

### Live reload

To get live-reload of static files during development, you can use [browser-sync](https://www.browsersync.io/).

```
$ browser-sync start --proxy http://localhost:8901/ --files src/static/*
[Browsersync] Proxying: http://localhost:8901
[Browsersync] Access URLs:
 -------------------------------------
       Local: http://localhost:3000
    External: http://192.168.1.11:3000
 -------------------------------------
          UI: http://localhost:3001
 UI External: http://localhost:3001
 -------------------------------------
[Browsersync] Watching files...
[Browsersync] File event [change] : src/static/style.css
[Browsersync] File event [change] : src/static/style.css
[…]
```

Now, whenever you edit some CSS or JS, you see the results instantly in the browser.

### Use the API

ABStock defines API endpoints, free for the developer to use to create new applications:

- `/api/v1/selection.json`
- `/api/v1/lastcreated.json`


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

AGPLv3.

The `src/static/img/no_cover.png` image is a modified version of [this original
one](https://commons.wikimedia.org/wiki/File:Meuble_h%C3%A9raldique_Livre_ouvert_2.svg)
that is distributed under CC-BY-SA ([Ssire](https://commons.wikimedia.org/wiki/User:Ssire)).

---

Lisp?! Oh yes, Lisp.

* https://lisp-journey.gitlab.io/pythonvslisp/
* https://lisp-lang.org/success/
* https://common-lisp.net/
* https://github.com/CodyReichert/awesome-cl
* https://lispcookbook.github.io/cl-cookbook/
