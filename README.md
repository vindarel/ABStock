## ABStock: view your books online

Clients can see your books online, search your stock, and select them
in a basket.

It uses the [Abelujo](http://abelujo.cc/) database (or a copy of
it). You must have done an inventory with Abelujo before (or, we can
override a couple functions that are supposed to query the DB).

The database is loaded in memory at startup and doesn't access it
afterwards.

**disclaimer**: this is an alpha project done in haste to help an
actual bookshop and many things are missing.

The welcome screen:

![](welcome.png)

Searching the stock:

![welcome screen](search.png "welcome screen")

Seeing one's shopping basket:

![](basket.png)

Current features:

- show the catalogue
- search it (by title, authors, publisher, shelf, ISBN…)
  - search many ISBNs at once in the stock (separated by a space or a comma). Display the ones not found.
- the user adds books in his or her basket
- (s)he fills a confirmation form which sends an **email** to the shop owner to pass command
- the DB is synced every night.

Interested? Please get in touch.


## Config

Write your config either
* into `config.lisp` at the project root
* into `~/.abstock.lisp`.

You can overwrite:

- the contact information (two phone numbers, email…)
- your SendGrid API key
- the simple anti-spam validation form question (and answer)
- the welcome texts
- the shopping basket top text
- etc

See all available settings into `config-example.lisp`.


~~~lisp
(setf *port* 9889)

(setf *contact-infos*
  '(:|phone| "06 09 09 09 09"
    :|email| "me@test.fr"))

;; SendGrid config:
(setf *email-config*
   '(:|sender-api-key| "api-key"
     :|from| "your@mail.com"))

;; Simple anti-script-kiddy question for the validation form:
(setf *secret-question* "stupid question")
(setf *secret-answer* "answer")
~~~

## Run

Two possibilities:

    sbcl --load run.lisp

note that in that case, we are dropped into the Lisp REPL, so we can
interact with the running application (useful to reload settings and
such). You can reload all settings with `(load-init)`.

or download/build the binary and run it:

    ./abstock


Environment variables:

* the application port:

    AB_PORT=9999 sbcl --load run.lisp

## Develop

Install SBCL:

    apt install sbcl

install Quicklisp (see [Cookbook: getting started](https://lispcookbook.github.io/cl-cookbook/getting-started.html))

build the binary:

    make build

then simply run it:

```
./abstock

Reading the DB...
Done. 3517 cards found.
Starting the web server on port 9889
Ready. You can access the application!

86.210.212.58 - [2020-04-07 19:56:28] "GET / HTTP/1.1" 200 3903 "-" "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:69.0) Gecko/20100101 Firefox/69.0"
```

The UI uses the [Bulma](https://bulma.io) CSS framework.

Issue tracker: https://gitlab.com/vindarel/abstock/-/issues

## Deploy

Use a user's configuration file:

    ln -s ../path/to/user/config-user.lisp config.lisp

## Licence

GPLv3
