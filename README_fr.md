
<p>
  <h2 align="center"> ABStock </h2>
  <h2 align="center"> Consultation de votre stock et commandes en ligne </h2>
</p>

<p align="center">
  <a href="https://gitlab.com/vindarel/abstock"><b>Accueil</b></a> |
  <a href="https://gitlab.com/vindarel/abstock#install"><b>Installation</b></a> |
  <a href="https://framasphere.org/people/4ac5fae0bed90133a3ed2a0000053625"><b>Blog</b></a> |
  <a href="https://framavox.org/g/V6oiDr8Y/abelujo"><b>Forum</b></a> |
  <a href="https://liberapay.com/vindarel/donate"><b>Nous soutenir</b></a> |
  <a href="/README.md">English</a>

</p>


ABStock se connecte à votre base de données et affiche votre stock en ligne. Les clients ajoutent des livres à leur panier et passent commande.

La page de résultats de recherche ressemble à ceci:

![welcome screen](search.png "welcome screen")


Le site se compose de plusieurs pages, toutes configurables:

- la **page d'accueil**, avec:
  - les informations importantes de la librairie,
  - un formulaire de recherche,
  - et un échantillon de livres mis en avant par le libraire.
- le formulaire de recherche permet de **chercher par titre, auteur, éditeur, rayon et ISBN(s)**.
- en voyant les résultats, l'internaute peut cliquer sur une icône "+" pour **ajouter des livres à son panier**.
- la page panier montre la sélection du client, et propose de remplir un formulaire pour valider la commande.
- le formulaire envoie la commande par **courriel** au libraire.
- une page spéciale qui montre **la sélection du libraire**, livres triés par rayon.


<p style="text-align: center">
    <strong>Testez par vous-même:</strong>
</p>

<p style="text-align: center; background-color: gainsboro">
    <a href="http://5.196.70.6:8902/">démo en ligne</a>
</p>

Le site fonctionne par défaut avec le logiciel libre de gestion de
librairies [Abelujo](http://abelujo.cc/), et se synchronise avec sa
base de données toutes les nuits. Abelujo est l'outil pour le
libraire: Abelujo gère les inventaires, la vente, etc, et sait se
connecter au FEL à la demande de Dilicom.

**Vous pouvez avoir un site sur le même modèle**. N'hésitez pas à nous contacter: `contact @ abelujo . cc`.

Pour la petite histoire, nous avons développé ce site au milieu du
confinement pour aider la (très chouette) librairie [La
Palpitante](http://www.lapalpitante.fr/) à Mens (38) à garder une
activité et un lien avec ses client·es, alors qu'Amazon tourne à plein
régime et que les sites de regroupement de libraires du type
lalibrairie.com sont à l'arrêt (et ne permettent au demeurant pas de
savoir ce qu'il y a en rayon). Les librairies sont très fragiles. On
ne veut pas en voir disparaître, surtout lorsqu'il ne s'en trouve
qu'une sur le territoire. ABStock est notre proposition !

## Installation

ABStock est un logiciel libre, il est gratuit à télécharger,
installer, modifier et redistribuer. Vous pouvez aussi passer par nos
services pour qu'on prenne en charge l'installation, l'hébergement et
les mises à jour sur nos serveurs (cf plus bas).

Voici les étapes pour l'installer vous-même.

Installer SBCL:

    apt install sbcl rlwrap

Installer Quicklisp: https://lispcookbook.github.io/cl-cookbook/getting-started.html#install-quicklisp

Il y a une dépendance à cloner dans le répertoire ~/quicklisp/local-projects:

- https://github.com/mmontone/cl-sentry-client

Cloner le projet:

    git clone https://gitlab.com/vindarel/abstock

## Usage

Pour lancer l'application il y a deux possibilités.

**La première**

    make run
    # aka:
    # rlwrap sbcl --load run.lisp --eval '(in-package :abstock)'

Au premier lancement, ça va installer les dépendances logicielles. Le second démarrage est plus rapide.

Les étapes au lancement sont:

- lecture des variables d'environnement,
- lecture du fichier de config,
- lecture des livres à mettre en avant,
- chargement des données précédemment sauvegardées sur disque, s'il y en a,
- démarrage du server web. À ce stade le site est visible en ligne.
- Enfin, synchronisation avec la base de données (peut prendre 1 à 2 minutes).

Quand l'application est lancée, on obtient un prompt interactif avec lequel on peut interagir avec le site, en live. C'est particulièrement pratique pour recharger la configuration utilisateur. Pour ce faire taper ceci, parenthèses incluses:

    (load-init)

**La seconde** méthode est d'utiliser un exécutable.

Il faut le télécharger (mais il n'est pas encore dispo) ou le construire (voir ci-dessous). Pour le lancer:

    ./abstock

L'avantage est que le démarrage est immédiat.


Des paramètres sont modifiables via des variables d'environnement:

* le port:

    AB_PORT=9999 sbcl --load run.lisp

* la location du fichier de configuration

    CONFIG=../path/to/config.lisp make run

* ne pas charger la BD (mais lire les données sauvées en fichiers):

    LOAD_DB=nil …


## Configurer

Les textes et autres informations personnelles à afficher sont à configurer dans le
fichier `config.lisp`.

Cf tous les paramètres dans `config-example.lisp`.


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

### Envoi de mails

Les mails de commande sont envoyés avec SendGrid. Il faut créer un compte et renseigner sa clef (api key).

## Développer

Pour construire l'exécutable

    make build

Pour lancer l'application dans son éditeur Lisp, il faut "`load`er" le
fichier .asd (C-c C-k avec Slime) et charger l'app avec:

    (ql:quickload :abstock)

Puis la démarrer:

    (in-package :abstock)
    (start)  ; optional :port 9999 argument.

Une référence au language de programmation utilisé à garder sous la main: [https://lispcookbook.github.io/cl-cookbook/](https://lispcookbook.github.io/cl-cookbook/)

On utilise [Bulma](https://bulma.io) CSS.


## Déployer

L'application lancée comme expliqué ci-dessus est directement visible depuis l'internet.

Vous pouvez y ajouter un proxy Apache ou Nginx.

Pour utiliser un fichier de config qui se trouve ailleurs:

    ln -s ../path/to/user/config-user.lisp config.lisp

Il vous faut sûrement un cron job pour copier la BD d'Abelujo. Par exemple, toutes les nuits à 4h:

    crontab -e
    00 4 * * * cp /home/path/to/abelujo/db.db /home/path/to/abstock/ && echo $(date) "Abelujo DB copied to ABStock" >> log.txt || echo "oops could not copy Abelujo DB" $(date) >> log.txt


Pour utiliser **Sentry**: mettre son DSN dans `~/.config/abstock/sentry-dsn.txt`. (pour le moment, utiliser de DSN "deprecated").

## Hébergement

Nous pouvons héberger ces solutions sur nos serveurs. Demandez-nous un devis.

## Tickets

Pour rapports de bugs et demandes de fonctionnalités:

https://gitlab.com/vindarel/abstock/-/issues


## Contact et soutien

Écrivez-nous à `contact @ abelujo . cc` (sans espaces).

Vous pouvez nous soutenir:

- parlez de cette application
- donnez-nous un coup de pouce: https://liberapay.com/vindarel/ Merci!

## Licence

Parce que nous souhaitons faciliter la circulation des livres et de leurs idées, nous développons également cette application web sous licence libre.

GPLv3
