
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
base de données à intervalles réguliers. Abelujo est l'outil pour le
libraire: Abelujo gère les inventaires, la vente, etc, et sait se
connecter au FEL à la demande de Dilicom. Vous pouvez créer et
afficher des produits autre que des livres.

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
