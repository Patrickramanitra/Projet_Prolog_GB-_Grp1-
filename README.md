# Projet IA – Enquête Policière en Prolog

📌 Description
Ce projet est une modélisation d’une **enquête policière** en **Prolog**, intégrant à la fois :
- Une base de faits (suspects, crimes, preuves, alibis, témoins…).
- Des règles logiques permettant de déduire si un suspect est **coupable** ou **innocent**.
- Une interface **Web interactive** construite avec la bibliothèque HTTP de **SWI-Prolog**, permettant de tester les suspects directement dans un navigateur.

---
⚙️ Fonctionnalités principales
- Vérification si un **suspect** est coupable d’un type de crime.
- Détection des faux positifs (empreintes posées avant le crime).
- Gestion des témoins **fiables** ou **non fiables**.
- Prise en compte des **alibis** (heure et lieu).
- Justifications détaillées avec la liste des **preuves retenues**.
- Interface Web simple avec navigation :
  - **Accueil**
  - **Liste des suspects**
  - **Coupables par type de crime**
  - **Dossier individuel par suspect**

---
🌐 Interface Web
Page d’accueil : formulaire pour tester un suspect.
Navigation : menu pour accéder aux différentes sections.
Dossiers : chaque suspect dispose d’une fiche listant ses verdicts, éléments à charge, et un bouton pour tester individuellement.
Verdicts visuels :
🟢 Coupable
🔴 Non coupable

---
📂 Organisation
Le projet est contenu dans un seul fichier enquete_policiere_prolog.pl qui regroupe :
Faits et règles logiques.
Détection des coupables.
Serveur HTTP + génération HTML.

---
On consulte  au  dossier  qui  se  trouve  le  projet dans prolog : File ---> consult ---> puis  selectionner le  projet enquete_policiere_prolog.pl 
Et Pour  lancer  le  serveur Prolog on a  comme  port 8080 puis  l'executer avec  le  commande : ?- server(8080). 

---
👥 Auteurs 
RAMANITRARIVO Herve Patrick : 2498 ,
RANDRIANANDRASANA Njoro Alexandre : 2507 ,
RAKOTOZANDRY Mamitiana Christiano : 2554 ,
RAKOTOMALALA Andoniaina Landry : 2386 ,
RAKOTONDRANAIVO Mahefasoa Fenotoky : 2565 ,
RALAINANDRASANA Mijoro Marson : 2408 
