# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
import string
import sys

#
#  2    utilitaires pour vérifications et émission de messages
#  3    utilitaires pour création des objets jeveux en Python
#


#
#  2    utilitaires pour vérifications et émission de messages
#

from Execution.strfunc import convert


class ERREUR:

    def __init__(self):
        self.IER = 0
        self.contxt = []

    def mess(self, code, message):
        # pour imprimer un message d'erreur :
        ucode = string.upper(code)
        if ucode == 'I':
            print "\n<" + ucode + "> INFORMATION: ", convert(message)
        elif ucode == 'A':
            print "\n<" + ucode + "> ALARME: ", convert(message)
        elif ucode == 'E':
            print "\n<" + ucode + "> ERREUR: ", convert(message)
            self.IER = self.IER + 1
        elif ucode == 'F':
            print "\n<" + ucode + "> ERREUR: ", convert(message)
            self.IER = self.IER + 1
        else:
            raise StandardError

        if ucode == 'E' or ucode == 'A' or ucode == 'F':
            if len(self.contxt) > 0:
                print "    CONTEXTE: "
                for c in self.contxt:
                    print "         " + c
        if ucode == 'F':
            raise StandardError(convert(message))

    def veri_appartient_liste(self, code, element, liste):
        try:
            x = liste.index(element)
        except ValueError:
            self.mess(
                code, str(element) + " n'appartient pas a la liste: " + str(liste))

    def veri_pas_doublon_lpara(self, code, liste):
        # verifier qu'un parametre n'apparait pas plus d'une fois dans une
        # liste de couples [(param, modloc)]
        lpara = set([val[0] for val in liste])
        if len(lpara) != len(liste):
            self.mess(code, "Le paramètre " + param +
                      " apparait plusieurs fois dans la liste: " + str(liste))

    def veri_long_chaine(self, code, chaine, n):
        if len(chaine) > n:
            self.mess(code, chaine + ' est une chaine de caractères trop longue (>' + str(n) + ').\n'
                      'Stockés en utf-8, certains caractères accentués peuvent compter double.')

    def fini(self):
        # pour demander l'arret du code si necessaire :
        if self.IER > 0:
            print "<F> ERREUR FATALE declenchee suite aux erreurs <E> precedentes"
            raise StandardError

    def contexte(self, texte, statut="RAZ"):
        """pour definir le contexte d'un message d'erreur"""
        self.fini()
        if statut == 'RAZ':
            self.contxt = []
            if len(texte) > 0:
                self.contxt.append(texte)
        elif statut == 'AJOUT':
            self.contxt.append(texte)
        else:
            self.mess('F', 'erreur pgmeur')

# ERR: objet ERREUR partage par toute l'application :
# ----------------------------------------------------
ERR = ERREUR()


#
#  3    utilitaires pour creation des objets jeveux en Python
#


def cree_os(dicobj, nom, tsca, long):
    if dicobj.get(nom) is not None:
        ERR.mess('F', "Erreur objet deja declare:" + nom)
    o1 = JV_SIMPLE(nom, tsca, long)
    dicobj[nom] = o1
    return o1


def cree_pn(dicobj, nom, tsca):
    if dicobj.get(nom) is not None:
        ERR.mess('F', "Erreur objet deja declare:" + nom)
    o1 = JV_PNOM(nom, tsca)
    dicobj[nom] = o1
    return o1


def cree_co(dicobj, nom, tsca, tsca_pn, contig, acces, longv):
    if dicobj.get(nom) is not None:
        ERR.mess('F', "Erreur objet deja declare:" + nom)
    o1 = JV_COLLEC(nom, tsca, tsca_pn, contig, acces, longv)
    dicobj[nom] = o1
    return o1


class JV_COLLEC:

    def __init__(self, nom, tsca, tsca_pn, contig, acces, longv):
        # ----------------------------------------------------------------------------------------
        # pour creer une collection jeveux
        # tsca = /'I' /'R' /'C' /'K8' /'K16' ...
        # tsca_pn = /'K8' /'K16' /'K24' ... : longueur des chaines permettant l'acces aux OC
        # contig = /'CONTIG' /'DISPER'
        # acces = /'NU' /'NO'
        # longv : /0 (si longueur variable) /n (si longueur constante)
        self.nom = nom
        self.typojb = 'collec'
        self.tsca = tsca
        self.tsca_pn = tsca_pn
        self.acces = acces
        self.contig = contig
        self.longv = longv
        if longv < 0:
            ERR.mess('F', "Erreur")
        if acces != 'NO' and acces != 'NU':
            ERR.mess('F', "Erreur")
        if contig != 'CONTIG' and contig != 'DISPER':
            ERR.mess('F', "Erreur")
        self.pn = JV_PNOM(nom=nom, tsca=self.tsca_pn)
        self.objs = []

    def cree_oc(self, nom, long):
        oc1 = JV_SIMPLE(nom, self.tsca, long)
        num = self.pn.jenonu(nom, 'COOL')
        if num > 0:
            ERR.mess(
                'F', "Erreur : nom existant deja: " + nom + " dans: " + self.nom)
        if self.longv > 0 and long != self.longv:
            ERR.mess('F', "Erreur : longueur incorrecte: " +
                     str(long) + " pour: " + self.nom)
        self.objs.append(oc1)

    def ecri_co(self, nom, indice, valeur):
        num = self.pn.jenonu(nom)
        if num < 0:
            ERR.mess(
                'F', "Erreur : nom inexistant: " + nom + " dans: " + self.nom)
        oc1 = self.objs[num - 1]
        oc1.ecri_os(indice, valeur)

    def lit_co(self, nom, indice):
        num = self.pn.jenonu(nom)
        if num < 0:
            ERR.mess(
                'F', "Erreur : nom inexistant: " + nom + " dans: " + self.nom)
        oc1 = self.objs[num - 1]
        return oc1.lit_os(indice)

    def impr(self, file):
        nmaxoc = len(self.objs)
        lont = 0
        if self.contig == 'CONTIG':
            for oc1 in self.objs:
                lont = lont + len(oc1.valeurs)
        if self.longv == 0:
            modlon = 'VARIABLE'
        else:
            modlon = 'CONSTANT'

        lines = []
        lines.append("|TYPE_JEVEUX=COLLEC")
        # TODO nutioc == nmaxoc ?
        lines.append("|NOM={self.nom:24}|TYPE={self.tsca:3}|NMAXOC={nmaxoc:>12}"
                     "|NUTIOC={nmaxoc:>12}|ACCES={self.acces:2}|STOCKAGE={self.contig:8}"
                     "|MODELONG={modlon:8}|LONMAX={self.longv:>12}|LONT={lont:>12}"
                     .format(self=self, nmaxoc=nmaxoc, modlon=modlon, lont=lont))

        for oc1 in self.objs:
            if self.acces == "NO":
                # les collections ayant leur pointeur de nom en interne ont un
                # acces K8
                lines.append("|NOM={nom:8}|LONMAX={len:>12}"
                             .format(nom=oc1.nom, len=len(oc1.valeurs)))
            else:
                lines.append("|LONMAX={len:>12}".format(len=len(oc1.valeurs)))

            if self.tsca[0] == "K":
                lines.extend(oc1.valeurs)
            elif self.tsca[0] == "I":
                lines.extend(["{:>12}".format(val) for val in oc1.valeurs])
            else:
                ERR.mess('F', "Erreur : programmation a ajouter ...")
        lines.append('')
        file.write("\n".join(lines))


class JV_SIMPLE:

    def __init__(self, nom, tsca, long):
        # ----------------------------------------------------------------------------------------
        # pour creer un vecteur jeveux
        # tsca = /'I' /'R' /'C' /'K8' /'K16' ...
        # long : longueur du vecteur
        self.nom = nom
        self.typojb = 'vecteur'
        self.tsca = tsca
        self.long = long
        if long < 0:
            ERR.mess('F', "Erreur")
        if tsca[0] == "I":
            self.valeurs = [0] * long
        elif tsca[0] == "K":
            self.valeurs = [""] * long
        else:
            ERR.mess('F', "Erreur")

    def ecri_os(self, indice, valeur):
        if indice < 1 or indice > len(self.valeurs):
            ERR.mess('F', "Erreur")
        if self.tsca[0] == "K" and not isinstance(valeur, str):
            ERR.mess('F', "Erreur : on attend une chaine: " + str(valeur))
        if self.tsca[0] == "I" and not isinstance(valeur, int):
            ERR.mess('F', "Erreur : on attend un entier: " + str(valeur))
        if self.tsca[0] == "R" and not isinstance(valeur, float):
            ERR.mess('F', "Erreur : on attend un reel: " + str(valeur))

        if self.tsca[0] == "K":
            # on tronque eventuellement la chaine :
            ERR.veri_long_chaine('E', valeur, int(self.tsca[1:3]))
            nom2 = valeur[0:int(self.tsca[1:3])]
            self.valeurs[indice - 1] = nom2
        else:
            self.valeurs[indice - 1] = valeur

    def lit_os(self, indice):
        if indice < 1 or indice > len(self.valeurs):
            ERR.mess('F', "Erreur")
        return self.valeurs[indice - 1]

    def impr(self, file):
        lines = []
        lines.append("|TYPE_JEVEUX=SIMPLE")
        lines.append("|NOM={self.nom:24}|TYPE={self.tsca:3}|LONMAX={self.long:>12}"
                     .format(self=self))
        if self.tsca[0] == "K":
            lines.extend(self.valeurs)
        elif self.tsca[0] == "I":
            lines.extend(["{:>12}".format(val) for val in self.valeurs])
        else:
            ERR.mess('F', "Erreur : programmation a ajouter ...")
        lines.append('')
        file.write('\n'.join(lines))


class JV_PNOM:

    def __init__(self, nom, tsca):
        # ----------------------------------------------------------------------------------------
        # pour creer un pointeur de noms jeveux
        # tsca = /'I' /'R' /'C' /'K8' /'K16' ...
        self.nom = nom
        self.typojb = 'pteur_nom'
        if tsca[0] != "K":
            ERR.mess('F', "Erreur : tsca = K* obligatoire.")
        self.tsca = tsca
        self.valeurs = []
        self.dico = {}
        self.nomuti = 0

    def jenonu(self, nom, stop='PAS_COOL'):
        # rend le numero (num de 1 a n) d'un nom dans un pointeur de noms.
        # num est < 0 si le nom a ete ajoute au pointeur.
        if not isinstance(nom, str):
            ERR.mess('F', "Erreur : on attend nom=chaine.")
        ERR.veri_long_chaine('E', nom, int(self.tsca[1:3]))
        nom2 = nom[0:int(self.tsca[1:3])]
        num = self.dico.get(nom2)
        if num is not None:
            return num
        else:
            if stop != "COOL":
                ERR.mess(
                    'F', "Erreur: nom <" + nom + "> inexistant dans: " + self.nom)
            indice = self.nomuti + 1
            self.nomuti = indice
            self.dico[nom2] = indice
            self.valeurs.append(nom2)
            return -indice

    def ajout_nom(self, nom):
        # ajoute un nom dans un pointeur de noms.
        # s'arrete en erreur fatale si le nom existe deja
        if self.dico.get(nom) is not None:
            ERR.mess(
                'F', "Erreur: le nom: " + nom + " existe deja dans: " + self.nom)
        else:
            indice = self.nomuti + 1
            self.nomuti = indice
            ERR.veri_long_chaine('E', nom, int(self.tsca[1:3]))
            nom2 = nom[0:int(self.tsca[1:3])]
            self.dico[nom2] = indice
            self.valeurs.append(nom2)

    def impr(self, file):
        lines = []
        lines.append("|TYPE_JEVEUX=PT_NOM")
        lines.append("|NOM={self.nom:24}|TYPE={self.tsca:3}|NOMMAX={self.nomuti:>12}"
                     .format(self=self))
        nchar = int(self.tsca[1:4])
        lines.extend(["{:{size}}".format(val, size=nchar) for val in self.valeurs])
        lines.append('')
        file.write('\n'.join(lines))
