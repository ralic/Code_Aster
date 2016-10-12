# coding=utf-8
# person_in_charge: mathieu.courtois at edf.fr
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

"""
   Utilitaires pour tester la sd produite par une commande.
"""

from Noyau.asojb import OJB

# pour utilisation dans eficas
try:
    import aster
    from Utilitai.Utmess import UTMESS
except:
    pass


def get_list_objects():
    """Retourne la liste (set) des objets jeveux présents à un moment donné
    """
    return set(aster.jeveux_getobjects(' '))


def check(checker, co, l_before, etape):
    """Vérifie la cohérence de la SD produite :
       - type des objets / ceux déclarés dans le catalogue de la SD
       - présence d'objets imprévus dans le catalogue
    l_before : liste des objets jeveux présents avant la création de la SD.
    """

    type_concept = type(co).__name__
    sd = co.sdj
    if 0:
        print "AJACOT checksd " + type_concept + " >" + sd.nomj.nomj + '<'

    # l_new = objets créés par la commande courante
    l_after = get_list_objects()
    l_new = l_after - l_before

    # on vérifie le contenu de la SD sur la base de son catalogue
    checker = sd.check(checker)

    # Vérification des "checksum":
    if 0:
        # A modifier (if 1) si l'on souhaite vérifier que les commandes ne
        # modifient pas leurs arguments "in" :
        # On vérifie que le "checksum" des objets qui existaient déjà n'a pas changé:
        # Remarque : il faut bien le faire sur l_after car c'est checkSumOJB qui
        #             initialise la valeur pour les objets nouveaux
        for nom in l_after:
            if nom[0:1] == '&':
                # à cause des objets "&&SYS.*" et du cout des objets "&CATA.*"
                continue
            obj = OJB(nom)
            if etape.reuse:
                # les commandes réentrantes ont le droit de modifier leur
                # concept "reuse"
                if nom[0:8].strip() == sd.nomj.nomj.strip():
                    checker.checkSumOJB(obj, sd, 'maj')
                    continue
            checker.checkSumOJB(obj, sd)

    # on imprime les messages d'erreur stockés dans le checker
    lerreur = [(obj, msg) for level, obj, msg in checker.msg if level == 0]
    lerreur.sort()

    # on vérifie que la commande n'a pas créé d'objets interdits
    l_possible = set(checker.names.keys())
    l_interdit = list(l_new - l_possible)
    l_interdit.sort()

    # concept utilisateur
    if co.nom[0:8].strip() == sd.nomj.nomj.strip():
        obj = "{:24}".format("{0:<8}._TYPCO_".format(co.nom))
        if obj in l_interdit:
            l_interdit.remove(obj)
        if obj not in l_after:
            lerreur.append((obj, "type manquant"))

    if len(lerreur) > 0:
        # pour "ouvrir" le message
        nom_concept = sd.nomj.nomj.strip()
        nom_commande = etape.definition.nom.strip()
        UTMESS("E+", 'SDVERI_30', valk=(nom_concept, nom_commande))
        for obj, msg in lerreur:
            UTMESS("E+", 'SDVERI_31', valk=(obj, msg))
        UTMESS("E", 'VIDE_1')

    # on détruit les messages déjà imprimés pour ne pas les réimprimer avec la
    # SD suivante
    checker.msg = []

    if len(l_interdit) > 0:
        # pour "ouvrir" le message :
        UTMESS("E+", 'SDVERI_40', valk=type_concept)
        for x in l_interdit:
            UTMESS('E+', 'SDVERI_41', valk=x)
        UTMESS("E", 'VIDE_1')

    return checker
