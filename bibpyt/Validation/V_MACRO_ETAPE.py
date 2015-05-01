# coding=utf-8
# person_in_charge: mathieu.courtois at edf.fr
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#
#
# ======================================================================


"""
   Ce module contient la classe mixin MACRO_ETAPE qui porte les méthodes
   nécessaires pour réaliser la validation d'un objet de type MACRO_ETAPE
   dérivé de OBJECT.

   Une classe mixin porte principalement des traitements et est
   utilisée par héritage multiple pour composer les traitements.
"""
# Modules Python
import types
import sys
import traceback

# Modules EFICAS
import V_MCCOMPO
import V_ETAPE
from Noyau.N_Exception import AsException
from Noyau.N_utils import AsType
from Noyau.strfunc import ufmt


class MACRO_ETAPE(V_ETAPE.ETAPE):

    """
    """

    def isvalid(self, sd='oui', cr='non'):
        """
           Methode pour verifier la validité de l'objet ETAPE. Cette méthode
           peut etre appelée selon plusieurs modes en fonction de la valeur
           de sd et de cr.

           Si cr vaut oui elle crée en plus un compte-rendu.

           Cette méthode a plusieurs fonctions :

            - mettre à jour l'état de self (update)

            - retourner un indicateur de validité 0=non, 1=oui

            - produire un compte-rendu : self.cr

        """
        if CONTEXT.debug:
            print "ETAPE.isvalid ", self.nom
        if self.state == 'unchanged':
            return self.valid
        else:
            valid = 1
            # On marque les concepts CO pour verification ulterieure de leur
            # bonne utilisation
            l = self.get_all_co()
            # On verifie que les concepts CO sont bien passes par type_sdprod
            for c in l:
                # if c.etape is self.parent:
                if c.is_typco() != 2:
                    # le concept est propriete de l'etape parent
                    # Il n'a pas ete transforme par type_sdprod
                    # Cette situation est interdite
                    # Pb: La macro-commande a passe le concept a une commande
                    # (macro ?) mal definie
                    if cr == 'oui':
                        self.cr.fatal(_(u"Macro-commande mal définie : le concept n'a pas été typé par "
                                        u"un appel à type_sdprod pour %s"), c.nom)
                    valid = 0

            valid = valid * self.valid_child()
            valid = valid * self.valid_regles(cr)

            if self.reste_val != {}:
                if cr == 'oui':
                    self.cr.fatal(
                        _(u"Mots clés inconnus : %s"), ','.join(self.reste_val.keys()))
                valid = 0

            if sd == "non":
                # Dans ce cas, on ne calcule qu'une validite partielle, on ne modifie pas l'état de self
                # on retourne simplement l'indicateur valid
                return valid

            if self.sd != None:
                valid = valid * self.valid_sdnom(cr)

            if self.definition.reentrant == 'n' and self.reuse:
                # Il ne peut y avoir de concept reutilise avec une MACRO  non
                # reentrante
                if cr == 'oui':
                    self.cr.fatal(
                        _(u'Macro-commande non réentrante : ne pas utiliser reuse'))
                valid = 0

            if valid:
                valid = self.update_sdprod(cr)

            # Si la macro comprend des etapes internes, on teste leur validite
            for e in self.etapes:
                if not e.isvalid():
                    valid = 0
                    break

            self.set_valid(valid)

            return self.valid

    def update_sdprod(self, cr='non'):
        """
             Cette méthode met à jour le concept produit en fonction des conditions initiales :

              1. Il n'y a pas de concept retourné (self.definition.sd_prod == None)

              2. Le concept retourné n existait pas (self.sd == None)

              3. Le concept retourné existait. On change alors son type ou on le supprime

             En cas d'erreur (exception) on retourne un indicateur de validité de 0 sinon de 1
        """
        sd_prod = self.definition.sd_prod
        # On memorise le type retourné dans l attribut typret
        self.typret = None
        if type(sd_prod) == types.FunctionType:
            # Type de concept retourné calculé
            d = self.cree_dict_valeurs(self.mc_liste)
            try:
                # la sd_prod d'une macro a l'objet lui meme en premier argument
                # contrairement à une ETAPE ou PROC_ETAPE
                # Comme sd_prod peut invoquer la méthode type_sdprod qui ajoute
                # les concepts produits dans self.sdprods, il faut le mettre à
                # zéro
                self.sdprods = []
                sd_prod = apply(sd_prod, (self,), d)
            except:
                # Erreur pendant le calcul du type retourné
                if CONTEXT.debug:
                    traceback.print_exc()
                self.sd = None
                if cr == 'oui':
                    l = traceback.format_exception(sys.exc_info()[0],
                                                   sys.exc_info()[1],
                                                   sys.exc_info()[2])
                    self.cr.fatal(
                        _(u'Impossible d affecter un type au résultat\n%s'), ' '.join(l[2:]))
                return 0
        # on teste maintenant si la SD est r\351utilis\351e ou s'il faut la
        # cr\351er
        valid = 1
        if self.reuse:
            # Un concept reutilise a ete specifie
            if AsType(self.reuse) != sd_prod:
                if cr == 'oui':
                    self.cr.fatal(
                        _(u'Type de concept réutilisé incompatible avec type produit'))
                valid = 0
            if self.sdnom != '':
                if self.sdnom[0] != '_' and self.reuse.nom != self.sdnom:
                    # Le nom de la variable de retour (self.sdnom) doit etre le
                    # meme que celui du concept reutilise (self.reuse.nom)
                    if cr == 'oui':
                        self.cr.fatal(_(u'Concept réutilisé : le nom de la variable de '
                                        u'retour devrait être %s et non %s'),
                                      self.reuse.nom, self.sdnom)
                    valid = 0
            if valid:
                self.sd = self.reuse
        else:
            # Cas d'un concept non reutilise
            if sd_prod == None:  # Pas de concept retourné
                # Que faut il faire de l eventuel ancien sd ?
                self.sd = None
            else:
                if self.sd:
                    # Un sd existe deja, on change son type
                    if CONTEXT.debug:
                        print "changement de type:", self.sd, sd_prod
                    if self.sd.__class__ != sd_prod:
                        self.sd.change_type(sd_prod)
                    self.typret = sd_prod
                else:
                    # Le sd n existait pas , on ne le crée pas
                    self.typret = sd_prod
                    if cr == 'oui':
                        self.cr.fatal(_(u"Concept retourné non défini"))
                    valid = 0
            if self.definition.reentrant == 'o':
                if cr == 'oui':
                    self.cr.fatal(
                        _(u'Commande obligatoirement réentrante : spécifier reuse=concept'))
                valid = 0
        return valid

    def report(self):
        """
            Methode pour la generation d un rapport de validation
        """
        V_ETAPE.ETAPE.report(self)
        for e in self.etapes:
            self.cr.add(e.report())
        return self.cr
