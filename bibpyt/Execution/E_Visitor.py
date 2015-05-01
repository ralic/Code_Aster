# coding=utf-8
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
# ======================================================================
# person_in_charge: mathieu.courtois at edf.fr

"""
This module defines a generic Visitor to walk JDC & its composite objects.
"""


class JDCVisitor(object):

    """This class walks the tree of JDC object and defines some "generic" methods.
    """

    def __init__(self, with_default=True):
        """Initialization."""
        self.with_default = with_default

    def visitJDC(self, jdc):
        """Visit the JDC object"""
        for etape in jdc.etapes:
            etape.accept(self)

    def visitPROC_ETAPE(self, step):
        """Visit the PROC_ETAPE object."""
        self._visit_etape(step, reuse=None)

    def visitMACRO_ETAPE(self, step):
        """Visit the MACRO_ETAPE object."""
        self._visit_etape(step, reuse=step.reuse)

    def visitETAPE(self, step):
        """Visit the ETAPE object."""
        self._visit_etape(step, reuse=step.reuse)

    def _visit_etape(self, step, reuse):
        """Visit generic ETAPE objects."""
        if reuse is not None:
            reuse.accept(self)
        self._visitMCCOMPO(step)

    def visitMCBLOC(self, bloc):
        """Visit the MCBLOC object."""
        # print "visit BLOC", bloc.nom
        self._visitMCCOMPO(bloc)

    def visitMCFACT(self, fact):
        """Visit the MCFACT object."""
        # print "visit MCFACT", fact.nom
        self._visitMCCOMPO(fact)

    def visitMCList(self, mclist):
        """Visit the MCList object."""
        # print 'visit MCList', mclist
        for data in mclist.data:
            data.accept(self)

    def _visit_default_keywords(self, node, seen):
        """Visit the default values of 'node' not already seen."""
        if not self.with_default:
            return
        for key, obj in node.definition.entites.items():
            has_default = getattr(obj, 'defaut', None) is not None \
                or getattr(obj, 'statut', None) == 'd'
            if not key in seen and has_default:
                mc = obj(obj.defaut, key, parent=node)
                mc.accept(self)

    def _visitMCCOMPO(self, compo):
        """Visit generic MCCOMPO objects (ETAPE, MCFACT, MCBLOC)
        (*private*, no 'accept' method in MCCOMPO class)."""
        # print "visit MCCOMPO", compo.nom
        seen = set()
        for obj in compo.mc_liste:
            obj.accept(self)
            seen.add(obj.nom)
        self._visit_default_keywords(compo, seen)

    def visitMCSIMP(self, mcsimp):
        """Visit the MCSIMP object."""
        raise NotImplementedError('must be overridden in a derivated class')
        # for value in force_list(mcsimp.valeur):
            # if isinstance(value, ASSD):
                # value.accept(self)
            #...

    def visitASSD(self, sd):
        """Visit the ASSD object."""
        raise NotImplementedError('must be overridden in a derivated class')
