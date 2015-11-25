# coding=utf-8
#
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
# person_in_charge: mathieu.courtois@edf.fr

"""
This module provides helper objects to easily apply changes between elements
"""

class CataElemVisitor(object):

    """This class walks the tree of CataElem object.
    """

    def __init__(self):
        """Initialization."""

    def visitCataElem(self, cataelem):
        """Visit a CataElem object"""

    def visitPhenomenon(self, phenomenon):
        """Visit a Phenomenon object"""

    def visitModelisation(self, modelisation):
        """Visit a Modelisation object"""

    def visitElement(self, element):
        """Visit a Element object"""
        for _dummy, calc in element.getCalculs():
            calc.accept(self)

    def visitCalcul(self, calcul):
        """Visit a Calcul object"""
        calcul.option.accept(self)
        for param, locCmp in calcul.para_in + calcul.para_out:
            param.accept(self)
            locCmp.accept(self)

    def visitOption(self, option):
        """Visit a Option object"""
        for para in option.para_in + option.para_out:
            para.accept(self)

    def visitArrayOfComponents(self, array):
        """Visit a ArrayOfComponents object"""
        for locCmp in array.locatedComponents:
            locCmp.accept(self)

    def visitLocatedComponents(self, locCmp):
        """Visit a LocatedComponents object"""
        locCmp.physicalQuantity.accept(self)

    def visitInputParameter(self, param):
        """Visit a InputParameter object"""
        param.physicalQuantity.accept(self)

    def visitOutputParameter(self, param):
        """Visit a OutputParameter object"""
        param.physicalQuantity.accept(self)

    def visitArrayOfQuantities(self, array):
        """Visit a ArrayOfQuantities object"""
        array.physicalQuantity.accept(self)

    def visitPhysicalQuantity(self, phys):
        """Visit a PhysicalQuantity object"""
