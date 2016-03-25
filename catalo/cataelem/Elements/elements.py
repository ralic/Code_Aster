# coding=utf-8

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

# person_in_charge: jacques.pellet@edf.fr

from cataelem.Tools.base_objects import AbstractEntityStore, Element
from cataelem.Tools.base_objects import LocatedComponents, ArrayOfComponents
from cataelem import __DEBUG_ELEMENTS__


class ElementStore(AbstractEntityStore):
    """Helper class to give access to all elements"""
    entityType = Element
    subTypes = (LocatedComponents, ArrayOfComponents)


EL = ElementStore("Elements", ignore_names=['ele', ],
                  only_mods=__DEBUG_ELEMENTS__)
