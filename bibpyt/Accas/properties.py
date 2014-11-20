# coding=utf-8
# person_in_charge: mathieu.courtois@edf.fr
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#     IDENTIFICATION DE LA VERSION
#----------------------------------------------------------------------
version = "12.2.18"
date = "20/11/2014"
exploit = False

parentid, branch, from_branch = ['?'] * 3
changes, uncommitted = 0, None

# read values filled by the build script:
#  version_as_tuple : last tag found in parents,
#  revision_id      : hex identifier of the parent (join by '+' if
#                     more than one),
#  branch           : branch name,
#  date             : date of the revision,
#  from_branch      : 'official' parent branch,
#  changes          : number of changes after the tagged revision (0 means
#                     no change since the tag was set)
try:
    from pkginfo import pkginfo
    version = '.'.join([str(i) for i in pkginfo.pop(0)])
    parentid = pkginfo.pop(0)
    branch = pkginfo.pop(0)
    exploit = branch.startswith('v')
    date = pkginfo.pop(0)
    from_branch = pkginfo.pop(0)
    changes = pkginfo.pop(0)
    uncommitted = pkginfo.pop(0)
except (ImportError, IndexError, TypeError):
    pass
