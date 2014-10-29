# coding=utf-8
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
# person_in_charge: mathieu.courtois at edf.fr


class MissDomains(object):

    """A MissDomain instance assigns the identifiers of the domains and
    the different groups they contains"""

    def __init__(self, use_pc, use_issf):
        """Initialization"""
        self.use_pc = use_pc
        self.use_issf = use_issf
        # XXX by defining all the domains, even for issf, fdlv112b fails
        self.def_all_domains = (not use_issf) or use_pc
        self.domain = {}
        self.group = {}
        self._define_domain()

    def __getitem__(self, key):
        """Return a domain definition"""
        return self.domain[key]

    def get(self, key, default=None):
        """Return a domain definition"""
        return self.domain.get(key, default)

    def _define_domain(self):
        """Define the group and domain numbers.
        'group' and 'domain' are dictionnaries.
        - Keys of 'group' are strings: 'sol-struct', 'fluide-struct',
          'sol-fluide', 'sol libre', 'pc', 'struct'
        - Keys of 'domain' are strings: 'struct', 'sol', 'fluide'.
          A domain is defined by its number and the groups that belong to it.

             groupes                   ISS    ISS+PC  ISFS    ISFS+PC
        . interface sol-structure       1       1       1       1
        . interface fluide-structure                    2       2
        . interface sol-fluide                          3       3
        . sol libre                                     4       4
        . points de contrôle                    2               5
        . volume de la structure        2       3       5       6
        """
        self.domain['struct'] = (1, [1])
        self.domain['sol'] = (2, [-1])
        self.group['sol-struct'] = i = 1
        if self.use_issf:
            if self.def_all_domains:
                self.domain['fluide'] = (3, [-2, -3])
            else:
                self.domain['sol'] = (1, [-1])
                self.domain['fluide'] = (2, [-2, -3])
            self.group['fluide-struct'] = i = i + 1
            self.group['sol-fluide'] = i = i + 1
            self.group['sol libre'] = i = i + 1
            self.domain['struct'][1].append(self.group['fluide-struct'])
            self.domain['sol'][1].extend([self.group['sol-fluide'],
                                          self.group['sol libre']])
        if self.use_pc:
            self.group['pc'] = i = i + 1
            self.domain['sol'][1].append(self.group['pc'])
        self.group['struct'] = i = i + 1
        self.domain['struct'][1].append(self.group['struct'])
        # checkings
        if not self.use_pc and not self.use_issf:
            assert self.group['struct'] == 2
        elif self.use_pc and not self.use_issf:
            assert self.group['struct'] == 3
        elif not self.use_pc and self.use_issf:
            assert self.group['struct'] == 5
        elif self.use_pc and self.use_issf:
            assert self.group['struct'] == 6
