#@ MODIF calc_table_ops Macro  DATE 23/05/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

# RESPONSABLE COURTOIS M.COURTOIS
import os

def calc_table_ops(self, TABLE, ACTION, INFO, **args):
    """
    Macro CALC_TABLE permettant de faire des opérations sur une table
    """
    import aster

    from Accas                 import _F
    from Cata.cata             import table_fonction, table_jeveux
    from Utilitai.Utmess       import UTMESS
    from Utilitai.Table        import merge
    from Utilitai.utils        import get_titre_concept

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Le concept sortant (de type table_sdaster ou dérivé) est tabout
    self.DeclareOut('tabout', self.sd)
    if self.sd.__class__ == table_fonction:
        typ_tabout = 'TABLE_FONCTION'
    else:
        typ_tabout = 'TABLE'

    # On importe les definitions des commandes a utiliser dans la macro
    # Le nom de la variable doit etre obligatoirement le nom de la commande
    CREA_TABLE    = self.get_cmd('CREA_TABLE')
    DETRUIRE      = self.get_cmd('DETRUIRE')

    # 0. faut-il utiliser une table dérivée
    if args['SENSIBILITE']:
        ncomp = self.jdc.memo_sensi.get_nocomp(TABLE.nom, args['SENSIBILITE'].nom)
        sdtab = table_jeveux(ncomp)
        tab = sdtab.EXTR_TABLE()
    else:
        tab = TABLE.EXTR_TABLE()

    # Réinitialiser le titre si on n'est pas réentrant
    if self.reuse is None:
        tab.titr = get_titre_concept(self.sd)

    # Boucle sur les actions à effectuer
    for fOP in ACTION:
        occ = fOP.cree_dict_valeurs(fOP.mc_liste)
        for mc, val in occ.items():
            if val == None:
                del occ[mc]

        # 1. Traitement du FILTRE
        # format pour l'impression des filtres
        form_filtre = '\nFILTRE -> NOM_PARA: %-16s CRIT_COMP: %-4s VALE: %s'
        if occ['OPERATION'] == 'FILTRE':
            # peu importe le type, c'est la meme méthode d'appel
            opts = [occ[k] for k in ('VALE', 'VALE_I', 'VALE_C', 'VALE_K') if occ.has_key(k)]
            kargs = {}
            for k in ('CRITERE', 'PRECISION'):
                if occ.has_key(k):
                    kargs[k] = occ[k]

            col = getattr(tab, occ['NOM_PARA'])
            tab = getattr(col, occ['CRIT_COMP'])(*opts, **kargs)

            # trace l'operation dans le titre
            #if FORMAT in ('TABLEAU', 'ASTER'):
            tab.titr += form_filtre % (occ['NOM_PARA'], occ['CRIT_COMP'], \
                ' '.join([str(v) for v in opts]))

        # 2. Traitement de EXTR
        if occ['OPERATION'] == 'EXTR':
            lpar = occ['NOM_PARA']
            if type(lpar) not in (list, tuple):
                lpar = [lpar]
            for p in lpar:
                if not p in tab.para:
                    UTMESS('F', 'TABLE0_2', valk=[p, TABLE.nom])
            tab = tab[occ['NOM_PARA']]

        # 3. Traitement de SUPPRIME
        if occ['OPERATION'] == 'SUPPRIME':
            lpar = occ['NOM_PARA']
            if type(lpar) not in (list, tuple):
                lpar = [lpar]
            keep = []
            for p in tab.para:
                if not p in lpar:
                    keep.append(p)
            tab = tab[keep]

        # 4. Traitement de RENOMME
        if occ['OPERATION'] == 'RENOMME':
            try:
                tab.Renomme(*occ['NOM_PARA'])
            except KeyError, msg:
                UTMESS('F', 'TABLE0_3', valk=msg)

        # 5. Traitement du TRI
        if occ['OPERATION'] == 'TRI':
            tab.sort(CLES=occ['NOM_PARA'], ORDRE=occ['ORDRE'])

        # 6. Traitement de COMB
        if occ['OPERATION'] == 'COMB':
            tab2 = occ['TABLE'].EXTR_TABLE()
            lpar = []
            if occ.get('NOM_PARA') != None:
                lpar = occ['NOM_PARA']
                if type(lpar) not in (list, tuple):
                    lpar = [lpar]
                for p in lpar:
                    if not p in tab.para:
                        UTMESS('F', 'TABLE0_2', valk=[p, TABLE.nom])
                    if not p in tab2.para:
                        UTMESS('F', 'TABLE0_2', valk=[p, occ['TABLE'].nom] )
            restrict = occ.get('RESTREINT') == 'OUI'
            tab = merge(tab, tab2, lpar, restrict=restrict)

        # 7. Traitement de OPER
        if occ['OPERATION'] == 'OPER':
            # ajout de la colonne dans la table
            tab.fromfunction(occ['NOM_PARA'], occ['FORMULE'])
            if INFO == 2:
                vectval = getattr(tab, occ['NOM_PARA']).values()
                aster.affiche('MESSAGE', 'Ajout de la colonne %s : %s' \
                    % (occ['NOM_PARA'], repr(vectval)))

        # 8. Traitement de AJOUT_LIGNE
        if occ['OPERATION'] == 'AJOUT_LIGNE':
            if len(occ['NOM_PARA']) != len(occ['VALE']):
                UTMESS('F', 'TABLE0_14', valk=('NOM_PARA', 'VALE'))
            dnew = dict(zip(occ['NOM_PARA'], occ['VALE']))
            # ajout de la ligne avec vérification des types
            tab.append(dnew)

        # 9. Traitement de AJOUT_COLONNE
        if occ['OPERATION'] == 'AJOUT_COLONNE':
            if len(occ['NOM_PARA']) != len(occ['VALE']):
                UTMESS('F', 'TABLE0_14', valk=('NOM_PARA', 'VALE'))
            for para, value in zip(occ['NOM_PARA'], occ['VALE']):
                nval = [value, ] * len(tab)
                tab[para] = nval

    # 99. Création de la table_sdaster résultat
    # cas réentrant : il faut détruire l'ancienne table_sdaster
    if self.reuse is not None:
        DETRUIRE(CONCEPT=_F(NOM=TABLE), INFO=1)

    dprod = tab.dict_CREA_TABLE()
    if INFO == 2:
        echo_mess = ['']
        echo_mess.append( repr(tab) )
        from pprint import pformat
        echo_mess.append( pformat(dprod) )
        echo_mess.append('')
        texte_final = os.linesep.join(echo_mess)
        aster.affiche('MESSAGE', texte_final)

    # surcharge par le titre fourni
    tit = args['TITRE']
    if tit != None:
        if type(tit) not in (list, tuple):
            tit = [tit]
        dprod['TITRE'] = tuple(['%-80s' % lig for lig in tit])
    # type de la table de sortie à passer à CREA_TABLE
    tabout = CREA_TABLE(TYPE_TABLE=typ_tabout,
                       **dprod)

    return ier
