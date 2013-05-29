# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

########################################################################
def macr_ecla_pg_ops(self,RESULTAT,MAILLAGE,RESU_INIT,MODELE_INIT,
                     TOUT, GROUP_MA, MAILLE,
                     SHRINK, TAILLE_MIN,
                     NOM_CHAM, TOUT_ORDRE, NUME_ORDRE, LIST_ORDRE, INST, LIST_INST, PRECISION, CRITERE,
                     **args):
    """
       Ecriture de la macro macr_ecla_pg
    """
    import os, string
    from Accas import _F
    from Noyau.N_utils import AsType
    ier=0

    # On importe les definitions des commandes a utiliser dans la macro
    CREA_MAILLAGE  =self.get_cmd('CREA_MAILLAGE')
    CREA_RESU      =self.get_cmd('CREA_RESU')


    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)


    # Appel à CREA_MAILLAGE :
    motscles={}
    if   TOUT      : motscles['TOUT']       =TOUT
    if   GROUP_MA  : motscles['GROUP_MA']   =GROUP_MA
    if   MAILLE    : motscles['MAILLE']     =MAILLE

    self.DeclareOut('ma2',MAILLAGE)
    ma2=CREA_MAILLAGE(ECLA_PG=_F( MODELE = MODELE_INIT,  NOM_CHAM=NOM_CHAM,
                                  SHRINK = SHRINK, TAILLE_MIN=TAILLE_MIN, **motscles ));



    # Appel à CREA_RESU :
    typ2=AsType(RESU_INIT).__name__
    if   TOUT_ORDRE         : motscles['TOUT_ORDRE']     =TOUT_ORDRE
    if   NUME_ORDRE != None : motscles['NUME_ORDRE']     =NUME_ORDRE
    if   LIST_ORDRE         : motscles['LIST_ORDRE']     =LIST_ORDRE
    if   LIST_INST          : motscles['LIST_INST']      =LIST_INST
    if   INST != None       : motscles['INST']           =INST

    self.DeclareOut('resu2',RESULTAT)
    resu2=CREA_RESU( OPERATION='ECLA_PG', TYPE_RESU=string.upper(typ2),
                    ECLA_PG=_F( MODELE_INIT= MODELE_INIT, RESU_INIT=RESU_INIT, NOM_CHAM=NOM_CHAM,
                                MAILLAGE= ma2, **motscles ));
    return ier
############################################################################################
