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
def rest_mode_nonl_prod(TYPE_RESU,**args):
    if TYPE_RESU == 'DYNA_TRANS' : return dyna_trans
    elif TYPE_RESU == 'MODE_MECA' : return mode_meca
    raise AsException("type de concept resultat non prevu")

REST_MODE_NONL=OPER(nom="REST_MODE_NONL", op=63,
         sd_prod=rest_mode_nonl_prod, reentrant='n',
         fr=tr("Post traitement de mode_non_line : récuperation résultats"),
         UIinfo={"groupes":("Post-traitements",)},

         MODE_NON_LINE    =SIMP(statut='o',typ=table_container,max=1),
         TYPE_RESU    =SIMP(statut='o',typ='TXM',into=('MODE_MECA','DYNA_TRANS'),defaut='DYNA_TRANS',max=1),
         NUME_ORDRE      =SIMP(statut='o',typ='I',max=1),
         b_dyna_trans  =BLOC(condition="TYPE_RESU=='DYNA_TRANS'",
                NB_INST =SIMP(statut='f',typ='I',max=1,defaut=512),),
         INFO          =SIMP(statut='f',typ='I',defaut= 1,into=(1,2) ),

)  ;
