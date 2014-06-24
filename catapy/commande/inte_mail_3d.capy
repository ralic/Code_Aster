# ======================================================================
# COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: xavier.desroches at edf.fr
INTE_MAIL_3D=OPER(nom="INTE_MAIL_3D",op=96,sd_prod=surface_sdaster,
            UIinfo={"groupes":("Post-traitements",)},
                  fr=tr("Définir un chemin de type segment de droite dans un maillage 3D"),reentrant='n',
         MAILLAGE        =SIMP(statut='o',typ=maillage_sdaster),
         TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
         MAILLE          =SIMP(statut='f',typ=ma  ,validators=NoRepeat(),max='**'),
         DEFI_SEGMENT    =FACT(statut='o',max='**',
           regles=(UN_PARMI('ORIGINE','NOEUD_ORIG','GROUP_NO_ORIG'),
                   UN_PARMI('EXTREMITE','NOEUD_EXTR','GROUP_NO_EXTR'),),
           ORIGINE         =SIMP(statut='f',typ='R',min=3,max=3),  
           NOEUD_ORIG      =SIMP(statut='f',typ=no,),
           GROUP_NO_ORIG   =SIMP(statut='f',typ=grno,),
           EXTREMITE       =SIMP(statut='f',typ='R',min=3,max=3),  
           NOEUD_EXTR      =SIMP(statut='f',typ=no,),
           GROUP_NO_EXTR   =SIMP(statut='f',typ=grno,),
         ),
         PRECISION       =SIMP(statut='f',typ='R',defaut=1.0E-6),  
         INFO            =SIMP(statut='f',typ='I',defaut=1,into=(1,2)),
)  ;
