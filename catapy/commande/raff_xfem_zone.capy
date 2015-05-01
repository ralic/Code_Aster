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
# person_in_charge: samuel.geniaut at edf.fr

# commande cachee appelee uniquement par la macro RAFF_XFEM

RAFF_XFEM_ZONE=OPER(nom="RAFF_XFEM_ZONE",
                    op=188,
#                    sd_prod=cham_elem,
                    sd_prod=carte_sdaster,
                    fr=tr("Calcul d'un indicateur binaire pour le raffinement"),
                    reentrant='n',
                    UIinfo={"groupes":("Résultats et champs","Rupture",)},

                    FISSURE=SIMP(statut='o',typ=fiss_xfem,min=1,max=1),
                    RAYON  =SIMP(statut='o',typ='R',val_min=0.),                

                    )  ;
