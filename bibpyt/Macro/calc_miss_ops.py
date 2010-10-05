#@ MODIF calc_miss_ops Macro  DATE 29/09/2010   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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


def calc_miss_ops(self, OPTION, **kwargs):
    """Macro CALC_MISS :
    Préparation des données et exécution d'un calcul MISS3D
    """
    import aster
    from Utilitai.Utmess  import UTMESS
    from Miss.miss_utils  import MISS_PARAMETER
    from Miss.miss_calcul import CalculMissFactory
    
    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # conteneur des paramètres du calcul
    param = MISS_PARAMETER(initial_dir=os.getcwd(), **kwargs)
    
    # création de l'objet CALCUL_MISS_xxx
    option_calcul = OPTION["MODULE"]
    calcul = CalculMissFactory(option_calcul, self, param)

    try:
        calcul.prepare_donnees()
        calcul.execute()
        calcul.post_traitement()
    except aster.error, err:
        UTMESS('F', err.id_message, valk=err.valk, vali=err.vali, valr=err.valr)

