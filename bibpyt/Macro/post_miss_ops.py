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
# person_in_charge: mathieu.courtois at edf.fr

import sys
import os
import traceback


def post_miss_ops(self, **kwargs):
    """Macro POST_MISS :
    Post-traitement d'un calcul MISS3D
    """
    import aster
    from Utilitai.Utmess  import UTMESS
    from Miss.miss_post import PostMissFactory
    
    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # création de l'objet POST_MISS_xxx
    post = PostMissFactory(kwargs['OPTION'], self, kwargs)

    try:
        post.argument()
        post.execute()
        post.sortie()
    except aster.error, err:
        UTMESS('F', err.id_message, valk=err.valk, vali=err.vali, valr=err.valr)
    except Exception, err:
        trace = ''.join(traceback.format_tb(sys.exc_traceback))
        UTMESS('F', 'SUPERVIS2_5', valk=('POST_MISS', trace, str(err)))
