#@ MODIF pre_gmsh_ops Macro  DATE 11/06/2002   AUTEUR DURAND C.DURAND 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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

def pre_gmsh_ops(self,UNITE_MAILLAGE,UNITE_GMSH,MODI_QUAD,**args):
  """
     Ecriture de la macro PRE_GMSH
  """
  import os
  from Macro.ajout_quad_gmsh import ajout_quad_gmsh
  ier=0

  PRE_GMSH_LECT =self.get_cmd('PRE_GMSH_LECT')

  # La macro compte pour 1 dans la numerotation des commandes
  self.icmd=1

  if MODI_QUAD=='OUI':
     cur_dir=os.getcwd()
     unit  = str(UNITE_GMSH)
     nomFichierGmsh = cur_dir+'/fort.'+unit
     nomFichierMail = cur_dir+'/sortie'

#    récupération du fichier .msh complet mis dans la string 'texte'

     fproc=open(nomFichierGmsh,'r')
     texte=fproc.read()
     fproc.close()

     resu=ajout_quad_gmsh(texte)
     if not resu:
        ier=ier+1
        self.cr.fatal("Erreur dans la methode python de transformation mailles lineaires-quadratiques")
        return ier

     fsort=open(nomFichierMail,'w')
     fsort.write(resu)
     fsort.close()
     os.system('cp '+nomFichierMail+' '+nomFichierGmsh)

  PRE_GMSH_LECT(UNITE_MAILLAGE = UNITE_MAILLAGE,
                UNITE_GMSH     = UNITE_GMSH     )

  return ier

