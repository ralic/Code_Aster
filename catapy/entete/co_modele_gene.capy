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


class modele_gene(ASSD):
   cata_sdj = "SD.sd_modele_gene.sd_modele_gene"

   def LIST_SOUS_STRUCT(self) :
      """ retourne la liste des sous structures du modele generalise
         la liste des macro-elements sous-jacents"""
      if not self.accessible():
         raise Accas.AsException("Erreur dans modele_gene.LIST_SOUS_STRUCT en PAR_LOT='OUI'")
      nommodgen=self.get_name()
      ncham=nommodgen+(8-len(nommodgen))*' '
      ssno=aster.getvectjev(ncham+(14-len(ncham))*' '+'.MODG.SSNO')
      ssme=aster.getcolljev(ncham+(14-len(ncham))*' '+'.MODG.SSME')
      return [([ssno[ind], ssme[ind+1]]) for ind in range(len(ssno))]

   def LIST_LIAIS_STRUCT(self) :
      """ retourne la liste des liaisons entre sous structures du modele generalise sous la forme :
         [ (ss1, nom_liais1,  ss2 , nom_liais2), ...] """
      if not self.accessible() :
         raise Accas.AsException("Erreur dans modele_gene.LIST_LIAIS_STRUCT en PAR_LOT='OUI'")
      nommodgen=self.get_name()
      ncham=nommodgen+(8-len(nommodgen))*' '
      lidf=aster.getcolljev(ncham+(14-len(ncham))*' '+'.MODG.LIDF')
      return [([(lidf[ind][indb]) for indb in range(4)]) for ind in lidf]
