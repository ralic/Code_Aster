      SUBROUTINE NMLECV(SDERRO,NOMBCL,LCONV )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*24 SDERRO
      CHARACTER*4  NOMBCL
      LOGICAL      LCONV
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C BOUCLE CONVERGEE OU PAS ?
C
C ----------------------------------------------------------------------
C
C
C IN  SDERRO : SD GESTION DES ERREURS
C IN  NOMBCL : NOM DE LA BOUCLE
C               'RESI' - RESIDUS D'EQUILIBRE
C               'NEWT' - BOUCLE DE NEWTON
C               'FIXE' - BOUCLE DE POINT FIXE
C               'INST' - BOUCLE SUR LES PAS DE TEMPS
C OUT LCONV  : .TRUE. SI LA BOUCLE EST DANS L'ETAT CONVERGE
C
C
C
C
      CHARACTER*4  ETABCL
C
C ----------------------------------------------------------------------
C
      LCONV  = .FALSE.
      CALL NMLEEB(SDERRO,NOMBCL,ETABCL)
      LCONV  = (ETABCL.EQ.'CONV')

      END
