      SUBROUTINE NMCORU(VRESI ,VRESID,CONVOK)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8       VRESI,VRESID
      LOGICAL      CONVOK
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - CONVERGENCE)
C
C VERIFICATION DES CRITERES D'ARRET SUR RESIDU - OPTION PIC
C
C ----------------------------------------------------------------------
C
C
C IN  VRESI  : NORME MAXI DU RESIDU A EVALUER
C IN  VRESID : DONNEE UTILISATEUR POUR CONVERGENCE
C OUT CONVOK . .TRUE. SI CRITERE RESPECTE
C
C ----------------------------------------------------------------------
C
      CONVOK = .TRUE.
      IF ((VRESI.GT.VRESID).OR.(VRESI.LT.0.D0)) THEN
        CONVOK = .FALSE.  
      ENDIF       
      END
