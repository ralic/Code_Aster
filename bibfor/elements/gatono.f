      SUBROUTINE GATONO(VALGAU,VALNO,NCMP,NNO,NGA)
      IMPLICIT NONE
      INTEGER I,NCMP,NNO,NGA
      REAL*8 VALGAU(NCMP,NGA),VALNO(NCMP,NNO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/07/2003   AUTEUR LAVERNE J.LAVERNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C INTERPOLATION LINEAIRE DES VALEURS DE SIEF_ELGA ET VARI_ELGA
C AU NOEUD DE L'ELEMENT.
C IE : OPTIONS SIEF_ELNO_ELGA ET VARI_ELNO_ELGA
C
C IN  : VALGAU,NCMP,NNO,NGA
C OUT : VALNO
C-----------------------------------------------------------------------

      DO 10 I=1,NCMP
       
        VALNO(I,1) = VALGAU(I,1)   + 
     &                 (VALGAU(I,2)-VALGAU(I,1))*(1-SQRT(3.D0))/2
        VALNO(I,4) = VALGAU(I,1)   + 
     &                 (VALGAU(I,2)-VALGAU(I,1))*(1-SQRT(3.D0))/2
        
        VALNO(I,2) = VALGAU(I,1)   + 
     &                 (VALGAU(I,2)-VALGAU(I,1))*(1+SQRT(3.D0))/2
        VALNO(I,3) = VALGAU(I,1)   + 
     &                 (VALGAU(I,2)-VALGAU(I,1))*(1+SQRT(3.D0))/2
      
   10 CONTINUE                
      END
