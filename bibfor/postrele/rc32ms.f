      SUBROUTINE RC32MS (MECA,SA,SB,CMAX )
      IMPLICIT   NONE
      REAL*8     SB(2),SA(2)
      LOGICAL    CMAX,MECA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 16/02/2009   AUTEUR GALENNE E.GALENNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     IDENTIFICATION DE SP MAX 
C    IN  - MECA : SI TRUE : CHARGEMENT MECANIQUE PUR
C    IN - SB : AMPLITUDE POUR LA COMBINAISON B
C    VAR - SA : EN ENTREE :AMPLITUDE POUR LA COMBINAISON A
C               EN SORTIE : AMPLITUDE MAX(SA,SB)
C    OUT - CMAX : TRUE SI SB > SA
C     ------------------------------------------------------------------
      INTEGER       I3
C DEB ------------------------------------------------------------------
C
      CMAX = .FALSE.
C
C CAS SANS THERMIQUE   
C   
      IF (MECA) THEN
        IF (SB(1).GT.SA(1)) THEN
           SA(2) = SA(1)
           SA(1) = SB(1)
           CMAX = .TRUE.
        ELSEIF (SB(1).GT.SA(2)) THEN
           SA(2) = SB(1)
        ENDIF
C
C CAS GENERAL
C
      ELSE
        DO 2 I3 = 1,2
          IF (SB(I3).GT.SA(I3)) THEN
            SA(I3) = SB(I3)
            CMAX = .TRUE.
         ENDIF
  2     CONTINUE  

      ENDIF
C
      END
