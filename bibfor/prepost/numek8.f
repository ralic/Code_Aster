      SUBROUTINE NUMEK8(TGLOK8,TLOCK8,NBGK8,NBLK8,TIND)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
      CHARACTER*8 TLOCK8(*),TGLOK8(*)
      INTEGER     TIND(*),NBLK8,NBGK8
C
C***********************************************************************
C
C     TIND(I) <-- INDICE DANS LE TABLEAU TGLOK8 DE L' ELEMEMT
C                 NUMERO I DE TLOCK8
C                 (NBLK8 : DIMENSION DE TLOCK8)
C                 (NBGK8 : DIMENSION DE TGLOK8)
C
C***********************************************************************
C
      CHARACTER*8 NLK8
      LOGICAL     TROUVE
      INTEGER     I,J
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      TROUVE = .FALSE.
C
      I = 0
      J = 0
C
      DO 10, I =1, NBLK8, 1
C
         TIND (I) = 0
C
10    CONTINUE
C
      DO 100, I = 1, NBLK8, 1
C
         NLK8 = TLOCK8(I)
C
         J = 0
C
         TROUVE = .FALSE.
C
110      CONTINUE
         IF ( (.NOT. TROUVE) .AND. (J .LT. NBGK8) ) THEN
C
            J = J + 1
C
            IF ( NLK8 .EQ. TGLOK8(J) ) THEN
C
               TROUVE = .TRUE.
C
            ENDIF
C
            GOTO 110
C
         ENDIF
C
         IF ( TROUVE ) THEN
C
            TIND(I) = J
C
         ENDIF
C
100   CONTINUE
C
      END
