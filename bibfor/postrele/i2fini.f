      SUBROUTINE I2FINI (EPSI,BINF,BSUP,TSOR,TSEX,TM2,ADRGT,FINI)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER ADRGT,TM2(*)
      REAL*8  TSOR(*),TSEX(*),EPSI,BINF,BSUP
      LOGICAL FINI
C
      INTEGER I
      LOGICAL STOP
C
      FINI = .FALSE.
      STOP = .FALSE.
      I    = 2
C
      IF ( ADRGT .GE. 2 ) THEN
C
         IF ( (ABS(TSOR(1) - BINF) .LT. EPSI)
     +        .AND.
     +        (ABS(TSEX(ADRGT-1) - BSUP) .LT. EPSI)
     +        .AND.
     +        (TM2(1) .NE. -1)
     +      ) THEN
C
10           CONTINUE
             IF ( (.NOT. STOP) .AND. (I .LT. ADRGT) ) THEN
C
                IF ( (TM2(I) .NE. -1)
     +               .AND.
     +               (ABS(TSOR(I)-TSEX(I-1)).LT. EPSI)
     +             ) THEN
C
                   I = I+1
C
                ELSE
C
                   STOP = .TRUE.
C
                ENDIF
C
                GOTO 10
C
             ENDIF
C
             FINI = .NOT. STOP
C
         ENDIF
C
      ENDIF
C
      END
