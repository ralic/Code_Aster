      SUBROUTINE I3DCH2(EPSI,SEUIL,MAXITR,FK,M,R,S,IRET)
      IMPLICIT NONE
C
      INTEGER MAXITR,IRET
      REAL*8  EPSI,SEUIL,FK(4,*),M(*),R(*),S(*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     ------------------------------------------------------------------
C     INITIALISATION DU CALCUL DES COORDONEES DE REFERENCE DANS UNE
C     FACE QUAD PLANE PAR DICHOTOMIE-RELAXATION
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  SEUIL  : R : CRITERE D' ARRET
C IN  MAXITR : I : NOMBRE MAX D' ITERATION AUTORISE
C IN  FK     : R : TABLE(1..4,1..2) DES COEF DE LA TRANSFORMATION
C IN  M      : R : TABLE(1..2)      DES COORDONNEES DU POINT
C OUT R      : R : TABLE(1..2)      INTERVALE DE LA COORDO REF 1
C OUT S      : R : TABLE(1..2)      INTERVALE DE LA COORDO REF 2
C OUT IRET   : I : CODE RETOUR :  0 --> SOLUTION TROUVER
C                                 I --> NBR ITER UTILISE
C                                -1 --> DEGENERESCENCE
C     ------------------------------------------------------------------
C
      INTEGER I
      REAL*8  VALFK(3,4),VALR(4),VALS(4),UN,ZERO
      REAL*8  X0,Y0,A,B,C,D,RSUP,RINF,SSUP,SINF,T1,T2
      LOGICAL FINI,EGALR,EGALS
C
C======================================================================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      FINI = .FALSE.
      X0   =  M(1)
      Y0   =  M(2)
      I    =  1
      IRET =  1
      UN   =  1.0D0
      ZERO =  0.0D0
      RSUP =  UN
      RINF = -UN
      SSUP =  UN
      SINF = -UN
10    CONTINUE
      IF ( .NOT. FINI ) THEN
         VALR(1) = RINF
         VALR(2) = RSUP
         VALR(3) = RSUP
         VALR(4) = RINF
         VALS(1) = SINF
         VALS(2) = SINF
         VALS(3) = SSUP
         VALS(4) = SSUP
         CALL I3EFK2(FK,4,VALR,VALS,VALFK)
         D    = UN/(RINF - RSUP)
         A    = VALFK(1,2)
         B    = VALFK(1,1)
         C    =  A
         A    = (B  - A)*D
         B    = (C*RINF - B*RSUP)*D
         T1   = (X0 - B)/A
         A    = VALFK(1,3)
         B    = VALFK(1,4)
         C    =  A
         A    = (B  - A)*D
         B    = (C*RINF - B*RSUP)*D
         T2   = (X0 - B)/A
         RSUP = MAX(-UN,MIN( UN,MAX(T1,T2)))
         RINF = MIN( UN,MAX(-UN,MIN(T1,T2)))
         D    = UN/(SINF - SSUP)
         A    = VALFK(2,3)
         B    = VALFK(2,2)
         C    =  A
         A    = (B  - A)*D
         B    = (C*SINF - B*SSUP)*D
         T1   = (Y0 - B)/A
         A    = VALFK(2,4)
         B    = VALFK(2,1)
         C    =  A
         A    = (B  - A)*D
         B    = (C*SINF - B*SSUP)*D
         T2   = (Y0 - B)/A
         SSUP = MAX(-UN,MIN( UN,MAX(T1,T2)))
         SINF = MIN( UN,MAX(-UN,MIN(T1,T2)))
         CALL RVEGAL(EPSI,'R',RSUP,RINF,EGALR,A)
         CALL RVEGAL(EPSI,'R',SSUP,SINF,EGALS,A)
         IF ( EGALR ) THEN
            IRET = 0
            RSUP = 0.5D0*(RSUP + RINF)
            RINF = RSUP
            IF ( .NOT. EGALS ) THEN
               B = FK(1,2)+FK(2,2)*RSUP
               A = FK(3,2)+FK(4,2)*RSUP
               IF ( A .EQ. ZERO ) THEN
                  IRET = -1
                  FINI = .TRUE.
               ELSE
                  SSUP = (Y0 - B)/A
                  SINF = SSUP
               ENDIF
            ENDIF
         ENDIF
         IF ( EGALS ) THEN
            IRET = 0
            SSUP = 0.5D0*(SSUP + SINF)
            SINF = SSUP
            IF ( .NOT. EGALR ) THEN
               B = FK(1,1)+FK(3,1)*SSUP
               A = FK(2,1)+FK(4,1)*SSUP
               IF ( A .EQ. ZERO ) THEN
                  IRET = -1
                  FINI = .TRUE.
               ELSE
                  RSUP = (X0 - B)/A
                  RINF = RSUP
               ENDIF
            ENDIF
         ENDIF
         D = MAX(ABS(RINF-RSUP),ABS(SINF-SSUP))
         IF ( D .GT. SEUIL ) THEN
            I    =   I + 1
         ELSE
            FINI = .TRUE.
         ENDIF
         FINI = (EGALR .OR. EGALS .OR. FINI .OR. (I .GT. MAXITR) )
         GOTO 10
      ENDIF
      R(1) = RINF
      R(2) = RSUP
      S(1) = SINF
      S(2) = SSUP
      IF ( (IRET . NE. 0) .AND. (IRET .NE. -1) ) THEN
         IRET = I
      ENDIF
      END
