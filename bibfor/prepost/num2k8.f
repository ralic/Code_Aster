      SUBROUTINE NUM2K8(NOMGD,TGLOK8,TLOCK8,NBLK8,TIND)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                               NBLK8,TIND(*)
      CHARACTER*8       NOMGD,TGLOK8(*),TLOCK8(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C ----------------------------------------------------------------------
C     COPIE DE NUMEK8
C     TIND(I) <-- INDICE DANS LE TABLEAU TGLOK8 DE L' ELEMEMT
C                 NUMERO I DE TLOCK8
C                 (NBLK8 : DIMENSION DE TLOCK8)
C
C ----------------------------------------------------------------------
C
      IF ( NOMGD(1:6).EQ.'SIEF_R') THEN
         DO 10, I = 1, NBLK8, 1
            TIND (I) = 0
C  COMPOSANTES TRAITEES: SIXX SIYY SIZZ SIXY SIXZ SIYZ
            DO 20, J = 1, 6, 1
               IF ( TLOCK8(I) .EQ. TGLOK8(J) ) THEN
                  TIND(I) = J
                  GOTO 10
               ENDIF
 20         CONTINUE
C  COMPOSANTES TRAITEES: NXX NYY NXY MXX MYY MXY 
            DO 22, J = 14, 19, 1
               IF ( TLOCK8(I) .EQ. TGLOK8(J) ) THEN
                  TIND(I) = J
                  GOTO 10
               ENDIF
 22         CONTINUE
 10      CONTINUE
C
      ELSEIF ( NOMGD(1:6).EQ.'EPSI_R') THEN
C  COMPOSANTES TRAITEES: EPXX EPYY EPZZ EPXY EPXZ EPYZ
C                        EXX EYY EXY KXX KYY KXY
         DO 30, I = 1, NBLK8, 1
            TIND (I) = 0
            DO 40, J = 1, 12, 1
               IF ( TLOCK8(I) .EQ. TGLOK8(J) ) THEN
                  TIND(I) = J
                  GOTO 30
               ENDIF
 40         CONTINUE
 30      CONTINUE
C
      ELSE
         DO 50, I = 1, NBLK8, 1
            TIND (I) = 0
 50      CONTINUE
      ENDIF
C
      END
