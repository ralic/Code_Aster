      SUBROUTINE FOC1MA( NBVAR,VAR,FON, NBMAX,VARMAX,FONMAX  )
      IMPLICIT NONE
      INTEGER            NBVAR,         NBMAX
      REAL*8                   VAR(*),FON(*), VARMAX(*), FONMAX(*)
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     "MEDISIS"   CALCUL DES MAXIMA D'UNE FONCTION
C     ----------------------------------------------------------------
      REAL*8          LEMAX,EPSD,EPS
C     ----------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I 
C-----------------------------------------------------------------------
      EPSD      = 1.D-6
      LEMAX     = ABS(FON(1))
      EPS       = EPSD * LEMAX
      NBMAX     = 1
      VARMAX(1) = VAR(1)
      FONMAX(1) = FON(1)
      DO 100 I = 2 , NBVAR
         IF ( ABS(FON(I)) .GE. LEMAX-EPS ) THEN
            IF ( ABS(FON(I)) .GT. LEMAX+EPS ) THEN
               NBMAX = 1
               LEMAX = ABS(FON(I))
               EPS   = EPSD * LEMAX
               VARMAX(NBMAX) = VAR(I)
               FONMAX(NBMAX) = FON(I)
            ELSE
               NBMAX = NBMAX + 1
               VARMAX(NBMAX) = VAR(I)
               FONMAX(NBMAX) = FON(I)
            ENDIF
         ENDIF
  100 CONTINUE
      END
