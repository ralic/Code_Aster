      SUBROUTINE NM1TRA(IMATE,TP,DEFM,DEPS,
     &                  EPSPM,PM,
     &                  SIG,EPSPP,PP,DSDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
C ----------------------------------------------------------------------
C      PLASTICITE VON MISES ISOTROPE A PARTIR D'UNE COURBE DE TRACTION
C
C IN  IMAT     : MATERIAU
C IN  TP       : TEMPERATURE PLUS
C
C IN  DEFM    : DEFORMATION MOINS
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  EPSPM   : DEFORMATION  PLASTIQUE MOINS
C IN  PM      : DEFORMATION  PLASTIQUE CUMULEE MOINS
C
C OUT SIG     : CONTRAINTES PLUS
C OUT EPSPP   : DEFORMATION  PLASTIQUE PLUS
C OUT PP      : DEFORMATION  PLASTIQUE CUMULEE PLUS
C OUT DSDEP   : DSIG/DEPS TEMPS PLUS
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL*8        TP,DEFM
      REAL*8        DEPS,EPSPM,PM
      REAL*8        SIG,EPSPP,PP,DSDEP
      INTEGER       IMATE
C     ------------------------------------------------------------------
C     VARIABLES LOCALES
C     ------------------------------------------------------------------
      REAL*8        EP,DUM,RM,RP,SIG1,ZERO
      REAL*8        DPCUM,DEPSIP
      INTEGER       JPROLP,JVALEP,NBVALP
C     ------------------------------------------------------------------

      ZERO = 0.0D0

C     RECUPERATION DE YOUNG A TP
      CALL RCTRAC(IMATE,1,'SIGM',TP,JPROLP,JVALEP,
     &            NBVALP,EP)

C     CALCUL DU SIGMA1
      SIG1 = EP*( DEFM + DEPS - EPSPM )

C     CALCUL DE R(P-)
      CALL RCFONC('V',1,JPROLP,JVALEP,NBVALP,DUM,DUM,
     &            DUM,PM,RM,DUM,DUM,DUM,DUM)

      IF ( (ABS(SIG1) - RM) .LT. ZERO ) THEN
        SIG   = SIG1
        EPSPP = EPSPM
        PP    = PM
        DSDEP = EP
      ELSE
        DPCUM = ( ABS(SIG1) - RM ) / EP
        PP   = PM + DPCUM
C       CALCUL DE R(P+)
        CALL RCFONC('V',1,JPROLP,JVALEP,NBVALP,DUM,DUM,
     &              DUM,PP,RP,DUM,DUM,DUM,DUM)
        DEPSIP = ( RP - RM )/EP
        IF ( DEPS .GT. ZERO ) THEN
          EPSPP = EPSPM + ( DPCUM - DEPSIP )
          DSDEP = ABS((RP - EP*( DEFM - EPSPM ))/DEPS)
        ELSEIF ( DEPS .LT. ZERO ) THEN
          EPSPP = EPSPM - ( DPCUM - DEPSIP )
          DSDEP = ABS((RP - EP*( DEFM - EPSPM ))/DEPS)
        ELSE
          EPSPP = EPSPM + ( DPCUM - DEPSIP )
          DSDEP = EP
        ENDIF
        SIG   = EP*( DEFM + DEPS - EPSPP )

      ENDIF

      END
