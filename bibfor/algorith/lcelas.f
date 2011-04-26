        SUBROUTINE LCELAS ( LOI,   MOD, NMAT,  MATERD, MATERF,
     1                      MATCST,NVI,
     2                      DEPS, SIGD , VIND,  SIGF,   VINF,
     3                      THETA )
        IMPLICIT   NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C       ----------------------------------------------------------------
C       INTEGRATION ELASTIQUE SUR DT
C       IN  LOI    :  NOM DU MODELE DE COMPORTEMENT
C           MOD    :  MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           VIND   :  VARIABLES INTERNES A T
C           SIGD   :  CONTRAINTE  A T
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C       OUT SIGF   :  CONTRAINTE A T+DT
C           VINF   :  VARIABLES INTERNES A T+DT
C       ----------------------------------------------------------------
        INTEGER         NMAT , NDT    , NDI , NVI
C
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          THETA
        REAL*8          SIGD(6) ,       SIGF(6)
        REAL*8          VIND(*) ,       VINF(*)
        REAL*8          DEPS(6)
C
        CHARACTER*8     MOD
        CHARACTER*16    LOI
        CHARACTER*3     MATCST
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       --------------------------------------------------------------
C
      IF   (LOI(1:8)   .EQ. 'ROUSS_PR'  .OR.
     &      LOI(1:10)  .EQ. 'ROUSS_VISC'    ) THEN
         CALL RSLLIN ( MOD , NMAT, MATERD, MATERF, MATCST,
     &                 DEPS, SIGD, VIND, SIGF, THETA )
      ELSE
C        ELASTICITE LINEAIRE ISOTROPE OU ANISOTROPE
         CALL LCELIN(MOD, NMAT, MATERD, MATERF,
     &               DEPS, SIGD, SIGF)
C
      ENDIF
C
      END
