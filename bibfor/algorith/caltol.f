      SUBROUTINE CALTOL(NP3,NBNL,TYPCH,NBSEG,RC,THETA,TOL,TOLC,
     &                  TOLN,TOLV)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2000   AUTEUR KXBADNG T.KESTENS 
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
C-----------------------------------------------------------------------
C DESCRIPTION : CALCUL DES TOLERANCES
C -----------
C               APPELANT : MDITM2
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER    NP3, NBNL, TYPCH(*), NBSEG(*)
      REAL*8     RC(NP3,*), THETA(NP3,*), TOL, TOLC, TOLN, TOLV
C
C VARIABLES LOCALES
C -----------------
      INTEGER    I, IC, NBS, TYPOBS
      REAL*8     TOLX, PI, JEU, A1, B1, R, R1, R2, T
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  ABS, COS, MIN, SIN, SQRT
C
C FONCTIONS EXTERNES
C ------------------
      REAL*8     R8PI
C     EXTERNAL   R8PI
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL   UTMESS
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      TOLX = 1.0D-03
      TOL  = 1.0D+10
      PI   = R8PI()
C
      DO 10 IC = 1, NBNL
         TYPOBS = TYPCH(IC)
         NBS = NBSEG(IC)
         IF ( (TYPOBS.EQ.0).OR.(TYPOBS.EQ.1).OR.(TYPOBS.EQ.2) ) THEN
            JEU = RC(1,IC)
         ELSE
            JEU = RC(1,IC)
            DO 20 I = 2, NBS
               R1 = RC(I-1,IC)
               R2 = RC(I  ,IC)
               T  = THETA(I,IC) - THETA(I-1,IC)
               IF ( T.LT.PI ) THEN
                  A1 = (R2-R1*COS(T))
                  B1 = R1*SIN(T)
                  R = ABS(R2*B1/SQRT(A1*A1+B1*B1))
                  IF ( R.LT.JEU ) JEU = R
               ELSE
                  CALL UTMESS('F','CALTOL','OBSTACLE DE TYPE ' //
     &                        'DISCRET MAL DEFINI (UN ANGLE > PI).')
               ENDIF
  20        CONTINUE
         ENDIF
         TOL = MIN(TOL,TOLX*JEU)
  10  CONTINUE
C
      TOLC = TOL * 10.0D0
      TOLN = TOL * 1.0D-04
      TOLV = TOL * 200.0D0
C
C --- FIN DE CALTOL.
      END
