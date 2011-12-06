      SUBROUTINE DISBUT(NP3,IC,XLOC,TYPOBS,XJEU,RAYON,THETA,NBSEG,
     &                  COST,SINT,DNORM)
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
C DESCRIPTION : CALCUL DE LA DISTANCE NORMALE A LA BUTEE
C -----------
C               APPELANTS : CALND2, CALREF, CALRES, CHVERI, COMPTR,
C                           FTEST1, FTEST2, INIALG, JACBQU, MDCHOE,
C                           MDCHOF, TESTCH, TSTCNT
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER    NP3, IC
      REAL*8     XLOC(*)
      INTEGER    TYPOBS
      REAL*8     XJEU, RAYON(NP3,*), THETA(NP3,*)
      INTEGER    NBSEG
      REAL*8     COST, SINT, DNORM
C
C VARIABLES LOCALES
C -----------------
      INTEGER    I
      REAL*8     ZERO, UN, XLG, SINTNO, COSTNO, TETANO
      REAL*8     R1, R2, T1, T2, Y1, Y2, Z1, Z2, DY, DZ, XLS
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  ABS, ATAN2, COS, SIGN, SIN, SQRT
C
C FONCTIONS EXTERNES
C ------------------
      REAL*8     R8DEPI
C     EXTERNAL   R8DEPI
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      ZERO = 0.0D0
      UN   = 1.0D0
C
C 0.  OBSTACLE PLAN PARALLELE A YLOCAL
C     --------------------------------
      IF ( TYPOBS.EQ.0 ) THEN
C
         XLG = ABS(XLOC(2))
         DNORM = XJEU - XLG
         SINT  = ZERO
         COST  = -SIGN( UN,XLOC(2) )
C
C 1.  OBSTACLE PLAN PARALLELE A ZLOCAL
C     --------------------------------
      ELSE IF ( TYPOBS.EQ.1 ) THEN
C
         XLG = ABS(XLOC(3))
         DNORM = XJEU - XLG
         COST = ZERO
         SINT = -SIGN( UN,XLOC(3) )
C
C 2.  OBSTACLE CIRCULAIRE
C     -------------------
      ELSE IF ( TYPOBS.EQ.2 ) THEN
C
         XLG = SQRT( XLOC(2)*XLOC(2) + XLOC(3)*XLOC(3) )
         DNORM = XJEU - XLG
         IF ( XLG.NE.ZERO ) THEN
            SINT = -XLOC(3) / XLG
            COST = -XLOC(2) / XLG
         ELSE
            SINT =  ZERO
            COST = -UN
         ENDIF
C
C 3.  OBSTACLE DISCRETISE
C     -------------------
      ELSE IF ( TYPOBS.EQ.3 ) THEN
C
         XLG = SQRT( XLOC(2)*XLOC(2) + XLOC(3)*XLOC(3) )
         IF ( XLG.NE.ZERO ) THEN
            SINTNO = XLOC(3) / XLG
            COSTNO = XLOC(2) / XLG
         ELSE
            SINTNO = ZERO
            COSTNO = UN
         ENDIF
         TETANO = ATAN2(SINTNO,COSTNO)
         IF ( TETANO.LT.ZERO ) TETANO = TETANO + R8DEPI()
         DO 10 I = 1, NBSEG-1
            T1 = THETA(I,IC)
            T2 = THETA(I+1,IC)
            IF ( (TETANO.GE.T1).AND.(TETANO.LE.T2) ) THEN
               R1 = RAYON(I,IC)
               R2 = RAYON(I+1,IC)
               Y1 = R1*COS(T1)
               Y2 = R2*COS(T2)
               Z1 = R1*SIN(T1)
               Z2 = R2*SIN(T2)
               DY = Y2-Y1
               DZ = Z2-Z1
               XLS = SQRT( DY*DY + DZ*DZ )
               IF ( XLS.NE.ZERO ) THEN
                  COST = -DZ / XLS
                  SINT =  DY / XLS
               ELSE
                  SINT =  ZERO
                  COST = -UN
               ENDIF
               DNORM = (XLOC(2)-Y1)*COST+(XLOC(3)-Z1)*SINT
               GO TO 999
            ENDIF
  10     CONTINUE
C
      ENDIF
C
 999  CONTINUE
C
C --- FIN DE DISBUT.
      END
