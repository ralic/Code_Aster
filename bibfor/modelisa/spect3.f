       FUNCTION SPECT3 ( X, A, B, F, TOL, COEFF,XLC,VITN,DEFM,
     +                  RHOE, NBP,IM,JM )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C **********************************************************************
       IMPLICIT REAL*8 (A-H,O-Z)
C
C                                 A
C DESCRIPTION : CALCULE DINTG2 = S F(X,Y) DY   POUR DINTEG.
C -----------                     B
C
C               (S EST LE SYMBOLE DE L'INTEGRALE).
C
C               TOL DONNE LE SEUIL DE CONVERGENCE RELATIVE.
C
C               F EST LA FONCTION A INTEGRER.
C               ELLE DOIT ETRE DECLAREE EXTERNAL DANS L'APPELANT.
C               SA SPECIFICATION EST :
C                        DOUBLE PRECISION FUNCTION F ( X, Y )
C                        DOUBLE PRECISION X, Y
C
C               A ET B DONNENT LES BORNES DE L'INTEGRALE.
C               COEFF EST LE TABLEAU DES COEFFICIENTS FOURNI PAR DINTEG.
C
C *****************   DECLARATIONS DES VARIABLES   *********************
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C ARGUMENTS
C ---------
      REAL*8   X, A, B, F, TOL, COEFF(*)
      REAL*8   VITN(NBP,*), RHOE(NBP,*), DEFM(NBP,*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER          INDEX, N1, N2, I, ARRET
      REAL*8 RES, YM, DY, Y0, R1, SOM, Y
      REAL*8 W(127)
C
C *****************    DEBUT DU CODE EXECUTABLE    *********************
C
      RES = 0.0D0
C
      IF ( ABS(A-B) .LT. 1.0D-30 ) THEN
         SPECT3 = RES
         GOTO 9999
      END IF
C
      YM = ( A + B ) / 2.0D0
      DY = ( B - A ) / 2.0D0
      Y0 = F ( X, YM ,XLC,VITN,RHOE,DEFM,NBP,IM,JM)
      R1 = (Y0+Y0) * DY
      INDEX = 0
      N1    = 0
      N2    = 1
      SOM   = 0.0D0
C
C --- REPETER ...
C
   10 CONTINUE
      N1 = N1 + N2
      DO 20 I = N2, N1
         INDEX = INDEX + 1
         Y = COEFF(INDEX) * DY
         W(I) = F(X,YM+Y,XLC,VITN,RHOE,DEFM,NBP,IM,JM) +
     +          F(X,YM-Y,XLC,VITN,RHOE,DEFM,NBP,IM,JM)
         INDEX = INDEX + 1
         SOM = SOM + COEFF(INDEX)*W(I)
   20 CONTINUE
      N2 = N1 + 1
      INDEX = INDEX + 1
      RES = ( SOM + COEFF(INDEX)*Y0 ) * DY
C
C --- TEST DE CONVERGENCE.
C
      IF ( ABS(RES-R1) .LE. ABS(R1*TOL) ) THEN
         ARRET = 1
      ELSE
         IF ( N1 .GE. 127 ) THEN
            ARRET = 1
         ELSE
            ARRET = 0
            R1 = RES
            SOM = 0.0D0
            DO 22 I = 1, N1
               INDEX = INDEX + 1
               SOM = SOM + COEFF(INDEX)*W(I)
   22       CONTINUE
         END IF
      END IF
C
C --- JUSQUE ARRET = 1.
C
      IF ( ARRET .EQ. 0 ) GO TO 10
C
      SPECT3 = RES
C
 9999 CONTINUE
      END
