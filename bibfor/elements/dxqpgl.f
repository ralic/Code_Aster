      SUBROUTINE DXQPGL ( XYZG , PGL )
      IMPLICIT NONE
      REAL*8              XYZG(3,*), PGL(3,3)
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/06/2007   AUTEUR DURAND C.DURAND 
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
C     IN  XYZG  R  12  COORDONNEES  X1 Y1 Z1 X2 Y2 ...
C     OUT PGL   R 3,3  MATRICE DE PASSAGE GLOBAL LOCAL
C     -----------------------------------------------------------------
C     CONSTRUCTION DE LA MATRICE DE PASSAGE GLOBAL --> LOCAL
C     POUR UNE MAILLE TRIANGLE DKQ OU DSQ
C
C            I MILIEU DE 4 1                        3
C            J MILIEU DE 2 3                        *
C            K MILIEU DE 1 2                     L *  *
C            L MILIEU DE 3 4                      *     *
C                                                *        *
C        I : VECTEUR UNITAIRE PORTE PAR IJ    4 *           * J
C                                                *            *
C        K : PERPENDICULAIRE A IJ ET A KL        I*             *
C                                                  *              *
C        J : PRODUIT VECTORIEL K I                  *****************
C                                                  1        K        2
C
C     CALCUL DES MATRICE T1VE ET T2VE DE PASSAGE D'UNE MATRICE,
C     RESPECTIVEMENT (3,3) ET (2,2), DU REPERE DE LA VARIETE AU REPERE
C     DE L'ELEMENT ET T2VE, INVERSE DE T2EV
C
C     VERIFICATION QUE L'ELEMENT EST REELLEMENT PLAN
C
C     ------------------------------------------------------------------
      REAL*8  VX,VY,VZ , XI,YI,ZZI , XJ,YJ,ZZJ , XK,YK,ZZK , XL,YL,ZZL
      REAL*8  NORM , DX , DY , DZ , S , C
      REAL*8  PS , PJDX , PJDY , PJDZ
      REAL*8  X12,Y12,Z12,X13,Y13,Z13,X14,Y14,Z14
      REAL*8  UX, UY, UZ, PSCAL, NORMU, NORM4, DIST
      INTEGER IADZI,IAZK24
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      REAL*8 VALR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      XI  = (XYZG(1,1) + XYZG(1,4))/2.D0
      YI  = (XYZG(2,1) + XYZG(2,4))/2.D0
      ZZI  = (XYZG(3,1) + XYZG(3,4))/2.D0
      XJ  = (XYZG(1,3) + XYZG(1,2))/2.D0
      YJ  = (XYZG(2,3) + XYZG(2,2))/2.D0
      ZZJ  = (XYZG(3,3) + XYZG(3,2))/2.D0
      XK  = (XYZG(1,2) + XYZG(1,1))/2.D0
      YK  = (XYZG(2,2) + XYZG(2,1))/2.D0
      ZZK  = (XYZG(3,2) + XYZG(3,1))/2.D0
      XL  = (XYZG(1,4) + XYZG(1,3))/2.D0
      YL  = (XYZG(2,4) + XYZG(2,3))/2.D0
      ZZL  = (XYZG(3,4) + XYZG(3,3))/2.D0
C
      NORM = SQRT((XJ-XI)*(XJ-XI)+(YJ-YI)*(YJ-YI)+(ZZJ-ZZI)*(ZZJ-ZZI))
      PGL(1,1) = (XJ-XI)/NORM
      PGL(1,2) = (YJ-YI)/NORM
      PGL(1,3) = (ZZJ-ZZI)/NORM
C
      VX  =   (YJ-YI)*(ZZL-ZZK) - (ZZJ-ZZI)*(YL-YK)
      VY  = - (XJ-XI)*(ZZL-ZZK) + (ZZJ-ZZI)*(XL-XK)
      VZ  =   (XJ-XI)*(YL-YK) - (YJ-YI)*(XL-XK)
C
      NORM  = SQRT(VX*VX + VY*VY + VZ*VZ)
      PGL(3,1) = VX / NORM
      PGL(3,2) = VY / NORM
      PGL(3,3) = VZ / NORM
C
      PGL(2,1) =   PGL(3,2)*PGL(1,3) - PGL(3,3)*PGL(1,2)
      PGL(2,2) = - PGL(3,1)*PGL(1,3) + PGL(3,3)*PGL(1,1)
      PGL(2,3) =   PGL(3,1)*PGL(1,2) - PGL(3,2)*PGL(1,1)
C
      NORM = SQRT ( PGL(2,1) * PGL(2,1) + PGL(2,2) * PGL(2,2) +
     >             PGL(2,3) * PGL(2,3) )
      PGL(2,1) = PGL(2,1) / NORM
      PGL(2,2) = PGL(2,2) / NORM
      PGL(2,3) = PGL(2,3) / NORM
C
C
C          VERIFICATION DE LA PLANEITE : 
C     CALCUL DE : T14 P_SCAL (T12 P_VECT T13)
C
C     DEFINITION DU VECTEUR T12 (VECTEUR DE DIR 1 A 2)
C
      X12 = XYZG(1,2) - XYZG(1,1)
      Y12 = XYZG(2,2) - XYZG(2,1)
      Z12 = XYZG(3,2) - XYZG(3,1)
C
C     DEFINITION DU VECTEUR T13 (VECTEUR DE DIR 1 A 3)
      X13 = XYZG(1,3) - XYZG(1,1)
      Y13 = XYZG(2,3) - XYZG(2,1)
      Z13 = XYZG(3,3) - XYZG(3,1)
C     
C     DEFINITION DU VECTEUR T14 (VECTEUR DE DIR 1 A 4)
      X14 = XYZG(1,4) - XYZG(1,1)
      Y14 = XYZG(2,4) - XYZG(2,1)
      Z14 = XYZG(3,4) - XYZG(3,1)
C
C     U = (VECTEUR) T12 P_VECT T13
      UX = (Y12*Z13) - (Y13*Z12)
      UY = (Z12*X13) - (Z13*X12)
      UZ = (X12*Y13) - (X13*Y12)
C
C     PSCAL = (SCALAIRE) T14 P_SCAL U
      PSCAL = (UX*X14) + (UY*Y14) + (UZ*Z14)
C
C     DISTANCE DU POINT 4 AU PLAN (123)
      NORMU = SQRT( (UX*UX) + (UY*UY) + (UZ*UZ) )
      NORM4 = SQRT( (X14*X14) + (Y14*Y14) + (Z14*Z14) )
      DIST  = PSCAL / NORMU
      PSCAL = DIST  / NORM4
C
C     TESTE SI PSCAL > EPS (1D-4 EN DUR DANS LE FORTRAN)
C
      IF ( ABS(PSCAL).GT.1.D-4 ) THEN
        CALL TECAEL(IADZI,IAZK24)
        VALR = ABS(DIST)
        CALL U2MESG('A', 'ELEMENTS4_80',1,ZK24(IAZK24+2),0,0,1,VALR)
      ENDIF

      END
