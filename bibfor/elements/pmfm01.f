      SUBROUTINE PMFM01(KANL,XL,CARS,M)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/11/2001   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT NONE
      INTEGER KANL
      REAL*8 CARS(6),XL
      REAL*8 M(*)
C     ------------------------------------------------------------------
C     CALCUL DE LA MATRICE DE MASSE DES ELEMENTS DE POUTRE MULTIFIBRES
C     PAR LA METHODE
C          - DES MASSES CONCENTREES
C          - DES MASSES EQUIVALENTES
C     ------------------------------------------------------------------
C IN  KANL       - TYPE DE MODELISATION DES MASSES
C IN               KANL = 0 MASSES CONCENTREES FORMULATION S.D.R.C.
C IN               KANL = 1 MASSES COHERENTE
C IN R*8  ! CARS   !     6   ! CARACTERISTIQUES INTEGREES DE LA SECTION
C              INT(VAR DS) = INTEGRALE DE VAR SUR LA SECTION
C              RHO : MASSE VOLUMIQUE
C              Y,Z : COORDONNEES DE LA FIBRE

C IN R*8  ! CARS(1)!     -   ! RA   = INT(RHO. DS)
C IN R*8  ! CARS(2)!     -   ! RSZ  = INT(RHO.Y DS)
C IN R*8  ! CARS(3)!     -   ! RSY  = INT(RHO.Z DS)
C IN R*8  ! CARS(4)!     -   ! RIZ  = INT(RHO.Y.Y DS)
C IN R*8  ! CARS(5)!     -   ! RIY  = INT(RHO.Z.Z DS)
C IN R*8  ! CARS(6)!     -   ! RPYZ = INT(RHO.Y.Z DS)
C OUT M          -(78) MATRICE DE MASSE ELEMENT


      REAL*8 ZERO,DEUX,TROIS,CINQ,SIX,SEPT,NEUF,DIX,ONZE,DOUZE,TREIZE
      REAL*8 QUINZE,VINGT,TRENTE,V35,V70
      REAL*8 V105,V140,V210,V420
      REAL*8 V48

      PARAMETER (ZERO=0.0D+0,DEUX=2.0D+0,TROIS=3.0D+0,CINQ=5.0D+0,
     &          SIX=6.0D+0,SEPT=7.0D+0,NEUF=9.0D+0,DIX=1.0D+1,
     &          ONZE=1.1D+1,DOUZE=1.2D+1,TREIZE=1.3D+1,QUINZE=1.5D+1,
     &          VINGT=2.0D+1,V35=3.5D+1,V48=4.8D+1,V70=7.0D+1,
     &          TRENTE=3.0D+1,V105=1.05D+2,V140=1.4D+2,V210=2.1D+2,
     &          V420=4.2D+2)
      REAL*8 CO13,CO112,CO1335,CO65,CO720,C11210,CO970,CO320,C13420
      REAL*8 CO120,CO1302,CO130,CO1105,CO215,CO1140
      REAL*8 MS11,MS22,MS33,MS44,MS55,MS66,MS15,MS16,MS24,MS34,MS56
      REAL*8 ZAIRE,ZINEX,C
      INTEGER IP(12),I
      DATA IP/0,1,3,6,10,15,21,28,36,45,55,66/


      IF (KANL.EQ.0) THEN
C     ------------------------------------------------------------------
C               MASSES CONCENTREES FORMULATION S.D.R.C. KANL =0
C     ------------------------------------------------------------------
C     INITIALISATION
        DO 10 I = 1,78
          M(I) = ZERO
   10   CONTINUE
        ZAIRE = CARS(1)*XL/DEUX
        ZINEX = XL* (CARS(4)+CARS(5))/DEUX
        C = DEUX*ZAIRE*XL
        C = MIN(C*XL/V105,C/V48)
        M(IP(1)+1) = ZAIRE
        M(IP(2)+2) = ZAIRE
        M(IP(3)+3) = ZAIRE
        M(IP(4)+4) = ZINEX
        M(IP(5)+5) = C + DEUX*CARS(5)*XL/QUINZE
        M(IP(6)+6) = C + DEUX*CARS(4)*XL/QUINZE
        M(IP(7)+7) = ZAIRE
        M(IP(8)+8) = ZAIRE
        M(IP(9)+9) = ZAIRE
        M(IP(10)+10) = ZINEX
        M(IP(11)+11) = M(IP(5)+5)
        M(IP(12)+12) = M(IP(6)+6)
      ELSE
C     ------------------------------------------------------------------
C                       MASSES COHERENTES   KANL = 1
C     ------------------------------------------------------------------
        MS11 = CARS(1)
        MS22 = CARS(1)
        MS33 = CARS(1)
        MS44 = CARS(4) + CARS(5)
        MS55 = CARS(5)
        MS66 = CARS(4)
        MS15 = CARS(3)
        MS16 = -CARS(2)
        MS24 = -MS15
        MS34 = -MS16
        MS56 = -CARS(6)

        CO13 = XL/TROIS
        CO112 = XL/DOUZE

        CO1335 = TREIZE*XL/V35
        CO65 = SIX/ (CINQ*XL)
        CO720 = SEPT*XL/VINGT
        C11210 = ONZE*XL*XL/V210
        CO970 = NEUF*XL/V70
        CO320 = TROIS*XL/VINGT
        C13420 = TREIZE*XL*XL/V420

        CO120 = XL*XL/VINGT
        CO1302 = XL*XL/TRENTE

        CO130 = XL/TRENTE
        CO1105 = XL*XL*XL/V105
        CO215 = DEUX*XL/QUINZE
        CO1140 = XL*XL*XL/V140

C COLONNE 1
        M(IP(1)+1) = MS11*CO13
        M(IP(2)+1) = -MS16/DEUX
        M(IP(3)+1) = MS15/DEUX
        M(IP(4)+1) = ZERO
        M(IP(5)+1) = MS15*CO112
        M(IP(6)+1) = MS16*CO112
        M(IP(7)+1) = M(IP(1)+1)/DEUX
        M(IP(8)+1) = -M(IP(2)+1)
        M(IP(9)+1) = -M(IP(3)+1)
        M(IP(10)+1) = ZERO
        M(IP(11)+1) = -M(IP(5)+1)
        M(IP(12)+1) = -M(IP(6)+1)

C COLONNE 2
        M(IP(2)+2) = CO1335*MS22 + CO65*MS66
        M(IP(3)+2) = -CO65*MS56
        M(IP(4)+2) = CO720*MS24
        M(IP(5)+2) = MS56/DIX
        M(IP(6)+2) = C11210*MS22 + MS66/DIX
        M(IP(7)+2) = M(IP(2)+1)
        M(IP(8)+2) = CO970*MS22 - CO65*MS66
        M(IP(9)+2) = CO65*MS56
        M(IP(10)+2) = CO320*MS24
        M(IP(11)+2) = M(IP(5)+2)
        M(IP(12)+2) = -C13420*MS22 + MS66/DIX
C COLONNE 3
        M(IP(3)+3) = CO1335*MS33 + CO65*MS55
        M(IP(4)+3) = CO720*MS34
        M(IP(5)+3) = -C11210*MS33 - MS55/DIX
        M(IP(6)+3) = -M(IP(5)+2)
        M(IP(7)+3) = M(IP(3)+1)
        M(IP(8)+3) = M(IP(9)+2)
        M(IP(9)+3) = CO970*MS33 - CO65*MS55
        M(IP(10)+3) = CO320*MS34
        M(IP(11)+3) = C13420*MS33 - MS55/DIX
        M(IP(12)+3) = M(IP(6)+3)
C COLONNE 4
        M(IP(4)+4) = CO13*MS44
        M(IP(5)+4) = -CO120*MS34
        M(IP(6)+4) = CO120*MS24
        M(IP(7)+4) = ZERO
        M(IP(8)+4) = CO320*MS24
        M(IP(9)+4) = CO320*MS34
        M(IP(10)+4) = M(IP(4)+4)/DEUX
        M(IP(11)+4) = CO1302*MS34
        M(IP(12)+4) = -CO1302*MS24
C COLONNE 5
        M(IP(5)+5) = CO1105*MS33 + CO215*MS55
        M(IP(6)+5) = CO215*MS56
        M(IP(7)+5) = M(IP(11)+1)
        M(IP(8)+5) = M(IP(6)+3)
        M(IP(9)+5) = -M(IP(11)+3)
        M(IP(10)+5) = -M(IP(11)+4)
        M(IP(11)+5) = -CO1140*MS33 - CO130*MS55
        M(IP(12)+5) = -CO130*MS56
C COLONNE 6
        M(IP(6)+6) = CO1105*MS22 + CO215*MS66
        M(IP(7)+6) = M(IP(12)+1)
        M(IP(8)+6) = -M(IP(12)+2)
        M(IP(9)+6) = -M(IP(12)+3)
        M(IP(10)+6) = -M(IP(12)+4)
        M(IP(11)+6) = M(IP(12)+5)
        M(IP(12)+6) = -CO1140*MS22 - CO130*MS66
C COLONNE 7
        M(IP(7)+7) = M(IP(1)+1)
        M(IP(8)+7) = M(IP(8)+1)
        M(IP(9)+7) = M(IP(9)+1)
        M(IP(10)+7) = ZERO
        M(IP(11)+7) = -M(IP(11)+1)
        M(IP(12)+7) = -M(IP(12)+1)
C COLONNE 8
        M(IP(8)+8) = M(IP(2)+2)
        M(IP(9)+8) = M(IP(3)+2)
        M(IP(10)+8) = M(IP(4)+2)
        M(IP(11)+8) = -M(IP(5)+2)
        M(IP(12)+8) = -M(IP(6)+2)
C COLONNE 9
        M(IP(9)+9) = M(IP(3)+3)
        M(IP(10)+9) = M(IP(4)+3)
        M(IP(11)+9) = -M(IP(5)+3)
        M(IP(12)+9) = -M(IP(6)+3)
C COLONNE 10
        M(IP(10)+10) = M(IP(4)+4)
        M(IP(11)+10) = -M(IP(5)+4)
        M(IP(12)+10) = -M(IP(6)+4)
C COLONNE 11
        M(IP(11)+11) = M(IP(5)+5)
        M(IP(12)+11) = M(IP(6)+5)
C COLONNE 12
        M(IP(12)+12) = M(IP(6)+6)
      END IF

   20 CONTINUE
      END
