      SUBROUTINE NIRMTD(NDIM,NNO1,NNO2,NNO3,NPG,IW,VFF1,VFF2,VFF3,IVF1,
     &                  IDFF1,VU,VG,VP,IGEOM,MATE,MATR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/02/2013   AUTEUR SFAYOLLE S.FAYOLLE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE SFAYOLLE S.FAYOLLE
C TOLE CRS_1404
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      INTEGER      NDIM,NNO1,NNO2,NNO3,NPG,IW,IDFF1
      INTEGER      MATE
      INTEGER      VU(3,27),VG(27),VP(27)
      INTEGER      IVF1,IGEOM
      REAL*8       VFF1(NNO1,NPG),VFF2(NNO2,NPG),VFF3(NNO3,NPG)
      REAL*8       MATR(*)

C-----------------------------------------------------------------------
C          CALCUL DE LA RIGIDITE MECANIQUE POUR LES ELEMENTS
C          INCOMPRESSIBLES POUR LES GRANDES DEFORMATIONS
C          3D/D_PLAN/AXIS
C          ROUTINE APPELEE PAR TE0592
C-----------------------------------------------------------------------
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
C IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AU GONFLEMENT
C IN  NNO3    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  IW      : POIDS DES POINTS DE GAUSS
C IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
C IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES AU GONFLEMENT
C IN  VFF3    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
C IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
C IN  VG      : TABLEAU DES INDICES DES DDL DE GONFLEMENT
C IN  VP      : TABLEAU DES INDICES DES DDL DE PRESSION
C IN  IGEOM   : POINTEUR SUR LES COORDONEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  MATE    : MATERIAU CODE
C OUT MATR    : MATRICE DE RIGIDITE
C-----------------------------------------------------------------------

      INTEGER      G
      INTEGER      IA,NA,RA,SA,IB,NB,RB,SB,JA,JB
      INTEGER      OS,KK
      INTEGER      VUIANA,VGRA,VPSA
      INTEGER      NBSIGM,NBSIG,IDIM
      INTEGER      IDECPG,IDECNO
      REAL*8       W
      REAL*8       DSIDEP(2*NDIM,2*NDIM)
      REAL*8       B(2*NDIM,81),DEF(2*NDIM,NNO1,NDIM),DEFTR(NNO1,NDIM)
      REAL*8       DDEV(2*NDIM,2*NDIM),DEVD(2*NDIM,2*NDIM)
      REAL*8       DDDEV(2*NDIM,2*NDIM)
      REAL*8       IDDID,DEVDI(2*NDIM),IDDEV(2*NDIM)
      REAL*8       XYZGAU(3),BARY(3),REPERE(7)
      REAL*8       T1
      REAL*8       IDEV(6,6),IDEV2(4,4),KR(6)

      DATA         KR   / 1.D0, 1.D0, 1.D0, 0.D0, 0.D0, 0.D0/
      DATA         IDEV2/ 2.D0,-1.D0,-1.D0, 0.D0,
     &                   -1.D0, 2.D0,-1.D0, 0.D0,
     &                   -1.D0,-1.D0, 2.D0, 0.D0,
     &                    0.D0, 0.D0, 0.D0, 3.D0/
      DATA         IDEV / 2.D0,-1.D0,-1.D0, 0.D0, 0.D0, 0.D0,
     &                   -1.D0, 2.D0,-1.D0, 0.D0, 0.D0, 0.D0,
     &                   -1.D0,-1.D0, 2.D0, 0.D0, 0.D0, 0.D0,
     &                    0.D0, 0.D0, 0.D0, 3.D0, 0.D0, 0.D0,
     &                    0.D0, 0.D0, 0.D0, 0.D0, 3.D0, 0.D0,
     &                    0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 3.D0/
C-----------------------------------------------------------------------

C - NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
      NBSIG = NBSIGM()

C - RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE
C - COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
      BARY(1) = 0.D0
      BARY(2) = 0.D0
      BARY(3) = 0.D0
      DO 150 IA = 1,NNO1
        DO 40 IDIM = 1,NDIM
          BARY(IDIM) = BARY(IDIM)+ZR(IGEOM+IDIM+NDIM*(IA-1)-1)/NNO1
 40    CONTINUE
 150  CONTINUE
      CALL ORTREP(MATE,NDIM,BARY,REPERE)

C - CALCUL POUR CHAQUE POINT DE GAUSS
      DO 1000 G = 1,NPG
        IDECPG = NNO1* (G-1) - 1

C - COORDONNEES AU POINT D'INTEGRATION COURANT
        XYZGAU(1) = 0.D0
        XYZGAU(2) = 0.D0
        XYZGAU(3) = 0.D0
        IF(NDIM .EQ. 3)THEN
          DO 31 IA = 1,NNO1
            IDECNO = 3* (IA-1) - 1
            XYZGAU(1) = XYZGAU(1)+ZR(IVF1+IA+IDECPG)*ZR(IGEOM+1+IDECNO)
            XYZGAU(2) = XYZGAU(2)+ZR(IVF1+IA+IDECPG)*ZR(IGEOM+2+IDECNO)
            XYZGAU(3) = XYZGAU(3)+ZR(IVF1+IA+IDECPG)*ZR(IGEOM+3+IDECNO)
 31       CONTINUE
        ENDIF

C - CALCUL DES ELEMENTS GEOMETRIQUES
C - CALCUL DE DFDI,F,EPS,R(EN AXI) ET POIDS
        CALL BMATMC(G,NBSIG,ZR(IGEOM),IW,IVF1,IDFF1,NNO1,0.D0,W,B)

        DO 10 IA = 1, 2*NDIM
          DO 20 JA = 1, NNO1
            DO 30 NA = 1, NDIM
              DEF(IA,JA,NA) = B(IA,(JA-1)*NDIM+NA)
  30        CONTINUE
  20      CONTINUE
  10    CONTINUE

C - CALCUL DE TRACE(B)
        DO 50 NA = 1,NNO1
          DO 49 IA = 1,NDIM
            DEFTR(NA,IA) =  DEF(1,NA,IA) + DEF(2,NA,IA) + DEF(3,NA,IA)
 49       CONTINUE
 50     CONTINUE

C - CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
C - ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
        CALL DMATMC('RIGI','  ',MATE,0.D0,'+',G,1,REPERE,XYZGAU,
     &              NBSIG,DSIDEP)

        IF(NDIM .EQ. 3)THEN
          CALL PMAT(6,IDEV/3.D0,DSIDEP,DEVD)
          CALL PMAT(6,DSIDEP,IDEV/3.D0,DDEV)
          CALL PMAT(6,DEVD,IDEV/3.D0,DDDEV)
        ELSE
          CALL PMAT(4,IDEV2/3.D0,DSIDEP,DEVD)
          CALL PMAT(4,DSIDEP,IDEV2/3.D0,DDEV)
          CALL PMAT(4,DEVD,IDEV2/3.D0,DDDEV)
        ENDIF

C - CALCUL DE D^DEV:ID ET ID:D^DEV ET ID:D:ID/9.D0
        IDDID = 0.D0
        DO 380 IA = 1,2*NDIM
          DEVDI(IA) = DEVD(IA,1)+DEVD(IA,2)+DEVD(IA,3)
          IDDEV(IA) = DDEV(1,IA)+DDEV(2,IA)+DDEV(3,IA)
          DO 390 JA = 1,3
            IDDID = IDDID+KR(IA)*DSIDEP(IA,JA)
 390      CONTINUE
 380    CONTINUE
        IDDID = IDDID/9.D0

C - CALCUL DE LA MATRICE DE RIGIDITE
C - TERME K:UX
        DO 400 NA = 1,NNO1
          DO 410 IA = 1,NDIM
            VUIANA = VU(IA,NA)
            OS = (VUIANA-1)*VUIANA/2

C - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
            DO 420 NB = 1,NNO1
              DO 430 IB = 1,NDIM
                IF(VU(IB,NB).LE.VUIANA)THEN
                KK = OS+VU(IB,NB)
                T1 = 0.D0
                DO 440 JA = 1,2*NDIM
                  DO 450 JB = 1,2*NDIM
                  T1 = T1 + DEF(JA,NA,IA)*DDDEV(JA,JB)*DEF(JB,NB,IB)
 450              CONTINUE
 440            CONTINUE
                MATR(KK) = MATR(KK) + W*T1
                ENDIF
 430          CONTINUE
 420        CONTINUE

C - TERME K:UG      KUG(NDIM,NNO1,NNO2)
            T1 = 0.D0
            DO 470 JA = 1,2*NDIM
              T1 = T1 + DEF(JA,NA,IA)*DEVDI(JA)
 470        CONTINUE
            T1 = T1/3.D0

            DO 480 RB = 1,NNO2
              IF(VG(RB).LT.VUIANA)THEN
              KK = OS + VG(RB)
              MATR(KK) = MATR(KK) + W*T1*VFF2(RB,G)
              ENDIF
 480        CONTINUE

C - TERME K:UP      KUP(NDIM,NNO1,NNO3)
            DO 490 SB = 1,NNO3
              IF(VP(SB).LT.VUIANA)THEN
              KK = OS + VP(SB)
              T1 = DEFTR(NA,IA)*VFF3(SB,G)
              MATR(KK) = MATR(KK) + W*T1
              ENDIF
 490        CONTINUE
 410      CONTINUE
 400    CONTINUE

C - TERME K:GX
        DO 500 RA = 1,NNO2
          VGRA = VG(RA)
          OS = (VGRA-1)*VGRA/2

C - TERME K:GU      KGU(NDIM,NNO2,NNO1)
          DO 510 NB = 1,NNO1
            DO 520 IB = 1,NDIM
              IF(VU(IB,NB).LT.VGRA)THEN
              KK = OS + VU(IB,NB)
              T1 = 0.D0
              DO 530 JB = 1,2*NDIM
                T1 = T1 + IDDEV(JB)*DEF(JB,NB,IB)
 530          CONTINUE
              MATR(KK) = MATR(KK) + W*T1*VFF2(RA,G)/3.D0
              ENDIF
 520        CONTINUE
 510      CONTINUE

C - TERME K:GG      KGG(NNO2,NNO2)
          DO 540 RB = 1,NNO2
            IF(VG(RB).LE.VGRA)THEN
            KK = OS + VG(RB)
            T1 = VFF2(RA,G)*IDDID*VFF2(RB,G)
            MATR(KK) = MATR(KK) + W*T1
            ENDIF
 540      CONTINUE

C - TERME K:GP      KGP(NNO2,NNO3)
          DO 550 SB = 1,NNO3
            IF(VP(SB).LT.VGRA)THEN
            KK = OS + VP(SB)
            T1 = - VFF2(RA,G)*VFF3(SB,G)
            MATR(KK) = MATR(KK) + W*T1
            ENDIF
 550      CONTINUE
 500    CONTINUE

C - TERME K:PX
        DO 600 SA = 1,NNO3
          VPSA = VP(SA)
          OS = (VPSA-1)*VPSA/2

C - TERME K:PU      KPU(NDIM,NNO3,NNO1)
            DO 610 NB = 1,NNO1
              DO 620 IB = 1,NDIM
                IF(VU(IB,NB).LT.VPSA)THEN
                KK = OS + VU(IB,NB)
                T1 = VFF3(SA,G)*DEFTR(NB,IB)
                MATR(KK) = MATR(KK) + W*T1
                ENDIF
 620          CONTINUE
 610        CONTINUE

C - TERME K:PG      KPG(NNO3,NNO2)
          DO 630 RB = 1,NNO2
            IF(VG(RB).LT.VPSA)THEN
            KK = OS + VG(RB)
            T1 = - VFF3(SA,G)*VFF2(RB,G)
            MATR(KK) = MATR(KK) + W*T1
            ENDIF
 630      CONTINUE

C - TERME K:PP = 0.D0      KPP(NNO3,NNO3)
 600    CONTINUE
 1000 CONTINUE
      END
