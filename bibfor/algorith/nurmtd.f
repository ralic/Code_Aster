      SUBROUTINE NURMTD(NDIM,NNO1,NNO2,NPG,IW,VFF1,VFF2,IVF1,
     &                  IDFF1,VU,VP,TYPMOD,IGEOM,MATE,MINI,MATR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/02/2013   AUTEUR SFAYOLLE S.FAYOLLE 
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

      LOGICAL      MINI
      INTEGER      NDIM,NNO1,NNO2,NPG,IW,IDFF1
      INTEGER      MATE
      INTEGER      VU(3,27),VP(27)
      INTEGER      IVF1,IGEOM
      REAL*8       VFF1(NNO1,NPG),VFF2(NNO2,NPG)
      CHARACTER*8  TYPMOD(*)
      REAL*8       MATR(*)

C-----------------------------------------------------------------------
C          CALCUL DES FORCES NODALES POUR LES ELEMENTS
C          INCOMPRESSIBLES POUR LES PETITES DEFORMATIONS
C          3D/D_PLAN/AXIS
C          ROUTINE APPELEE PAR TE0596
C-----------------------------------------------------------------------
C IN  MINI    : STABILISATION BULLE - MINI ELEMENT
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
C IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  IW      : POIDS DES POINTS DE GAUSS
C IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
C IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
C IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
C IN  VP      : TABLEAU DES INDICES DES DDL DE PRESSION
C IN  GEOMI   : COORDONEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  MATE    : MATERIAU CODE
C OUT MATR    : MATRICE DE RIGIDITE
C-----------------------------------------------------------------------

      LOGICAL      AXI
      INTEGER      G
      INTEGER      IA,NA,SA,IB,NB,SB,JA,JB
      INTEGER      OS,KK
      INTEGER      VUIANA,VPSA
      INTEGER      IDIM
      INTEGER      IDECPG,IDECNO
      REAL*8       RAC2
      REAL*8       R,W,DFF1(NNO1,NDIM)
      REAL*8       DSIDEP(2*NDIM,2*NDIM)
      REAL*8       DEF(2*NDIM,NNO1,NDIM),DEFTR(NNO1,NDIM)
      REAL*8       DDEV(2*NDIM,2*NDIM),DEVD(2*NDIM,2*NDIM)
      REAL*8       DDDEV(2*NDIM,2*NDIM)
      REAL*8       XYZGAU(3),BARY(3),REPERE(7)
      REAL*8       T1
      REAL*8       IDEV(6,6),IDEV2(4,4)
      REAL*8       ALPHA,TREPST
      REAL*8       PRESM(NNO2),PRESD(NNO2)
      REAL*8       KBB(NDIM,NDIM),KBP(NDIM,NNO2)
      REAL*8       KCE(NNO2,NNO2),RCE(NNO2)
      REAL*8       FM(3,3)
      CHARACTER*16 COMPOR,OPTION

      DATA         FM   / 1.D0, 0.D0, 0.D0,
     &                    0.D0, 1.D0, 0.D0,
     &                    0.D0, 0.D0, 1.D0/
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

C - INITIALISATION
      AXI  = TYPMOD(1).EQ.'AXIS'
      RAC2  = SQRT(2.D0)
      OPTION = 'RIGI_MECA       '
      COMPOR = 'ELAS            '

      CALL R8INIR(NNO2, 0.D0, PRESM, 1)
      CALL R8INIR(NNO2, 0.D0, PRESD, 1)

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
        CALL DFDMIP(NDIM,NNO1,AXI,ZR(IGEOM),G,IW,VFF1(1,G),IDFF1,R,W,
     &              DFF1)

C - CALCUL DE LA MATRICE B EPS_ij=B_ijkl U_kl
        IF (NDIM.EQ.2) THEN
          DO 35 NA=1,NNO1
            DO 45 IA=1,NDIM
             DEF(1,NA,IA)= FM(IA,1)*DFF1(NA,1)
             DEF(2,NA,IA)= FM(IA,2)*DFF1(NA,2)
             DEF(3,NA,IA)= 0.D0
             DEF(4,NA,IA)=(FM(IA,1)*DFF1(NA,2)+FM(IA,2)*DFF1(NA,1))/RAC2
 45         CONTINUE
 35       CONTINUE
        ELSE
          DO 36 NA=1,NNO1
            DO 46 IA=1,NDIM
             DEF(1,NA,IA)= FM(IA,1)*DFF1(NA,1)
             DEF(2,NA,IA)= FM(IA,2)*DFF1(NA,2)
             DEF(3,NA,IA)= FM(IA,3)*DFF1(NA,3)
             DEF(4,NA,IA)=(FM(IA,1)*DFF1(NA,2)+FM(IA,2)*DFF1(NA,1))/RAC2
             DEF(5,NA,IA)=(FM(IA,1)*DFF1(NA,3)+FM(IA,3)*DFF1(NA,1))/RAC2
             DEF(6,NA,IA)=(FM(IA,2)*DFF1(NA,3)+FM(IA,3)*DFF1(NA,2))/RAC2
 46         CONTINUE
 36       CONTINUE
        ENDIF

C - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
        IF (AXI) THEN
          DO 47 NA=1,NNO1
            DEF(3,NA,1) = FM(3,3)*VFF1(NA,G)/R
 47       CONTINUE
        END IF

C - CALCUL DE TRACE(B)
        DO 50 NA = 1,NNO1
          DO 49 IA = 1,NDIM
            DEFTR(NA,IA) =  DEF(1,NA,IA) + DEF(2,NA,IA) + DEF(3,NA,IA)
 49       CONTINUE
 50     CONTINUE

C - CALCUL DE LA MATRICE D'ELASTICITE BULLE
        CALL TANBUL(OPTION,NDIM,G,MATE,COMPOR,.FALSE.,.TRUE.,ALPHA,
     &              DSIDEP,TREPST)

C - CALCUL DE LA MATRICE DE CONDENSATION STATIQUE
        IF (MINI) THEN
          CALL CALKBB(NNO1,NDIM,W,DEF,DSIDEP,KBB)
          CALL CALKBP(NNO2,NDIM,W,DFF1,G,VFF2,NPG,KBP)
          CALL CALKCE(NNO1,NDIM,KBP,KBB,PRESM,PRESD,KCE,RCE)
        ELSE
          CALL R8INIR(NNO2*NNO2,0.D0,KCE,1)
        ENDIF

        IF(NDIM .EQ. 3)THEN
          CALL PMAT(6,IDEV/3.D0,DSIDEP,DEVD)
          CALL PMAT(6,DSIDEP,IDEV/3.D0,DDEV)
          CALL PMAT(6,DEVD,IDEV/3.D0,DDDEV)
        ELSE
          CALL PMAT(4,IDEV2/3.D0,DSIDEP,DEVD)
          CALL PMAT(4,DSIDEP,IDEV2/3.D0,DDEV)
          CALL PMAT(4,DEVD,IDEV2/3.D0,DDDEV)
        ENDIF

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

C - TERME K:UP      KUP(NDIM,NNO1,NNO2)
            DO 490 SB = 1,NNO2
              IF(VP(SB).LT.VUIANA)THEN
              KK = OS + VP(SB)
              T1 = DEFTR(NA,IA)*VFF2(SB,G)
              MATR(KK) = MATR(KK) + W*T1
              ENDIF
 490        CONTINUE
 410      CONTINUE
 400    CONTINUE

C - TERME K:PX
        DO 600 SA = 1,NNO2
          VPSA = VP(SA)
          OS = (VPSA-1)*VPSA/2

C - TERME K:PU      KPU(NDIM,NNO2,NNO1)
          DO 610 NB = 1,NNO1
            DO 620 IB = 1,NDIM
              IF(VU(IB,NB).LT.VPSA)THEN
              KK = OS + VU(IB,NB)
              T1 = VFF2(SA,G)*DEFTR(NB,IB)
              MATR(KK) = MATR(KK) + W*T1
              ENDIF
 620        CONTINUE
 610      CONTINUE

C - TERME K:PP      KPP(NNO2,NNO2)
          DO 640 SB = 1,NNO2
            IF(VP(SB).LE.VPSA)THEN
            KK = OS + VP(SB)
            T1 = - VFF2(SA,G)*VFF2(SB,G)*ALPHA
            MATR(KK) = MATR(KK) + W*T1 - KCE(SA,SB)
            ENDIF
 640       CONTINUE
 600    CONTINUE
 1000 CONTINUE
      END
