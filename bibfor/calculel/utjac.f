      SUBROUTINE UTJAC ( L2D, GEOM, IPG, IDFDE, NIV, IFM, NNO, JACOB )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 31/01/2012   AUTEUR REZETTE C.REZETTE 
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
C RESPONSABLE  BOITEAU O.BOITEAU
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL LE JACOBIEN D'UN ELEMENT FINI K
C                          POUR AERER TE0003
C
C IN L2D    : FLAG INDICATEUR DU 2D
C IN GEOM   : LA GEOMETRIE
C IN IDFDE/DK/DN  : ADRESSE JEVEUX DES DERIVEES DES FONCTIONS DE FORME
C IN NIV    : NIVEAU D'IMPRESSION
C IN IFM    : UNITE LOGIQUE D'IMPRESSION
C IN NNO    : NOMBRE DE NOEUDS
C OUT JACOB : SIGNE DU JACOBIEN
C   -------------------------------------------------------------------
C     FONCTIONS INTRINSEQUES:
C       SIGN.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       18/09/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER IPG,IDFDE,NIV,IFM,NNO,IA1,IA2
      REAL*8  JACOB,GEOM(*)
      LOGICAL L2D

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      REAL*8 VALR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION VARIABLES LOCALES
      INTEGER I,I1,J, KP, IDFDK, IDFDN
      INTEGER VALI
      REAL*8  DXDE,DXDK,DYDE,DYDK,XP,YP,DFRDE,DFRDK,DFRDN,G(3,3),J11,
     &        J21,J31

C INIT
      IF (L2D) THEN
        IDFDK = IDFDE + 1
      ELSE 
        IDFDN = IDFDE + 1
        IDFDK = IDFDN + 1
      ENDIF

      IF (L2D) THEN
C CAS 2D
        KP = 2*(IPG-1)*NNO
        DXDE=0.D0
        DXDK=0.D0
        DYDE=0.D0
        DYDK=0.D0
        DO 100 I=1,NNO
          I1 = 2*(I-1)+1
          XP = GEOM(I1)
          YP = GEOM(I1+1)
          DFRDE = ZR(IDFDE+KP+I1-1)
          DFRDK = ZR(IDFDK+KP+I1-1)
          DXDE = DXDE+XP*DFRDE
          DXDK = DXDK+XP*DFRDK
          DYDE = DYDE+YP*DFRDE
          DYDK = DYDK+YP*DFRDK
  100   CONTINUE
        JACOB=DXDE*DYDK-DXDK*DYDE

      ELSE
C CAS 3D

        KP = 3*(IPG-1)*NNO
        CALL MATINI(3,3,0.D0,G)
        DO 140 I=1,NNO
          I1 = 3*(I-1)
          DFRDE = ZR(IDFDE+KP+I1)
          DFRDK = ZR(IDFDK+KP+I1)
          DFRDN = ZR(IDFDN+KP+I1)
          DO 130 J=1,3
            XP = GEOM(I1+J)
            G(1,J) = G(1,J) + XP * DFRDE
            G(2,J) = G(2,J) + XP * DFRDN
            G(3,J) = G(3,J) + XP * DFRDK
  130     CONTINUE
  140   CONTINUE
        J11 = G(2,2) * G(3,3) - G(2,3) * G(3,2)
        J21 = G(3,1) * G(2,3) - G(2,1) * G(3,3)
        J31 = G(2,1) * G(3,2) - G(3,1) * G(2,2)
        JACOB = G(1,1)*J11 + G(1,2)*J21 + G(1,3)*J31

      ENDIF

C EN 2D ON NE TESTE PAS LE SIGNE DU JACOBIEN
      IF (.NOT.L2D.AND.(JACOB.LT.0.D0)) THEN
        CALL U2MESG('A+','CALCULEL6_73',0,' ',0,0,0,0.D0)
        CALL TECAEL(IA1,IA2)
        VALI = ZI(IA1)
        VALR = JACOB
        CALL U2MESG('A','CALCULEL6_74',0,' ',1,VALI,1,VALR)
      ENDIF

C CALCUL DU SIGNE DU JACOBIEN + AFFICHAGE SI NECESSAIRE
      JACOB = SIGN(1.D0,JACOB)
      IF (NIV.EQ.2)  WRITE(IFM,*)'ORIENTATION MAILLE ',JACOB

      END
