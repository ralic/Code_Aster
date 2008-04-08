      SUBROUTINE BOITFI(NOMBO0,NGRMA0,FILTRE,NGRMA,NOMBOI)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      CHARACTER*16 NOMBOI,NOMBO0
      CHARACTER*24 NGRMA0,NGRMA
      REAL*8       FILTRE(2,*)
C
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN ENSEMBLE DE MAILLES
C
C FILTRAGE DES MAILLES SITUEES DANS UNE ZONE RECTANGULAIRE
C
C ----------------------------------------------------------------------
C
C
C IN  NGRMA0 : LISTE DES MAILLES A FILTRER
C IN  NOMBO0 : SD BOITE DES MAILLES A FILTRER
C IN  FILTRE : ZONE DE FILTRATION
C               (1,1): XMIN - (1,2): YMIN - [(1,3): ZMIN]
C               (2,1): XMAX - (2,2): YMAX - [(2,3): ZMAX]
C OUT NGRMA  : LISTE DES MAILLES FILTREES
C OUT NOMBOI : SD BOITE DES MAILLES FILTREES
C
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      LCPAN,LCSOM
      INTEGER      NDIME,NMA,NPAN,NSOM,NINBOX,NELIM
      REAL*8       MN(3),MX(3),R
      REAL*8       MINBOI,MAXBOI,MINMAI,MAXMAI
      INTEGER      IMA,IDIME,INDMA
      INTEGER      IFM,NIV
      INTEGER      A0,A1,A2,A3,A4,A5,A6
      INTEGER      B0,B1,B2,B3,B4,B5,B6
      INTEGER      DD,DP,Q,P0,P1,I,J,K,N
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C --- LECTURE DES DONNEES
C
      CALL JEVEUO(NGRMA0           ,'L',A0)
      CALL JEVEUO(NOMBO0(1:16)//'.DIME'  ,'L',A1)
      CALL JEVEUO(NOMBO0(1:16)//'.MINMAX','L',A2)
      CALL JEVEUO(NOMBO0(1:16)//'.PAN'   ,'L',A3)
      CALL JEVEUO(NOMBO0(1:16)//'.SOMMET','L',A4)
      CALL JEVEUO(NOMBO0(1:16)//'.MMGLOB','L',A5)
      CALL JEVEUO(NOMBO0(1:16)//'.H'     ,'L',A6)
      NDIME = ZI(A1)
      NMA   = ZI(A1+1)
      DD    = 2*NDIME
      DP    = NDIME+2
C
C --- QUELQUES VERIFICATIONS
C
      IF (NMA.LE.0) THEN
        CALL ASSERT(.FALSE.)
      ENDIF
      IF ((NDIME.LT.2).OR.(NDIME.GT.3)) THEN
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- CREATION DE LA SD TEMPORAIRE POUR LE FILTRAGE
C
      CALL WKVECT('&&BOITFI.FILTRE','V V L',NMA,Q)
      DO 10 IMA = 1, NMA
        ZL(Q-1+IMA) = .FALSE.
 10   CONTINUE
C
C --- FILTRAGE DES MAILLES UTILES.
C --- POUR CHAQUE MAILLE: .TRUE. SI MAILLE DANS BOITE .FALSE. SINON
C --- IL Y A <NINBOX> MAILLES DANS LA BOITE
C
      NINBOX = 0
      DO 20 IMA = 1, NMA
        INDMA = NDIME*2*(IMA-1)
        DO 30 IDIME = 1,NDIME
          MINBOI = FILTRE(1,IDIME)
          MAXBOI = FILTRE(2,IDIME)
          MINMAI = ZR(A2+INDMA+2*(IDIME-1))
          MAXMAI = ZR(A2+INDMA+2*(IDIME-1)+1)
          IF ((MINMAI.GT.MAXBOI).OR.
     &        (MAXMAI.LT.MINBOI)) THEN
            GOTO 20
          ENDIF
 30     CONTINUE
        NINBOX = NINBOX + 1
        ZL(Q-1+IMA) = .TRUE.
 20   CONTINUE
C
C --- NOMBRE DE MAILLES ELIMINEES
C
      NELIM = NMA - NINBOX
C
C --- VERIFICATION
C
      IF (NIV.GE.2) THEN
        IF (NELIM.EQ.0) THEN
          WRITE(IFM,*) '<BOITES  > ... TOUTES LES MAILLES SONT '//
     &               'DANS LA ZONE '
        ELSE
          WRITE(IFM,*) '<BOITES  > ... NOMBRE DE MAILLES ELIMINEES '//
     &               'CAR PAS DANS LA ZONE : ',NELIM
        ENDIF
      ENDIF
C
C --- COMPTE NOUVEAUX NOMBRES DE PANS ET DE SOMMETS
C
      LCPAN = 0
      LCSOM = 0
      DO 40 IMA = 1, NMA
        IF (ZL(Q-1+IMA)) THEN
          NPAN = ZI(A1+2+2*(IMA-1)+2) -
     &           ZI(A1+2+2*(IMA-1))
          LCPAN = LCPAN + NPAN
          NSOM = ZI(A1+2+2*(IMA-1)+3) -
     &           ZI(A1+2+2*(IMA-1)+1)
          LCSOM = LCSOM + NSOM
        ENDIF
 40   CONTINUE
C
C --- ALLOCATION NOUVELLE SD GROUPEMA ET BOITE
C
      CALL BOITCR(NOMBOI,'V'    ,NINBOX,NDIME,LCPAN,LCSOM)
      CALL WKVECT(NGRMA ,'V V I',NINBOX,B0)
C
      CALL JEVEUO(NOMBOI(1:16)//'.DIME'  ,'E',B1)
      CALL JEVEUO(NOMBOI(1:16)//'.H'     ,'E',B6)
      CALL JEVEUO(NOMBOI(1:16)//'.MINMAX','E',B2)
      CALL JEVEUO(NOMBOI(1:16)//'.MMGLOB','E',B5)
      CALL JEVEUO(NOMBOI(1:16)//'.PAN'   ,'E',B3)
      CALL JEVEUO(NOMBOI(1:16)//'.SOMMET','E',B4)
C
C --- RECOPIE
C
      LCPAN    = 1
      LCSOM    = 1
      ZI(B1)   = NDIME
      ZI(B1+1) = NINBOX

      DO 50 I = 1, NDIME
        MX(I) = FILTRE(1,I)
        MN(I) = FILTRE(2,I)
 50   CONTINUE

      P0 = A2 - DD
      DO 60 I = 1, NMA

        A1 = A1 + 2
        P0 = P0 + DD
        IF (.NOT.ZL(Q-1+I)) GOTO 60

C ----- NDIME

        B1 = B1 + 2
        ZI(B1) = LCPAN
        ZI(B1+1) = LCSOM

C ----- MINMAX

        P1 = P0
        DO 70 J = 1, NDIME

          R = ZR(P1)
          ZR(B2) = R
          IF (R.LT.MN(J)) MN(J) = R

          R = ZR(P1+1)
          ZR(B2+1) = R
          IF (R.GT.MX(J)) MX(J) = R

          P1 = P1 + 2
          B2 = B2 + 2

 70     CONTINUE

C ----- PAN

        P1 = A3 + DP*(ZI(A1)-1)
        N = ZI(A1+2) - ZI(A1)

        DO 80 J = 1, N

          LCPAN = LCPAN + 1

          DO 80 K = 1, DP

            ZR(B3) = ZR(P1)
            B3 = B3 + 1
            P1 = P1 + 1

 80     CONTINUE

C ----- SOMMET

        P1 = A4 + NDIME*(ZI(A1+1)-1)
        N = ZI(A1+3) - ZI(A1+1)

        DO 90 J = 1, N

          LCSOM = LCSOM + 1

          DO 90 K = 1, NDIME

            ZR(B4) = ZR(P1)
            P1 = P1 + 1
            B4 = B4 + 1

 90     CONTINUE

C ----- GROUPEMA ET H

        ZI(B0) = ZI(A0-1+I)
        ZR(B6) = ZR(A6-1+I)
        B0 = B0 + 1
        B6 = B6 + 1

 60   CONTINUE

      ZI(B1+2) = LCPAN
      ZI(B1+3) = LCSOM

C --- MMGLOB

      DO 100 I = 1, NDIME
        ZR(B5) = MN(I)
        ZR(B5+1) = MX(I)
        B5 = B5 + 2
 100  CONTINUE
C
C --- DESALLOCATION ET MENAGE
C
      CALL JEDETR('&&BOITFI.FILTRE')
      CALL JEDETR(NGRMA0)
      CALL BOITDS(NOMBO0)
C
      CALL JEDEMA()

      END
