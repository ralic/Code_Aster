      SUBROUTINE CESCRE(BASEZ,CESZ,TYPCEZ,MAZ,NOMGDZ,NCMPG,LICMP,NPG,
     &                  NSPT,NCMP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C RESPONSABLE PELLET J.PELLET
C A_UTIL
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
      CHARACTER*(*) MAZ,NOMGDZ,CESZ,BASEZ,TYPCEZ
      INTEGER NPG(*),NSPT(*),NCMP(*)
      CHARACTER*(*) LICMP(*)
C ------------------------------------------------------------------
C BUT : CREER UN CHAM_ELEM_S VIERGE (CESZ)
C ------------------------------------------------------------------
C     ARGUMENTS:
C BASEZ   IN       K1  : BASE DE CREATION POUR CESZ : G/V/L
C CESZ    IN/JXOUT K19 : SD CHAM_ELEM_S A CREER
C TYPCEZ  IN       K4  : TYPE DU CHAM_ELEM_S :
C                        / 'ELNO'
C                        / 'ELGA'
C                        / 'ELEM'
C MAZ     IN/JXIN  K8  : SD MAILLAGE ASSOCIEE A CESZ
C NOMGDZ  IN       K8  : NOM DE LA GRANDEUR DE CESZ
C NCMPG   IN       I   : DIMENSION DE LICMP (CI-DESSOUS)
C            SI NCMPG <= 0  :  ON NE SE SERT PAS DE LICMP
C              SI NOMGDZ /= 'VARI_*' :
C                ON PREND TOUTES LES CMPS DU CATALOGUE
C              SI NOMGDZ = 'VARI_*' :
C                ON PREND LES CMPS V1,V2,...,'V'//CHAR(-NCMPG)


C LICMP   IN       L_K8: NOMS DES CMPS VOULUES DANS CESZ
C                        SI NOMGD='VARI_*' :
C                        LES CMPS DOIVENT AVOIR LA FORME : 'V1','V2',...

C SI TYPCEZ = 'ELGA' (SINON NPG EST INUTILISE):
C NPG     IN       V(I) : NOMBRES DE POINTS DE GAUSS POUR LES MAILLES.
C    / NPG(1)<0 : LE TABLEAU NPG EST ALORS DE DIMENSION 1
C                 ET -NPG EST LE NOMBRE DE POINTS DE GAUSS POUR TOUTES
C                 LES MAILLES DU MAILLAGE.
C    / NPG(1)>=0 : LE TABLEAU NPG EST DE DIMENSION NB_MAILLES(MAZ)
C                 NPG(IMA) EST LE NOMBRE DE POINTS VOULUS POUR LA
C                 MAILLE IMA

C NSPT    IN       V(I) : NOMBRES DE SOUS-POINTS POUR LES MAILLES.
C    / NSPT(1)<0 : LE TABLEAU NSPT EST ALORS DE DIMENSION 1
C                 ET -NSPT EST LE NOMBRE DE SOUS-POINTS POUR TOUTES
C                 LES MAILLES DU MAILLAGE.
C    / NSPT(1)>=0 : LE TABLEAU NSPT EST DE DIMENSION NB_MAILLES(MAZ)
C                 NSPT(IMA) EST LE NOMBRE DE SOUS-POINTS VOULUS POUR LA
C                 MAILLE IMA

C NCMP    IN       V(I) : NOMBRES DE CMPS POUR LES MAILLES.
C    / NCMP(1)<0 : LE TABLEAU NCMP EST ALORS DE DIMENSION 1
C                 ET -NCMP EST LE NOMBRE DE CMPS POUR TOUTES
C                 LES MAILLES DU MAILLAGE.
C    / NCMP(1)>=0 : LE TABLEAU NCMP EST DE DIMENSION NB_MAILLES(MAZ)
C                 NCMP(IMA) EST LE NOMBRE DE CMPS VOULUES POUR LA
C                 MAILLE IMA

C     ------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      CHARACTER*1 KBID,BASE
      CHARACTER*3 TSCA
      CHARACTER*4 TYPCES
      CHARACTER*8 MA,NOMGD,NOMCMP
      CHARACTER*19 CES
      CHARACTER*24 VALK(2)
      INTEGER GD,NCMPMX,IBID,NBMA,JCMPGD,ICMP,JCMP,JCESK,JCESD
      INTEGER INDIK8,JCESC,K,JCESL,JCESV,NCMPG,IMA,JLCONX,DECAL
      INTEGER NPTMA,NBNOMA,NSPTMA,NCMPMA,NCMP2,JLICMP,IRET

C     FONCTION FORMULE:
C     NBNOMA(IMA)=NOMBRE DE NOEUDS DE LA MAILLE IMA
      NBNOMA(IMA) = ZI(JLCONX-1+IMA+1) - ZI(JLCONX-1+IMA)
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CES = CESZ
      BASE = BASEZ
      NOMGD = NOMGDZ
      MA = MAZ

      CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMA,KBID,IBID)
      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)

C     -- SI CES EXISTE DEJA, ON LE DETRUIT :
      CALL DETRSD('CHAM_ELEM_S',CES)


C------------------------------------------------------------------
C     1- QUELQUES VERIFS (+ RECUPERATION DE JLCONX):
C     ----------------------------------------------

      TYPCES = TYPCEZ
      IF (TYPCES.EQ.'ELEM') THEN
      ELSE IF (TYPCES.EQ.'ELGA') THEN
      ELSE IF (TYPCES.EQ.'ELNO') THEN
        CALL JEVEUO(JEXATR(MA//'.CONNEX','LONCUM'),'L',JLCONX)
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',NOMGD),GD)
      IF (GD.EQ.0) CALL U2MESK('F','CALCULEL_67',1,NOMGD)

      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',JCMPGD)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,KBID)



C     -- ON CALCULE ET ON VERIFIE :  '&&CESCRE.LICMP' :
C     --------------------------------------------------
      IF (NCMPG.EQ.0) THEN
        CALL ASSERT(NOMGD(1:5).NE.'VARI_')
        NCMP2 = NCMPMX
        CALL WKVECT('&&CESCRE.LICMP','V V K8',NCMP2,JLICMP)
        DO 10,K = 1,NCMP2
          ZK8(JLICMP-1+K) = ZK8(JCMPGD-1+K)
   10   CONTINUE

      ELSE IF (NCMPG.GT.0) THEN
        CALL VERIGD(NOMGD,LICMP,NCMPG,IRET)
        CALL ASSERT(IRET.LE.0)

        NCMP2 = NCMPG
        CALL WKVECT('&&CESCRE.LICMP','V V K8',NCMP2,JLICMP)
        DO 20,K = 1,NCMP2
          ZK8(JLICMP-1+K) = LICMP(K)
   20   CONTINUE

      ELSE IF (NCMPG.LT.0) THEN
        CALL ASSERT(NOMGD(1:5).EQ.'VARI_')
        NCMP2 = -NCMPG
        CALL WKVECT('&&CESCRE.LICMP','V V K8',NCMP2,JLICMP)
        NOMCMP(1:1) = 'V'
        DO 30,K = 1,NCMP2
          CALL CODENT(K,'G',NOMCMP(2:8))
          ZK8(JLICMP-1+K) = NOMCMP
   30   CONTINUE
      END IF

      DO 40,ICMP = 1,NCMP2
        IF (NOMGD(1:5).NE.'VARI_') THEN
          JCMP = INDIK8(ZK8(JCMPGD),ZK8(JLICMP-1+ICMP),1,NCMPMX)
        ELSE
          IF (ZK8(JLICMP-1+ICMP) (1:1).NE.'V') THEN
            JCMP = 0
          ELSE
            JCMP = 1
          END IF
        END IF
        IF (JCMP.EQ.0) THEN
          VALK(1) = ZK8(JLICMP-1+ICMP)
          VALK(2) = NOMGD
          CALL U2MESK('F','CALCULEL_52', 2 ,VALK)
        ENDIF
   40 CONTINUE


C------------------------------------------------------------------
C     2- CREATION DE CES.CESC :
C     -------------------------------------------
      CALL WKVECT(CES//'.CESC',BASE//' V K8',NCMP2,JCESC)
      DO 50,K = 1,NCMP2
        ZK8(JCESC-1+K) = ZK8(JLICMP-1+K)
   50 CONTINUE

C------------------------------------------------------------------
C     3- CREATION DE CES.CESK:
C     ------------------------
      CALL WKVECT(CES//'.CESK',BASE//' V K8',3,JCESK)
      ZK8(JCESK-1+1) = MA
      ZK8(JCESK-1+2) = NOMGD
      ZK8(JCESK-1+3) = TYPCES

C------------------------------------------------------------------
C     4- CREATION DE CES.CESD:
C     ------------------------
      CALL WKVECT(CES//'.CESD',BASE//' V I',5+4*NBMA,JCESD)
      ZI(JCESD-1+1) = NBMA
      ZI(JCESD-1+2) = NCMP2
      ZI(JCESD-1+3) = 0
      ZI(JCESD-1+4) = 0
      ZI(JCESD-1+5) = 0
      DECAL = 0
      DO 60,IMA = 1,NBMA

C       -- CALCUL DE NPT(IMA):
        IF (TYPCES.EQ.'ELEM') THEN
          NPTMA = 1
        ELSE IF (TYPCES.EQ.'ELNO') THEN
          NPTMA = NBNOMA(IMA)
        ELSE IF (TYPCES.EQ.'ELGA') THEN
          IF (NPG(1).LT.0) THEN
            NPTMA = -NPG(1)
          ELSE
            NPTMA = NPG(IMA)
          END IF
        END IF

C       -- CALCUL DE NSPT(IMA):
        IF (NSPT(1).LT.0) THEN
          NSPTMA = -NSPT(1)
        ELSE
          NSPTMA = NSPT(IMA)
        END IF

C       -- CALCUL DE NCMP(IMA):
        IF (NCMP(1).LT.0) THEN
          NCMPMA = -NCMP(1)
        ELSE
          NCMPMA = NCMP(IMA)
        END IF

        ZI(JCESD-1+5+4* (IMA-1)+1) = NPTMA
        ZI(JCESD-1+5+4* (IMA-1)+2) = NSPTMA
        ZI(JCESD-1+5+4* (IMA-1)+3) = NCMPMA
        ZI(JCESD-1+5+4* (IMA-1)+4) = DECAL

        DECAL = DECAL + NPTMA*NSPTMA*NCMPMA

        ZI(JCESD-1+3) = MAX(NPTMA,ZI(JCESD-1+3))
        ZI(JCESD-1+4) = MAX(NSPTMA,ZI(JCESD-1+4))
        ZI(JCESD-1+5) = MAX(NCMPMA,ZI(JCESD-1+5))
   60 CONTINUE

C     -- POUR POUVOIR CONTINUER SI DECAL=0 (CES VIDE):
      DECAL=MAX(DECAL,1)

C------------------------------------------------------------------
C     5- CREATION DE CES.CESL:
C     ------------------------
      CALL WKVECT(CES//'.CESL',BASE//' V L',DECAL,JCESL)
      CALL JEUNDF(CES//'.CESL')

C------------------------------------------------------------------
C     6- CREATION DE CES.CESV:
C     ------------------------
      CALL WKVECT(CES//'.CESV',BASE//' V '//TSCA,DECAL,JCESV)
      CALL JEUNDF(CES//'.CESV')


C------------------------------------------------------------------
C     7- MENAGE :
C     ------------------------
      CALL JEDETR('&&CESCRE.LICMP')

      CALL JEDEMA()
      END
