      SUBROUTINE CALVCI(NOMCI,NOMNU,NBCHCI,LCHCI,INST,BASE)
      IMPLICIT NONE
C RESPONSABLE VABHHTS J.PELLET
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/10/2007   AUTEUR PELLET J.PELLET 
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
      CHARACTER*(*) NOMCI,LCHCI(*),NOMNU
      CHARACTER*1 BASE
      REAL*8 INST
      INTEGER NBCHCI
C ----------------------------------------------------------------------
C BUT  :  CALCUL DU CHAM_NO CONTENANT UN VECTEUR LE CINEMATIQUE
C ---     ASSOCIE A UNE LISTE DE CHAR_CINE_* A UN INSTANT INST
C         CHAR_CINE ET AYANT COMME PROF_CHNO CELUI DE NOMNU
C                              ---
C                              ! 0. SI I DDLS NON IMPOSE DANS
C         NOMCI(1:19).VALE(I) =!       LA LISTE DES CHAR_CINE
C                              ! U0(NI,INST) SINON
C                              ---
C             OU NI EST LE NUMERO DANS LE MAILLAGE DU NOEUD
C                   SUPPORTANT LE DDL NUMERO I DANS LA NUMEROTATION
C          U0(NI,INST)= VALEUR DU CHARGEMENT ASSOCIE A LA
C                       DERNIERE CHAR_CINE IMPOSANT I
C ----------------------------------------------------------------------
C IN/JXVAR  K*19 NOMCI  : NOM DU CHAM_NO CREE A PARTIR DE LA LISTE DE
C                   CHAR_CINE ET AYANT COMME PROF_CHNO CELUI DE NOMNU
C IN  K*14 NOMNU  : NOM DE LA NUMEROTATION SUPPORTANT LE CHAM_NO
C IN  I    NBCHCI : NOMBRE DE CHAR_CINE DE LA LISTE LCHCI
C IN  K*24 LCHCI  : LISTE DES NOMS DES CHARGES CINEMATIQUES ENTRANT
C                   DANS LE CALCUL DU CHAM_NO NOMCI
C IN  R*8  INST   : INSTANT
C IN  K*1  BASE   : BASE SUR LAQUELLE ON CREE LE CHAM_NO
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
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

C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      INTEGER IDDES,NEC,IVVALE,JNUEQ,JPRNO,ICOOR,ICHCIN,JAFCI
      INTEGER JAFCV,NBIMP,NIMP,N,NI,NDDL,NN,NUEQ,IPROL,IER
      INTEGER NEQ,IBID,NBNO,NUMGD,IDTYP,JDLCI,JAFCK
      INTEGER JCN1K,JCN1D,JCN1C,JCN1V,JCN1L,ICMP,ICMP1,INO,JNOCMP
      INTEGER NBCMP1,INDIK8,JDEEQ,IMAILL,VALI(1)
      CHARACTER*1 TYPVAL
      CHARACTER*4 PHEN
      LOGICAL FONC
      REAL*8 VALP(4),RES,VALR(1)
      CHARACTER*8 NOMMA,KBID,GD,NOMF,EVOIM,NOCMP,NOMCH
      CHARACTER*14 NU
      CHARACTER*16 NOMP(4)
      CHARACTER*19 VCINE,CHARCI,PRCHNO,CNOIMP,CNSIMP
      CHARACTER*24 VVALE,VALK(4)
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
C----------------------------------------------------------------------
      CALL JEMARQ()
      IF (NBCHCI.EQ.0) GOTO 9999
      VCINE = NOMCI
      NU = NOMNU
      VVALE = VCINE//'.VALE'
      VALR(1)=INST
      CNOIMP='&&CALVCI.CNOIMP'
      CNSIMP='&&CALVCI.CNSIMP'


C --- CREATION DU CHAM_NO ( SI IL EXISTE DEJA ON LE DETRUIT )
C     ---------------------------------------------------------
      CALL DETRSD('CHAMP_GD',VCINE)
      CALL JEDETR(VCINE//'.DLCI')
      CALL DISMOI('F','NB_EQUA',NU,'NUME_DDL',NEQ,KBID,IER)
      CALL DISMOI('F','NOM_GD',NU,'NUME_DDL',IBID,GD,IER)
      CALL DISMOI('F','NOM_MAILLA',NU,'NUME_DDL',IBID,NOMMA,IER)
      CALL DISMOI('F','NB_NO_MAILLA',NOMMA,'MAILLAGE',NBNO,KBID,IER)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',GD),NUMGD)
      CALL JEVEUO('&CATA.GD.TYPEGD','L',IDTYP)
      TYPVAL = ZK8(IDTYP-1+NUMGD)
      CALL JEVEUO(JEXNUM('&CATA.GD.DESCRIGD',NUMGD),'L',IDDES)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'L',JNOCMP)
      NEC = ZI(IDDES+2 )
      PRCHNO = NU//'.NUME'
      CALL CRCHN2(VCINE,PRCHNO,GD,NOMMA,BASE,TYPVAL,NBNO,NEQ)

C --- ALLOCATION DE VCINE.DLCI QUI SERVIRA DANS NMCVCI :
      CALL WKVECT(VCINE//'.DLCI','V V I',NEQ,JDLCI)

      CALL JEVEUO(VVALE,'E',IVVALE)
      CALL JEVEUO(NU//'.NUME.NUEQ','L',JNUEQ)
      CALL JEVEUO(NU//'.NUME.DEEQ','L',JDEEQ)
      CALL JENONU(JEXNOM(NU//'.NUME.LILI','&MAILLA'),IMAILL)
      CALL JEVEUO(JEXNUM(NU//'.NUME.PRNO',IMAILL),'L',JPRNO)
      CALL JEVEUO(NOMMA//'.COORDO    .VALE','L',ICOOR)


C --- BOUCLE SUR LES CHARGES CINEMATIQUES :
      DO 1 ICHCIN = 1,NBCHCI
        CHARCI = LCHCI(ICHCIN)
        CALL JEVEUO(CHARCI//'.AFCK','L',JAFCK)
        PHEN=ZK8(JAFCK-1+1)(1:4)
        FONC=ZK8(JAFCK-1+1)(6:6).EQ.'F'
        EVOIM=ZK8(JAFCK-1+3)
        CALL JEVEUO(CHARCI//'.AFCI','L',JAFCI)
        IF (EVOIM.EQ.' ') CALL JEVEUO(CHARCI//'.AFCV','L',JAFCV)


C       -- CAS DE EVOL_IMPO : ON PREPARE ...
C       ---------------------------------------
        IF (EVOIM.NE.' ') THEN
C         -- IL FAUT INTERPOLER EVOIM A L'INSTANT: INST :
          IF (GD.EQ.'DEPL_R') THEN
            NOMCH='DEPL'
          ELSEIF (GD.EQ.'TEMP_R') THEN
            NOMCH='TEMP'
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
          CALL ASSERT(FONC)
          CALL RSINCH(EVOIM,NOMCH,'INST',INST,CNOIMP,'EXCLU',
     &                'EXCLU',2,'V',IER)
          CALL CNOCNS(CNOIMP,'V',CNSIMP)
          CALL DETRSD('CHAMP',CNOIMP)
          CALL JEVEUO(CNSIMP//'.CNSK','L',JCN1K)
          CALL JEVEUO(CNSIMP//'.CNSD','L',JCN1D)
          CALL JEVEUO(CNSIMP//'.CNSC','L',JCN1C)
          CALL JELIRA(CNSIMP//'.CNSC','LONMAX',NBCMP1,KBID)
          CALL ASSERT(NBCMP1.EQ.ZI(JCN1D-1+2))
          CALL JEVEUO(CNSIMP//'.CNSV','L',JCN1V)
          CALL JEVEUO(CNSIMP//'.CNSL','L',JCN1L)
          VALK(1)=EVOIM
        ENDIF


C       -- AFFECTATION DES VALEURS IMPOSEES
C       ---------------------------------------
        NBIMP = ZI(JAFCI)



C       -- CAS DES VALEURS REELLES :
C       ---------------------------------
        IF (TYPVAL.EQ.'R') THEN
          DO 10 NIMP = 1, NBIMP
            N =3*(NIMP-1)+JAFCI
C           -- NI : NUMERO DU NOEUD
            NI = ZI(N+1)
C           -- NDDL : NUMERO DE LA COMPOSANTE (POUR LE NOEUD NI)
            NDDL = ZI(N+2)
            NN = (NEC+2)*(NI-1)
            NUEQ =ZI(JNUEQ-1+ ZI(JPRNO+NN)+NDDL-1)


C           -- CAS EVOL_IMPO (CNSIMP):
C           ----------------------------------
            IF (EVOIM.NE.' ') THEN
              INO=ZI(JDEEQ-1+2*(NUEQ-1)+1)
              ICMP=ZI(JDEEQ-1+2*(NUEQ-1)+2)
              CALL ASSERT (INO.EQ.NI)
              NOCMP=ZK8(JNOCMP-1+ICMP)
              VALI(1)=INO
              VALK(2)=NOCMP
              ICMP1=INDIK8(ZK8(JCN1C),NOCMP,1,NBCMP1)
              CALL ASSERT(ICMP1.GT.0)
              IF (.NOT.ZL(JCN1L-1+(INO-1)*NBCMP1+ICMP1))
     &           CALL U2MESG('F','CALCULEL_2',2,VALK,1,VALI,1,VALR)
              RES = ZR(JCN1V-1+(INO-1)*NBCMP1+ICMP1)
              ZR(IVVALE-1+NUEQ) = RES


C           -- CAS "NORMAL" (OBJET .AFCV) :
C           ----------------------------------
            ELSE IF (.NOT.FONC) THEN
              ZR(IVVALE-1+NUEQ) = ZR(JAFCV-1+NIMP)


C           -- CAS FONCTION :
C           -----------------
            ELSE IF (FONC) THEN
              NOMF = ZK8(JAFCV-1+NIMP)
              NOMP(1)='INST'
              NOMP(2)='X'
              NOMP(3)='Y'
              NOMP(4)='Z'
              VALP(1)=INST
              VALP(2)=ZR(ICOOR+3*(NI-1)+0)
              VALP(3)=ZR(ICOOR+3*(NI-1)+1)
              VALP(4)=ZR(ICOOR+3*(NI-1)+2)
              CALL FOINTE('F ',NOMF,4,NOMP,VALP,RES,IER)
              ZR(IVVALE-1+NUEQ) = RES
            ELSE
              CALL U2MESS('F','CALCULEL_37')
            ENDIF

            ZI(JDLCI-1+NUEQ) = 1
10        CONTINUE



C       -- CAS DES VALEURS COMPLEXES :
C       ---------------------------------
        ELSE IF (TYPVAL.EQ.'C') THEN
          CALL ASSERT(PHEN.EQ.'CIAC')
          CALL ASSERT(.NOT.FONC)
          DO 20 NIMP = 1, NBIMP
            N =3*(NIMP-1)+JAFCI
            NI = ZI(N+1)
            NDDL = ZI(N+2)
            NN = (NEC+2)*(NI-1)
            NUEQ =ZI(JNUEQ-1+ ZI(JPRNO+NN)+NDDL-1)
            ZC(IVVALE-1+NUEQ) = ZC(JAFCV-1+NIMP)
            ZI(JDLCI-1+NUEQ) = 1
20        CONTINUE


        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
1     CONTINUE

 9999 CONTINUE

      CALL DETRSD('CHAMP',CNSIMP)
      CALL JEDEMA()
      END
