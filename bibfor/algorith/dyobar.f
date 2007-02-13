      SUBROUTINE DYOBAR(MAILLA,NBMODS,INSTAP,ITERAT,
     &                  VITPLU,ACCPLU,VALPLU,DEPDEL,
     &                  SECMBR,IMPRCO,SUIVCO,
     &                  DEPENT,VITENT,ACCENT,CNSINR,
     &                  NOMTAB, NBOBS, NUOBSE, LSUIVI)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE  CRP_20
C-----------------------------------------------------------------------
C
      IMPLICIT     NONE
      INTEGER      NBMODS,NBOBS,NUOBSE
      CHARACTER*8  MAILLA
      REAL*8       INSTAP
      INTEGER      ITERAT
      CHARACTER*24 VALPLU
      CHARACTER*24 VITPLU
      CHARACTER*24 ACCPLU
      CHARACTER*24 DEPDEL
      CHARACTER*24 SECMBR(8)
      CHARACTER*24 DEPENT
      CHARACTER*24 VITENT
      CHARACTER*24 ACCENT
      CHARACTER*24 IMPRCO
      CHARACTER*24 SUIVCO
      CHARACTER*19 CNSINR,NOMTAB
      LOGICAL      LSUIVI
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : OP0070
C ----------------------------------------------------------------------
C
C REALISATION D'UN SUIVI EN TEMPS REEL ET/OU D'UNE OBSERVATION
C
C IN  MAILLA : NOM DU MAILLAGE
C IN  NBMODS : NOMBRE DE MODES STATIQUES
C IN  INSTAP : INSTANT COURANT
C IN  ITERAT : ITERATION COURANTE
C IN  VALPLU : VARIABLES A L'INSTANT ACTUEL (DANS CHAPEAU)
C IN  VITPLU : VITESSES A L'INSTANT ACTUEL
C IN  ACCPLU : ACCELERATIONS A L'INSTANT ACTUEL
C IN  SECMBR : SECOND MEMBRE A L'INSTANT ACTUEL (DANS CHAPEAU)
C IN  SUIVCO : SD POUR LE SUIVI DE DDL
C IN  IMPRCO : SD POUR L'AFFICHAGE
C IN  DEPENT : DEPLACEMENTS DE REFERENCE POUR LES DEPLACEMENTS ABSOLUS
C IN  VITENT : VITESSES DE REFERENCE POUR LES VITESSES ABSOLUS
C IN  ACCENT : ACCELERATIONS DE REFERENCE POUR LES ACCELERATIONS
C              ABSOLUS
C IN  CNSINR : CHAM_NO_S CONTENANT LES INFOS SUR LE CONTACT
C IN  NOMTAB : NOM DE LA TABLE DES OBSERVATIONS
C IN  NBOBS  : NOMBRE DE FONCTIONS (= NOMBRE D'OBSERVATIONS PAR INSTANT
C               D'OBSERVATION)
C IN  NUOBSE : NUMERO DE L'OBSERVATION
C IN  LSUIVI : = TRUE  SI ON REALISE UN SUIVI DDL
C              = FALSE SI ON REALISE UNE OBSERVATION

C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      ISUI,INOEUD,ICMP,VARINT,ICTC,ISND,NBSUIV,IBID,TABI(2)
      INTEGER      JDEPP,JVITP,JACCP,JDEPEN,JVITEN,JACCEN,JCONT,JDEPDE
      INTEGER      JCNSD,JCNSC,JCNSV,JCNSL,JCESC,JCESD,JCESL,IRET,NBNO
      INTEGER      INO,NBMA,IAD,JCESV,NNB,I,IICHAM,IICOMP,IIPOIN,IIMAIL
      INTEGER      IINOEU,IINUCM
      REAL*8       VALR,CONST(4),R8MAEM,TABR(2)
      COMPLEX*16   CBID
      CHARACTER*13 RESULT,CONCEP,NOMCMD
      CHARACTER*16 CHAM,TABK(4),NPARAN(6),NPARAS(7)
      CHARACTER*24 K24BID
      INTEGER      JCHAM,JCOMP,JNUCM,JNOEU,JMAIL,JPOIN,JSUINB,JEXTR
      INTEGER      KCHAM,KCOMP,KNUCM,KNOEU,KMAIL,KPOIN
      INTEGER      SUBTOP,TYPCHA,ICOMP,NBCMP,IMA,NBPT,NBSP,IPT,ISP
      CHARACTER*24 DEPPLU,SIGPLU,VARPLU,NOMCH(4),CHAMFO(4)
      CHARACTER*24 CNFEDO,CNFEPI,CNFSDO,CNFSPI,CHTRAV,CHTR1
      CHARACTER*19 CNSINZ
      CHARACTER*8  CMP,TOPO
      CHARACTER*16 K16BID
      LOGICAL      LISCH(4)
C
      DATA NPARAN / 'NUME_ORDRE' , 'INST' , 'NOM_CHAM' , 'NOM_CMP',
     &              'NOEUD' , 'VALE' /
C
      DATA NPARAS / 'NUME_ORDRE' , 'INST' , 'NOM_CHAM' , 'NOM_CMP',
     &              'MAILLE'  , 'POINT' , 'VALE' /

C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CHTR1='&&SUIDDL.TR1'
      CHTRAV='&&SUIDDL.TRAV'
C
      CALL GETRES(RESULT,CONCEP,NOMCMD)
      IF(LSUIVI)THEN
        CALL JEVEUO(SUIVCO(1:14)//'NBSUIV'   ,'L',JSUINB)
        NBSUIV = ZI(JSUINB)
        IF (NBSUIV.EQ.0) THEN
          GOTO 999
        ENDIF
        CALL JEVEUO(SUIVCO(1:14)//'NOM_CHAM' ,'L',JCHAM)
        CALL JEVEUO(SUIVCO(1:14)//'NOM_CMP ' ,'L',JCOMP)
        CALL JEVEUO(SUIVCO(1:14)//'NUME_CMP' ,'L',JNUCM)
        CALL JEVEUO(SUIVCO(1:14)//'NOEUD'    ,'L',JNOEU)
        CALL JEVEUO(SUIVCO(1:14)//'MAILLE'   ,'L',JMAIL)
        CALL JEVEUO(SUIVCO(1:14)//'POINT'    ,'L',JPOIN)
        CALL JEVEUO(SUIVCO(1:14)//'EXTREMA'  ,'L',JEXTR)
        NNB=NBSUIV
        IICHAM=JCHAM
        IICOMP=JCOMP
        IIMAIL=JMAIL
        IINOEU=JNOEU
        IIPOIN=JPOIN
        IINUCM=JNUCM
      ELSE
        CALL JEVEUO ( '&&DYOBSE.NOM_CHAM' , 'L' , KCHAM )
        CALL JEVEUO ( '&&DYOBSE.NOM_CMP ' , 'L' , KCOMP )
        CALL JEVEUO ( '&&DYOBSE.NUME_CMP' , 'L' , KNUCM )
        CALL JEVEUO ( '&&DYOBSE.NOEUD'    , 'L' , KNOEU )
        CALL JEVEUO ( '&&DYOBSE.MAILLE'   , 'L' , KMAIL )
        CALL JEVEUO ( '&&DYOBSE.POINT'    , 'L' , KPOIN )
        NNB=NBOBS
        IICHAM=KCHAM
        IICOMP=KCOMP
        IIMAIL=KMAIL
        IINOEU=KNOEU
        IIPOIN=KPOIN
        IINUCM=KNUCM
      ENDIF

C
C --- SI ON DEMANDE DES DDL ABSOLUS EN STATIQUE -> ARRET
C
      IF (NOMCMD(1:13).EQ.'STAT_NON_LINE')THEN
        DO 10 I = 1 , NNB
          CHAM   = ZK16(IICHAM-1+I)
          IF ( CHAM(1:11) .EQ. 'DEPL_ABSOLU'.OR.
     &         CHAM(1:11) .EQ. 'VITE_ABSOLU'.OR.
     &         CHAM(1:11) .EQ. 'ACCE_ABSOLU') THEN
                 CALL UTDEBM('F','SUIDDL','ERREUR FATALE')
                 CALL UTIMPK('L','LE CHAMP',1,CHAM(1:11))
                 CALL UTIMPK('S','EST INCOMPATIBLE AVEC'//
     &            ' LA COMMANDE ',1,NOMCMD(1:13))
                 CALL UTFINM()
          ENDIF
 10     CONTINUE
      ENDIF
C
C --- DECOMPACTION DES VARIABLES CHAPEAUX
C
      CALL DESAGG(VALPLU,DEPPLU,SIGPLU,VARPLU,K24BID,K24BID,K24BID,
     &            K24BID,K24BID)
      IF(LSUIVI)THEN
        CALL DESAGG(SECMBR,CNFEDO,CNFEPI,K24BID,K24BID,CNFSDO,CNFSPI,
     &              K24BID,K24BID)
      ENDIF
C
C --- RECUPERATIONS DES POINTEURS JEVEUX SUR LES CHAMPS
C
      CALL JEVEUO(DEPPLU(1:19)//'.VALE','L',JDEPP)
      IF(LSUIVI)THEN
        CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      ENDIF
      IF (NOMCMD(1:13).NE.'STAT_NON_LINE')THEN
         CALL JEVEUO(VITPLU(1:19)//'.VALE','L',JVITP)
         CALL JEVEUO(ACCPLU(1:19)//'.VALE','L',JACCP)
         CALL JEVEUO(DEPENT(1:19)//'.VALE','L',JDEPEN)
         CALL JEVEUO(VITENT(1:19)//'.VALE','L',JVITEN)
         CALL JEVEUO(ACCENT(1:19)//'.VALE','L',JACCEN)
      ENDIF
C
C --- ISND POUR LES FORCES NODALES
C
      IF(LSUIVI)THEN
        ISND = 1
      ELSE
        ISND = 0
        TABI(1) = NUOBSE
        TABR(1) = INSTAP
      ENDIF
C
C --- POUR LE CONTACT: CONVERSION CHAM_NO EN CHAM_NO_S
C
      CALL JEEXIN(CNSINR//'.CNSV',ICTC)
      IF (ICTC.NE.0) THEN
        CNSINZ = '&&DYOBAR.CNSINZ'
        CALL CNSCNO(CNSINR,' ','OUI','V',CNSINZ)
        CALL JEVEUO(CNSINZ(1:19)//'.VALE','L',JCONT)
      ENDIF


      DO 30 ISUI = 1 , NNB
C
C --- CHAMP
C
         CHAM   = ZK16(IICHAM-1+ISUI)
C
C --- IDENTIFICATION DU CHAMP A EXTRAIRE
C
         TYPCHA = 0
         IF     ( CHAM(1:11) .EQ. 'DEPL_ABSOLU' ) THEN
           TYPCHA = 1
         ELSEIF ( CHAM(1:11) .EQ. 'VITE_ABSOLU' ) THEN
           TYPCHA = 2
         ELSEIF ( CHAM(1:11) .EQ. 'ACCE_ABSOLU' ) THEN
           TYPCHA = 3
         ELSEIF ( CHAM(1:4) .EQ. 'DEPL' ) THEN
           TYPCHA = 4
         ELSEIF ( CHAM(1:9) .EQ. 'VALE_CONT' ) THEN
           TYPCHA = 5
         ELSEIF ( CHAM(1:4) .EQ. 'VITE' ) THEN
           TYPCHA = 6
         ELSEIF ( CHAM(1:4) .EQ. 'ACCE' ) THEN
           TYPCHA = 7
         ELSEIF ( CHAM(1:9) .EQ. 'SIEF_ELGA' ) THEN
           TYPCHA = 8
         ELSEIF ( CHAM(1:9) .EQ. 'VARI_ELGA' ) THEN
           TYPCHA = 9
         ELSEIF ( LSUIVI .AND. CHAM(1:9) .EQ. 'FORC_NODA' ) THEN
           TYPCHA = 10
         ELSE
           CALL U2MESS('F','CALCULEL_17')
         ENDIF
C
C --- COMPOSANTE
C
         CMP    = ZK8(IICOMP-1+ISUI)
C
C --- ENTITE TOPOLOGIQUE PRINCIPALE (NOEUD OU MAILLE)
C --- SI MAILLE -> SOUS-ENTITE TOPOLOGIQUE  (POINT DE GAUSS)
C
         IF (LSUIVI .AND. ZI(JEXTR-1+ISUI).NE.0) GOTO 150
         SUBTOP = 0
         IF ((TYPCHA.EQ.8).OR.(TYPCHA.EQ.9)) THEN
           TOPO   = ZK8(IIMAIL-1+ISUI)
           SUBTOP = ZI(IIPOIN-1+ISUI)
         ELSE
           TOPO   = ZK8(IINOEU-1+ISUI)
         ENDIF
C
C --- NUMERO DE VARIABLE INTERNE
C
         VARINT = 0
         IF (TYPCHA.EQ.9) THEN
           VARINT  = ZI(IINUCM-1+ISUI)
         ENDIF
C
C --- DEPL, VITE ET ACCE ABSOLUS -> Y A T-IL DES MODES STATIQUES ?
C
           IF ((TYPCHA.GE.1).AND.(TYPCHA.LE.3)) THEN
             IF (NBMODS.EQ.0) THEN
               IF(LSUIVI)THEN
                 VALR = 0.D0
                 GOTO 15
               ELSE
                 CALL U2MESS('F','ALGORITH3_45')
               ENDIF
             ENDIF
           ENDIF
C
           CALL DYOEXT(MAILLA,TYPCHA,TOPO,SUBTOP,CMP,VARINT,
     &                 DEPPLU,VITPLU,ACCPLU,SIGPLU,VARPLU,
     &                 DEPENT,VITENT,ACCENT,
     &                 ICTC,CNSINZ,
     &                 ISND,CNFEDO,CNFEPI,CNFSDO,CNFSPI,
     &                 INOEUD,ICMP,VALR)
C
           IF ((ICMP.EQ.0).OR.(INOEUD.EQ.0)) THEN
             IF(LSUIVI)THEN
               VALR = 0.D0
               CALL U2MESS('A','ALGORITH10_79')
               GOTO 15
             ELSEIF(TYPCHA.NE.8 .AND. TYPCHA.NE.9)THEN
               CALL UTDEBM('F','DYOBAR','ERREUR FATALE')
               CALL UTIMPK('L','L''ENTITE ',1,TOPO(1:8))
               CALL UTIMPK('S',' N''EST PAS POSSIBLE ',0,' ')
               CALL UTFINM()
             ENDIF
           ENDIF
C
C ---
C
           IF(LSUIVI)THEN


             IF     (TYPCHA.EQ.1) THEN
               VALR = ZR(JDEPP+ICMP-1) + ZR(JDEPEN+ICMP-1)
             ELSEIF (TYPCHA.EQ.2) THEN
               VALR = ZR(JVITP+ICMP-1) + ZR(JVITEN+ICMP-1)
             ELSEIF (TYPCHA.EQ.3) THEN
               VALR = ZR(JACCP+ICMP-1) + ZR(JACCEN+ICMP-1)
             ELSEIF (TYPCHA.EQ.4) THEN
               VALR = ZR(JDEPP+ICMP-1)
             ELSEIF (TYPCHA.EQ.5) THEN
               VALR = ZR(JCONT+ICMP-1)
             ELSEIF (TYPCHA.EQ.6) THEN
               VALR = ZR(JVITP+ICMP-1)
             ELSEIF (TYPCHA.EQ.7) THEN
               VALR = ZR(JACCP+ICMP-1)
             ELSEIF (TYPCHA.EQ.8) THEN
C              VALR = VALR
             ELSEIF (TYPCHA.EQ.9) THEN
C              VALR = VALR
             ELSEIF (TYPCHA.EQ.10) THEN
C              VALR = VALR
             ELSE
               CALL U2MESS('F','ALGORITH10_80')
             ENDIF
             CALL JEDETR(CHTR1)
             CALL JEDETR(CHTRAV)
             GOTO 15
           ELSE

             TABK(1) = CHAM
             TABK(2) = CMP
             IF     (TYPCHA.EQ.1) THEN
               TABK(3) = TOPO
               TABR(2) = ZR(JDEPP+ICMP-1) + ZR(JDEPEN+ICMP-1)
               CALL TBAJLI(NOMTAB,6,NPARAN,TABI,TABR,CBID,TABK,0)
             ELSEIF (TYPCHA.EQ.2) THEN
               TABK(3) = TOPO
               TABR(2) = ZR(JVITP+ICMP-1) + ZR(JVITEN+ICMP-1)
               CALL TBAJLI(NOMTAB,6,NPARAN,TABI,TABR,CBID,TABK,0)
             ELSEIF (TYPCHA.EQ.3) THEN
               TABK(3) = TOPO
               TABR(2) = ZR(JACCP+ICMP-1) + ZR(JACCEN+ICMP-1)
               CALL TBAJLI(NOMTAB,6,NPARAN,TABI,TABR,CBID,TABK,0)
             ELSEIF (TYPCHA.EQ.4) THEN
               TABK(3) = TOPO
               TABR(2) = ZR(JDEPP+ICMP-1)
               CALL TBAJLI(NOMTAB,6,NPARAN,TABI,TABR,CBID,TABK,0)
             ELSEIF (TYPCHA.EQ.5) THEN
               TABK(3) = TOPO
               TABR(2) = ZR(JCONT+ICMP-1)
               CALL TBAJLI(NOMTAB,6,NPARAN,TABI,TABR,CBID,TABK,0)
             ELSEIF (TYPCHA.EQ.6) THEN
               TABK(3) = TOPO
               TABR(2) = ZR(JVITP+ICMP-1)
               CALL TBAJLI(NOMTAB,6,NPARAN,TABI,TABR,CBID,TABK,0)
             ELSEIF (TYPCHA.EQ.7) THEN
               TABK(3) = TOPO
               TABR(2) = ZR(JACCP+ICMP-1)
               CALL TBAJLI(NOMTAB,6,NPARAN,TABI,TABR,CBID,TABK,0)
             ELSEIF (TYPCHA.EQ.8) THEN
               TABK(3) = TOPO
               TABI(2) = SUBTOP
               TABR(2) = VALR
               CALL TBAJLI(NOMTAB,7,NPARAS,TABI,TABR,CBID,TABK,0)
             ELSEIF (TYPCHA.EQ.9) THEN
               TABK(3) = TOPO
               TABI(2) = SUBTOP
               TABR(2) = VALR
               CALL TBAJLI(NOMTAB,7,NPARAS,TABI,TABR,CBID,TABK,0)
             ELSE
               CALL U2MESS('F','CALCULEL_17')
             ENDIF
             GOTO 30
           ENDIF


 150       CONTINUE

           IF (TYPCHA.EQ.1) THEN
             CONST(1)=1.D0
             CONST(2)=1.D0
             NOMCH(1)=DEPPLU
             NOMCH(2)=DEPENT
             CALL VTCMBL(2,'R',CONST,'R',NOMCH,'R',CHTR1)
             CALL CNOCNS(CHTR1,'V',CHTRAV)
           ELSEIF (TYPCHA.EQ.2) THEN
             CONST(1)=1.D0
             CONST(2)=1.D0
             NOMCH(1)=VITPLU
             NOMCH(2)=VITENT
             CALL VTCMBL(2,'R',CONST,'R',NOMCH,'R',CHTR1)
             CALL CNOCNS(CHTR1,'V',CHTRAV)
           ELSEIF (TYPCHA.EQ.3) THEN
             CONST(1)=1.D0
             CONST(2)=1.D0
             NOMCH(1)=ACCPLU
             NOMCH(2)=ACCENT
             CALL VTCMBL(2,'R',CONST,'R',NOMCH,'R',CHTR1)
             CALL CNOCNS(CHTR1,'V',CHTRAV)
           ELSEIF (TYPCHA.EQ.4) THEN
             CALL CNOCNS(DEPPLU,'V',CHTRAV)
           ELSEIF (TYPCHA.EQ.5) THEN
             CHTRAV=CNSINZ
           ELSEIF (TYPCHA.EQ.6) THEN
             CALL CNOCNS(VITPLU,'V',CHTRAV)
           ELSEIF (TYPCHA.EQ.7) THEN
             CALL CNOCNS(ACCPLU,'V',CHTRAV)
           ELSEIF (TYPCHA.EQ.8) THEN
             CALL CELCES(SIGPLU,'V',CHTRAV)
           ELSEIF (TYPCHA.EQ.9) THEN
             CALL CELCES(VARPLU,'V',CHTRAV)
           ELSEIF (TYPCHA.EQ.10) THEN
             CALL JEEXIN(CNFEDO(1:19)//'.VALE',IRET)
             IF (IRET.NE.0) THEN
               LISCH(1)=.TRUE.
               NOMCH(1)=CNFEDO
             ELSE
               LISCH(1)=.FALSE.
             ENDIF
             CALL JEEXIN(CNFEPI(1:19)//'.VALE',IRET)
             IF (IRET.NE.0) THEN
               LISCH(2)=.TRUE.
               CHAMFO(2)=CNFEPI
             ELSE
               LISCH(2)=.FALSE.
             ENDIF
             CALL JEEXIN(CNFSDO(1:19)//'.VALE',IRET)
             IF (IRET.NE.0) THEN
               LISCH(3)=.TRUE.
               CHAMFO(3)=CNFSDO
             ELSE
               LISCH(3)=.FALSE.
             ENDIF
              CALL JEEXIN(CNFSPI(1:19)//'.VALE',IRET)
             IF (IRET.NE.0) THEN
               LISCH(4)=.TRUE.
               CHAMFO(4)=CNFSPI
             ELSE
               LISCH(4)=.FALSE.
             ENDIF
             ICOMP=1
             DO 20 I=1,4
               IF (LISCH(I)) THEN
                 CONST(ICOMP)=1.D0
                 NOMCH(ICOMP)=CHAMFO(I)
                 ICOMP=ICOMP+1
               ENDIF
  20         CONTINUE
             CALL VTCMBL(ICOMP,'R',CONST,'R',NOMCH,'R',CHTR1)
             CALL CNOCNS(CHTR1,'V',CHTRAV)
           ELSE
             CALL U2MESS('F','ALGORITH10_80')
           ENDIF

           IF (TYPCHA.EQ.8.OR.TYPCHA.EQ.9) THEN
             CALL JEVEUO(CHTRAV(1:19)//'.CESC','L',JCESC)
             CALL JEVEUO(CHTRAV(1:19)//'.CESD','L',JCESD)
             CALL JEVEUO(CHTRAV(1:19)//'.CESL','L',JCESL)
             CALL JEVEUO(CHTRAV(1:19)//'.CESV','L',JCESV)
             NBCMP=ZI(JCESD+4)
             DO 40 I=1,NBCMP
               IF (CMP.EQ.ZK8(JCESC-1+I)) ICMP=I
  40         CONTINUE
             NBMA=ZI(JCESD)

             IF (ZI(JEXTR-1+ISUI).EQ.1) THEN
               VALR=R8MAEM()
               DO 50 IMA=1,NBMA
                 NBPT=ZI(JCESD+5+4*(IMA-1))
                 NBSP=ZI(JCESD+5+4*(IMA-1)+1)
                 DO 60 IPT=1,NBPT
                   DO 70 ISP=1,NBSP
                     CALL CESEXI('S',JCESD,JCESL,IMA,IPT,ISP,ICMP,IAD)
                     IF (IAD.GT.0) VALR=MIN(VALR,ZR(JCESV+IAD-1))
  70               CONTINUE
  60             CONTINUE
  50           CONTINUE

             ELSE IF (ZI(JEXTR-1+ISUI).EQ.2) THEN
               VALR=-R8MAEM()
               DO 80 IMA=1,NBMA
                 NBPT=ZI(JCESD+5+4*(IMA-1))
                 NBSP=ZI(JCESD+5+4*(IMA-1)+1)
                 DO 90 IPT=1,NBPT
                   DO 100 ISP=1,NBSP
                     CALL CESEXI('S',JCESD,JCESL,IMA,IPT,ISP,ICMP,IAD)
                     IF (IAD.GT.0) VALR=MAX(VALR,ZR(JCESV+IAD-1))
  100              CONTINUE
  90             CONTINUE
  80           CONTINUE
             ENDIF
           ELSE
             CALL JEVEUO(CHTRAV(1:19)//'.CNSD','L',JCNSD)
             CALL JEVEUO(CHTRAV(1:19)//'.CNSC','L',JCNSC)
             CALL JEVEUO(CHTRAV(1:19)//'.CNSV','L',JCNSV)
             CALL JEVEUO(CHTRAV(1:19)//'.CNSL','L',JCNSL)
             NBCMP=ZI(JCNSD+1)
             DO 110 I=1,NBCMP
               IF (CMP.EQ.ZK8(JCNSC-1+I)) ICMP=I
  110        CONTINUE
             NBNO=ZI(JCNSD)
             IF (ZI(JEXTR-1+ISUI).EQ.1) THEN
               VALR=R8MAEM()
               DO 120 INO=1,NBNO
                 IF(ZL(JCNSL+(INO-1)*NBCMP+ICMP-1)) THEN
                   VALR=MIN(VALR,ZR(JCNSV+(INO-1)*NBCMP+ICMP-1))
                 ENDIF
  120          CONTINUE
             ELSE IF (ZI(JEXTR-1+ISUI).EQ.2) THEN
               VALR=-R8MAEM()
               DO 130 INO=1,NBNO
                 IF(ZL(JCNSL+(INO-1)*NBCMP+ICMP-1)) THEN
                   VALR=MAX(VALR,ZR(JCNSV+(INO-1)*NBCMP+ICMP-1))
                 ENDIF
  130          CONTINUE
             ENDIF
           ENDIF
         CALL JEDETR(CHTR1)
         CALL JEDETR(CHTRAV)
  15     CONTINUE
C
C --- AFFICHAGE DANS LE TABLEAU
C
         IF (ISUI.EQ.1) THEN
           CALL IMPSDR(IMPRCO(1:14),
     &                 'SUIV_1   ',K16BID,VALR,IBID)
         ELSE IF (ISUI.EQ.2) THEN
           CALL IMPSDR(IMPRCO(1:14),
     &                 'SUIV_2   ',K16BID,VALR,IBID)
         ELSE IF (ISUI.EQ.3) THEN
           CALL IMPSDR(IMPRCO(1:14),
     &                 'SUIV_3   ',K16BID,VALR,IBID)
         ELSE IF (ISUI.EQ.4) THEN
           CALL IMPSDR(IMPRCO(1:14),
     &                 'SUIV_4   ',K16BID,VALR,IBID)
         ELSE
           CALL U2MESS('F','ALGORITH10_81')
         ENDIF
C
 30   CONTINUE
C
 999  CONTINUE
C
      CALL JEDEMA()

      END
