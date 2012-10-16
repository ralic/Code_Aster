      SUBROUTINE XTYELE(NOMA,MODELX,TRAV,NFISS,FISS,CONTAC,NDIM,LINTER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 15/10/2012   AUTEUR GENIAUT S.GENIAUT 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE PELLET J.PELLET
C TOLE CRS_1404
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8  NOMA,MODELX
      CHARACTER*24 TRAV
      INTEGER     NFISS
      CHARACTER*8  FISS(NFISS)
      INTEGER     CONTAC,NDIM,IRET
      LOGICAL     LINTER
C
C ----------------------------------------------------------------------
C
C --- ROUTINE XFEM
C
C --- REMPLISSAGE DE TAB, QUI DEFINIE LE TYPE D'ELEMENT XFEM POUR
C --- LA CREATION DU MODELE
C
C ----------------------------------------------------------------------
C
C
C
C
      REAL*8      R8MAEM,R8PREM,MINLSN,MINLST,MAXLSN,LSN
      REAL*8      DDOT,LSNA,LSTA,LSNB,LSTB,LSTC
      REAL*8      A(NDIM),B(NDIM),AB(NDIM),C(NDIM),AC(NDIM)
      REAL*8      CMIN(NDIM),LONGAR,M(NDIM),PADIST,RBID
      INTEGER     NMAENR,KK,JGRP,JCOOR,NBMA
      INTEGER     JLSN,JLST,JMASUP,JTMDIM,JTYPMA,JCONX1,JCONX2
      INTEGER     NBCOUP,NBCOU2,IBID,IFISS,ITYPMA,JTAB,JNBPT,JNBPT2
      INTEGER     NMASUP,NDIME,NBAR,NBHEAV,JSTNL(NFISS),JSTNV(NFISS)
      INTEGER     INO,INO2,NNGL,NNOT(3),NNO,NNO2,IMA,IMA2
      INTEGER     I,J,K
      INTEGER     AR(12,3),IA,NUNOA,NUNOB,STNA,STNB
      INTEGER     FA(6,4),IBID3(12,3),NBF,IFQ,CODRET,ILSN,ILST,IGEOM
      CHARACTER*2  CH2
      CHARACTER*8  TYPMA,K8BID,NOMAIL
      CHARACTER*19 CLSN,CLST,CNXINV,CSTN(NFISS)
      CHARACTER*24 GRP(3)
      LOGICAL     LCONT,ISMALI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATION
C
      LINTER = .FALSE.
      CLSN = '&&XTYELE.LSN'
      CLST = '&&XTYELE.LST'
      DO 40 IFISS=1,NFISS
        CALL CODENT(IFISS,'G',CH2)
        CSTN(IFISS)='&&XTYELE.STN'//CH2
 40   CONTINUE
      CALL JEVEUO(NOMA(1:8)//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO('&CATA.TM.TMDIM','L',JTMDIM)
      CALL JEVEUO(NOMA(1:8)//'.TYPMAIL','L',JTYPMA)
C
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IBID)
      CALL JEVEUO(NOMA(1:8)//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',JCONX2)
      CNXINV = '&&XTYELE.CNCINV'
      CALL CNCINV(NOMA,IBID,0,'V',CNXINV)

C     RECUPERATION DE L'ADRESSE DU TABLEAU DE TRAVAIL
      CALL JEVEUO(TRAV,'E',JTAB)
C --- CREATION DE L'OBJET CONTENANT LE NOMBRE DE FISSURE VUE PAR MAILLE,
C --- CORRESPOND AU NOMBRE DE SOUS POINTS POUR LA CREATION DES SD XCONNO
      CALL WKVECT('&&XTYELE.NBSP' ,'V V I',NBMA,JNBPT)
C --- CREATION DE L'OBJET CONTENANT LE NOMBRE DE FONCTIONS HEAVISIDES
C --- PAR MAILLES POUR LES MAILLES QUI VOIENT PLUS DE 2 FISSURES
C --- CORRESPOND AU NOMBRE DE SOUS POINTS POUR LA SD FISSNO
      CALL WKVECT('&&XTYELE.NBSP2' ,'V V I',NBMA,JNBPT2)
C
C --- BOUCLE SUR NOMBRE OCCURRENCES FISSURES
C
      DO 10 IFISS = 1,NFISS
        CALL CNOCNS(FISS(IFISS)//'.LNNO','V',CLSN)
        CALL CNOCNS(FISS(IFISS)//'.LTNO','V',CLST)
        CALL CNOCNS(FISS(IFISS)//'.STNO','V',CSTN(IFISS))
        CALL JEVEUO(CLSN//'.CNSV','L',JLSN)
        CALL JEVEUO(CLST//'.CNSV','L',JLST)
        CALL JEVEUO(CSTN(IFISS)//'.CNSL','L',JSTNL(IFISS))
        CALL JEVEUO(CSTN(IFISS)//'.CNSV','L',JSTNV(IFISS))
        GRP(1) = FISS(IFISS)//'.MAILFISS.HEAV'
        GRP(2) = FISS(IFISS)//'.MAILFISS.CTIP'
        GRP(3) = FISS(IFISS)//'.MAILFISS.HECT'
C
C --- BOUCLE SUR LES GRP
C
        DO 20 KK = 1,3
          CALL JEEXIN(GRP(KK),IRET)
          IF (IRET.NE.0) THEN
            CALL JEVEUO(GRP(KK),'L',JGRP)
            CALL JELIRA(GRP(KK),'LONMAX',NMAENR,K8BID)
C
C --- BOUCLE SUR LES MAILLES DU GROUPE
C
            DO 30 I = 1,NMAENR
              IMA = ZI(JGRP-1+I)
C
              ZI(JNBPT-1+IMA) = ZI(JNBPT-1+IMA)+1
              ITYPMA=ZI(JTYPMA-1+IMA)

              IF (ZI(JTAB-1+5*(IMA-1)+4).EQ.0) THEN
C --- BLINDAGE DANS LE CAS DU MULTI-HEAVISIDE
                CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',IMA),NOMAIL)
                CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
                IF (.NOT.ISMALI(TYPMA)) THEN
                  CALL U2MESK('F','XFEM_41', 1 ,NOMAIL)
                ENDIF
              ENDIF

C --- ON RECUPERE LE NB DE NOEUDS SOMMETS DE LA MAILLE
              CALL PANBNO(ITYPMA,NNOT)
              NNO = NNOT(1)
C
C --- ON DETERMINE S'IL S'AGIT D'UNE MAILLE DE CONTACT OU PAS
              LCONT = .FALSE.
C
C --- SI LE CONTACT EST DECLARÉ DANS LE MODELE
C
              IF (CONTAC.GE.1) THEN
C --- PAS DE CONTACT POUR LES MAILLE DE BORD
                NDIME= ZI(JTMDIM-1+ITYPMA)
                IF (NDIME.NE.NDIM) GOTO 110

                MAXLSN=-1*R8MAEM()
                MINLSN=R8MAEM()
C --- BOUCLE SUR LES ARETES DE LA MAILLE
                CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
                CALL CONARE(TYPMA,AR,NBAR)
                DO 100 IA=1,NBAR
                  NUNOA=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+AR(IA,1)-1)
                  NUNOB=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+AR(IA,2)-1)
                  LSNA=ZR(JLSN-1+NUNOA)
                  LSNB=ZR(JLSN-1+NUNOB)
                  STNA=ZI(JSTNV(IFISS)-1+NUNOA)
                  STNB=ZI(JSTNV(IFISS)-1+NUNOB)
                  IF(LSNA.LT.MINLSN) MINLSN=LSNA
                  IF(LSNB.LT.MINLSN) MINLSN=LSNB
                  IF(LSNA.GT.MAXLSN) MAXLSN=LSNA
                  IF(LSNB.GT.MAXLSN) MAXLSN=LSNB
C --- ARETE OU NOEUD COUPÉ AVEC STATUT NUL -> MAILLE MULTI-H NON COUPÉE
                  IF (LSNA*LSNB.LE.0) THEN
                    IF (LSNA*LSNB.LT.0.AND.(STNA.EQ.0.OR.STNB.EQ.0).OR.
     &              LSNA.EQ.0.AND.STNA.EQ.0.OR.
     &              LSNB.EQ.0.AND.STNB.EQ.0) GOTO 110
                  ENDIF
 100            CONTINUE
C --- BOUCLE SUR LES NOEUDS DE LA MAILLE
C                DO 100 INO=1,NNO
C                  NNGL=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INO-1)
C                  LSN = ZR(JLSN-1+NNGL)
C                  IF (LSN.LT.MINLSN) MINLSN=LSN
C                  IF (LSN.GT.MAXLSN) MAXLSN=LSN
C 100            CONTINUE
C --- TRAITEMENT DES DIFFERENTS CAS
                IF (MINLSN*MAXLSN.LT.0) THEN
C--- LA MAILLE EST COUPÉE, ON ACTIVE LE CONTACT
                  LCONT=.TRUE.
                ELSEIF (MAXLSN.EQ.0) THEN
C --- SI LA MAILLE EST ENTIEREMENT DU COTÉ ESCLAVE, MAIS TOUCHE LA LSN
C --- LE CONTACT EST ACTIVÉ SI TOUT LES NOEUDS D'UNE FACE SONT COUPÉS
                  NBCOUP = 0
                  DO 200 INO=1,NNO
                    NNGL=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INO-1)
                    LSN = ZR(JLSN-1+NNGL)
                    IF (LSN.EQ.0) THEN
C --- LE NOEUD EST COUPÉ SI LE MAX DE LSN DE SA CONNECTIVITÉ
C --- EST STRICTEMENT POSITIF
                      MAXLSN=-1*R8MAEM()
                      CALL JELIRA(JEXNUM(CNXINV,NNGL),'LONMAX',NMASUP,
     &                             K8BID)
                      CALL JEVEUO(JEXNUM(CNXINV,NNGL),'L',JMASUP)
                      DO 210 J=1,NMASUP
                        IMA2 = ZI(JMASUP-1+J)
                        CALL JELIRA(JEXNUM(NOMA//'.CONNEX',IMA2),
     &                     'LONMAX',NNO2,K8BID)
                        DO 220 INO2=1,NNO2
                          NNGL=ZI(JCONX1-1+ZI(JCONX2+IMA2-1)+INO2-1)
                          LSN = ZR(JLSN-1+NNGL)
                          IF (LSN.GT.MAXLSN) MAXLSN=LSN
 220                    CONTINUE
 210                  CONTINUE

                      IF (MAXLSN.GT.0) NBCOUP=NBCOUP+1
                    ENDIF
 200              CONTINUE
C --- ON REGARDE SI LE NOMBRE DE NOEUDS COUPÉES NBCOUP DEFINIT UNE FACE
                  IF (NDIM.EQ.2) THEN
                    IF (NBCOUP.EQ.2) LCONT=.TRUE.
                  ELSE
                    CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
                    IF (TYPMA(1:5).EQ.'TETRA') THEN
                      IF (NBCOUP.EQ.3) LCONT=.TRUE.
                    ELSEIF (TYPMA(1:4).EQ.'PYRA') THEN
                      NNGL=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+5-1)
                      LSN = ZR(JLSN-1+NNGL)
                      IF (LSN.EQ.0.AND.NBCOUP.EQ.3.OR.NBCOUP.EQ.4)
     &                   LCONT=.TRUE.
                    ELSEIF (TYPMA(1:5).EQ.'PENTA') THEN
                      NBCOU2=0
                      DO 300 INO=1,3
                        NNGL=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INO-1)
                        LSN = ZR(JLSN-1+NNGL)
                        IF (LSN.EQ.0) THEN
                          NBCOU2 = NBCOU2+1
                        ENDIF
 300                  CONTINUE
                      IF ((NBCOU2.EQ.3.OR.NBCOU2.EQ.0).AND.NBCOUP.EQ.3
     &                   .OR.NBCOUP.EQ.4) LCONT=.TRUE.
                    ELSEIF (TYPMA(1:4).EQ.'HEXA') THEN
                      IF (NBCOUP.EQ.4) LCONT=.TRUE.
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C --- CRITERE SUPLEMENTAIRE POUR LE GRP CTIP, ON DESACTIVE LE CONTACT
C --- SI LE MIN DE LST AUX PTS D'INTERSECTIONS DE L'ÉLÉMENT EST POSITIF
C
              IF (KK.EQ.2.AND.LCONT) THEN
                MINLST=R8MAEM()
                CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
                CALL CONARE(TYPMA,AR,NBAR)
                DO 400 IA=1,NBAR
                  NUNOA=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+AR(IA,1)-1)
                  NUNOB=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+AR(IA,2)-1)
                  LSNA=ZR(JLSN-1+NUNOA)
                  LSNB=ZR(JLSN-1+NUNOB)
                  LSTA=ZR(JLST-1+NUNOA)
                  LSTB=ZR(JLST-1+NUNOB)
                  IF (LSNA.EQ.0.D0.OR.LSNB.EQ.0.D0) THEN
                    IF (LSNA.EQ.0.D0.AND.LSTA.LT.MINLST) MINLST=LSTA
                    IF (LSNB.EQ.0.D0.AND.LSTB.LT.MINLST) MINLST=LSTB
                  ELSEIF((LSNA*LSNB).LT.0.D0) THEN
                    DO 410 K=1,NDIM
                      A(K)=ZR(JCOOR-1+3*(NUNOA-1)+K)
                      B(K)=ZR(JCOOR-1+3*(NUNOB-1)+K)
                      AB(K)=B(K)-A(K)
                      C(K)=A(K)-LSNA/(LSNB-LSNA)*AB(K)
                      AC(K)=C(K)-A(K)
 410                CONTINUE
                    CALL ASSERT(DDOT(NDIM,AB,1,AB,1).GT.R8PREM())
                    LSTC = LSTA + (LSTB-LSTA) * DDOT(NDIM,AB,1,AC,1)
     &                                    / DDOT(NDIM,AB,1,AB,1)
                    IF (LSTC.LT.MINLST) THEN
                      MINLST=LSTC
                      DO 420 K=1,NDIM
                        CMIN(K)=C(K)
 420                  CONTINUE
                    ENDIF
                  ENDIF
 400            CONTINUE
                IF (MINLST.GE.0) LCONT =.FALSE.
                IF (LCONT) THEN
C --- ON VERIFIE LA TOLERANCE AVEC LES PT DE FOND DE FISSURE
                  CALL CONFAC(TYPMA,IBID3,IBID,FA,NBF)
C     ON SE RECREE UN ENVIRONNEMENT COMME DANS UN TE
C                 POUR LSN, LST ET IGEOM
C                 AFIN DE POUVOIR APPELER INTFAC
                  CALL WKVECT('&&XTYELE.LSN','V V R',NNO,ILSN)
                  CALL WKVECT('&&XTYELE.LST','V V R',NNO,ILST)
                  CALL WKVECT('&&XTYELE.IGEOM','V V R',NNO*NDIM,IGEOM)
                  DO 430 INO=1,NNO
                    NNGL=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INO-1)
                    ZR(ILSN-1+INO) = ZR(JLSN-1+NNGL)
                    ZR(ILST-1+INO) = ZR(JLST-1+NNGL)
                    DO 440 J=1,NDIM
                      ZR(IGEOM-1+NDIM*(INO-1)+J) =
     &                ZR(JCOOR-1+3*(NNGL-1)+J)
 440                CONTINUE
 430              CONTINUE
C --- BOUCLE SUR LES FACES
                  DO 450 IFQ=1,NBF
                    CALL INTFAC(IFQ,FA,NNO,ZR(ILST),ZR(ILSN),NDIM,'NON',
     &                          IBID,IBID,IGEOM,M,RBID,RBID,CODRET)
                    IF (CODRET.EQ.1) THEN
C     LONGUEUR CARACTERISTIQUE
                      DO 460 J=1,NDIM
                        A(J) =  ZR(IGEOM-1+NDIM*(FA(IFQ,1)-1)+J)
                        B(J) =  ZR(IGEOM-1+NDIM*(FA(IFQ,2)-1)+J)
                        C(J) =  ZR(IGEOM-1+NDIM*(FA(IFQ,3)-1)+J)
 460                  CONTINUE
                      LONGAR=(PADIST(NDIM,A,B)+PADIST(NDIM,A,C))/2.D0
                      IF (PADIST(NDIM,M,CMIN).LT.(LONGAR*1.D-6))
     &                  LCONT =.FALSE.
                    ENDIF
 450              CONTINUE
                  CALL JEDETR('&&XTYELE.LSN')
                  CALL JEDETR('&&XTYELE.LST')
                  CALL JEDETR('&&XTYELE.IGEOM')
                ENDIF
              ENDIF
 110          CONTINUE
C
C --- POUR CHAQUE MAILLE DE CE GRP, REMPLIT LA COLONNE KK
C --- -1 -> X-FEM SANS CONTACT
C ---  1 -> X-FEM AVEC CONTACT
C ---  0 -> FEM SI LA COLONE 4 EST À 1,
C           NON AFFECTÉ SI LA COLONE 4 EST À 0
C
              IF (ZI(JTAB-1+5*(IMA-1)+4).EQ.1) THEN
                IF (LCONT) THEN
                  ZI(JTAB-1+5*(IMA-1)+KK) = 1
                ELSE
                  ZI(JTAB-1+5*(IMA-1)+KK) = -1
                ENDIF
                ZI(JTAB-1+5*(IMA-1)+4)  = 0
              ELSEIF (ZI(JTAB-1+5*(IMA-1)+4).EQ.0) THEN
C --- SI LA MAILLE EST VUE UNE DEUXIEME FOIS,
C
                IF (CONTAC.GT.1) CALL U2MESK('F','XFEM_43', 1 ,NOMAIL)
C --- SI CONTACT AUTRE QUE P1P1
                IF (KK.GT.1.OR.ABS(ZI(JTAB-1+5*(IMA-1)+2)).EQ.1
     &                      .OR.ABS(ZI(JTAB-1+5*(IMA-1)+3)).EQ.1) THEN
C --- SI UNE DES MAILLES CONTIENT DU CRACK-TIP
                  CALL U2MESK('F','XFEM_44', 1 ,NOMAIL)
                ENDIF
C
C --- CALCUL DU NOMBRE DE FONCTIONS HEAVISIDE
                CALL XTYHEA(NFISS,IFISS,IMA,NNO,JCONX1,JCONX2,
     &                      JSTNL,JSTNV,NBHEAV)
                IF (NBHEAV.GT.4) CALL U2MESK('F','XFEM_40', 1 ,NOMAIL)
                ZI(JNBPT2-1+IMA) = NBHEAV
                IF (ZI(JTAB-1+5*(IMA-1)+1).GT.0.OR.LCONT) THEN
C --- SI AU MOINS UNE DES 2 MAILLES A DU CONTACT
                  ZI(JTAB-1+5*(IMA-1)+KK) = NBHEAV
                ELSE
                  ZI(JTAB-1+5*(IMA-1)+KK) = -1*NBHEAV
                ENDIF
                LINTER = .TRUE.
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
 30         CONTINUE
          ENDIF
 20     CONTINUE
        CALL JEDETR(CLSN)
        CALL JEDETR(CLST)
 10   CONTINUE
C
      DO 50 IFISS=1,NFISS
        CALL JEDETR(CSTN(IFISS))
 50   CONTINUE
C
      CALL JEDETR(CNXINV)
C
      CALL JEDEMA()
      END
