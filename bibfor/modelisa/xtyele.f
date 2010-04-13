      SUBROUTINE XTYELE(NOMA,MODELX,TRAV,NFISS,FISS,CONTAC,NDIM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/04/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      CHARACTER*8  NOMA,MODELX
      CHARACTER*24 TRAV
      INTEGER     NFISS
      CHARACTER*8  FISS(NFISS)
      INTEGER     CONTAC,NDIM
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
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
      CHARACTER*32 JEXNUM,JEXATR
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      REAL*8      R8MAEM,R8PREM,MINLSN,MINLST,MAXLSN,LSN,LST
      REAL*8      DDOT,LSNA,LSTA,LSNB,LSTB,LSTC
      REAL*8      A(NDIM),B(NDIM),AB(NDIM),C(NDIM),AC(NDIM)
      INTEGER     NMAENR,JINDIC,KK,JGRP,JCOOR,NBMA,JVALV,JNCMP
      INTEGER     JLSN,JLST,JMASUP,JTMDIM,JTYPMA,JCONX1,JCONX2
      INTEGER     NBCOUP,NBCOU2,IBID,IFISS,ITYPMA,JTAB
      INTEGER     NMASUP,NDIME,NBAR
      INTEGER     INO,INO2,NNGL,NNOT(3),NNO,NNO2,IMA,IMA2
      INTEGER     I,J,K
      INTEGER     AR(12,2),IA,NUNOA,NUNOB
      CHARACTER*8  TYPMA,K8BID
      CHARACTER*19 CARTE,CLSN,CLST,CNXINV
      CHARACTER*24 XINDIC,GRP(3)
      LOGICAL     LCONT
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATION
C
      CLSN = '&&XTYELE.LSN'
      CLST = '&&XTYELE.LST'
      CALL JEVEUO(NOMA(1:8)//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO('&CATA.TM.TMDIM','L',JTMDIM)
      CALL JEVEUO(NOMA(1:8)//'.TYPMAIL','L',JTYPMA)
C
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IBID)
      CALL JEVEUO(NOMA(1:8)//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',JCONX2)
      CNXINV = '&&XTYELE.CNCINV'
      CALL CNCINV(NOMA,IBID,0,'V',CNXINV)

C     CREATION D'UNE CARTE CONTENANT LES NOMS DES SD FISS_XFEM
      CARTE = MODELX(1:8)//'.XMAFIS'
      CALL ALCART('G', CARTE, NOMA, 'NEUT_K8')
      CALL JEVEUO(CARTE//'.NCMP', 'E', JNCMP )
      CALL JEVEUO(CARTE//'.VALV', 'E', JVALV )
      ZK8(JNCMP)   = 'Z1'
C     RECUPERATION DE L'ADRESSE DU TABLEAU DE TRAVAIL
      CALL JEVEUO(TRAV,'E',JTAB)
C
C --- BOUCLE SUR NOMBRE OCCURRENCES FISSURES
C
      DO 10 IFISS = 1,NFISS
        CALL CNOCNS(FISS(IFISS)//'.LNNO','V',CLSN)
        CALL CNOCNS(FISS(IFISS)//'.LTNO','V',CLST)
        CALL JEVEUO(CLSN//'.CNSV','L',JLSN)
        CALL JEVEUO(CLST//'.CNSV','L',JLST)
        GRP(1) = FISS(IFISS)//'.MAILFISS  .HEAV'
        GRP(2) = FISS(IFISS)//'.MAILFISS  .CTIP'
        GRP(3) = FISS(IFISS)//'.MAILFISS  .HECT'
        XINDIC = FISS(IFISS)//'.MAILFISS .INDIC'
        CALL JEVEUO(XINDIC,'L',JINDIC)  
C
C --- BOUCLE SUR LES GRP
C
        DO 20 KK = 1,3
          IF (ZI(JINDIC-1+2*(KK-1)+1).EQ.1) THEN
            CALL JEVEUO(GRP(KK),'L',JGRP)
            NMAENR = ZI(JINDIC-1+2*KK)
C
C --- BOUCLE SUR LES MAILLES DU GROUPE
C
            DO 30 I = 1,NMAENR
              IMA = ZI(JGRP-1+I)
C
C --- ON DETERMINE S'IL S'AGIT D'UNE MAILLE DE CONTACT OU PAS
              LCONT = .FALSE.
C
C --- SI LE CONTACT EST DECLARÉ DANS LE MODELE
C
              IF (CONTAC.GE.1) THEN
C --- PAS DE CONTACT POUR LES MAILLE DE BORD
                ITYPMA=ZI(JTYPMA-1+IMA)
                NDIME= ZI(JTMDIM-1+ITYPMA)
                IF (NDIME.NE.NDIM) GOTO 110
C --- ON RECUPERE LE NB DE NOEUDS SOMMETS DE LA MAILLE
                CALL PANBNO(ITYPMA,NNOT)
                NNO = NNOT(1)
                MAXLSN=-1*R8MAEM()
                MINLSN=R8MAEM()
C --- BOUCLE SUR LES NOEUDS DE LA MAILLE
                DO 100 INO=1,NNO
                  NNGL=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INO-1)
                  LSN = ZR(JLSN-1+NNGL)
                  IF (LSN.LT.MINLSN) MINLSN=LSN
                  IF (LSN.GT.MAXLSN) MAXLSN=LSN
 100            CONTINUE
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
                    ELSEIF (TYPMA(1:4).EQ.'PENTA') THEN
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
                    IF (LSTC.LT.MINLST) MINLST=LSTC
                  ENDIF
 400            CONTINUE
                IF (MINLST.GE.0) LCONT =.FALSE.
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
              ELSE
C --- SI LA MAILLE EST VUE UNE DEUXIEME FOIS, LES FISSURES SONT TROP
C --- PROCHES, ERREUR CAR L'INTERSECTION N'EST PAS IMPLEMENTE
                CALL U2MESS('F','XFEM_1')
C --- ON DETECTERA ICI LES MAILLES SPECIFIQUES A L'INTERSECTION
C                ZI(JTAB-1+5*(IMA-1)+1) = 0
C                ZI(JTAB-1+5*(IMA-1)+2) = 0
C                ZI(JTAB-1+5*(IMA-1)+3) = 0
              ENDIF
              ZK8(JVALV) = FISS(IFISS)
              CALL NOCART ( CARTE,3,' ','NUM',1,' ',IMA,' ',1)
 30         CONTINUE
          ENDIF
 20     CONTINUE
        CALL JEDETR(CLSN)
        CALL JEDETR(CLST)
 10   CONTINUE
C
      CALL JEDETR(CNXINV)
C
      CALL JEDEMA()
      END
