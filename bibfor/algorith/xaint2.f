      SUBROUTINE XAINT2(NOMA,MODELE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8   MODELE,NOMA
C      
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (PREPARATION)
C
C ON MODIFIE DE TOPOAC.AI
C
C LORS DU TRAITEMENT DU CONTACT SUR LES BRANCHEMENTS DE FISSURES,
C SI LES FACETTES DU SUPPORT D'UN NOEUD D'UNE ARETE COUPÉE SONT
C ASSOCIÉES À DES FONCTIONS HEAVISIDE DIFFÉRENTES: IL Y A CONFLIT :
C ON DÉSACTIVE CE NOEUD :
C       +-----/
C       |    /|
C       |   / |
C       |  /  |
C +-----*-/---+           *NOEUD DÉSACTIVÉ
C |     |/    |
C |     / \   |
C |    /|  \  |
C +---/-+---\-+
C    /       \
C   FISS1     FISS2
C
C ON FAIT CETTE OPÉRATION EN DEHORS DU TE0510 CAR IL FAUT COMPARER LES
C FACETTES DU SUPPORT DU NOEUD.
C
C ----------------------------------------------------------------------
C
C
C  IN  NOMA   : NOM DE L'OBJET MAILLAGE
C  I/O MODELE   : NOM DE LA SD MODELE_XFEM
C
C
C
C
      CHARACTER*24  GRP(3),INDIC
      CHARACTER*19  CES(5),CEL(5),CNXINV,LIGREL
      CHARACTER*8   K8BID,TYPMA,NOMFIS
      CHARACTER*2   CH2
      REAL*8        CRIT,VMOIN,VPLUS,VTOT
      INTEGER       JTYPMA,JCESD(5),JCESL(5),JCESV(5),IAD
      INTEGER       JXC,ITYPMA,NNCP,IBID,IER
      INTEGER       JNBSP,JNBSP2
      INTEGER       JCONX1,JCONX2,JTMDIM,NDIME,NDIM
      INTEGER       NUNO(2),NUNO2(2),INO(2),INO2,IMA,IMA2
      INTEGER       JMAIL,NFIS,IFIS
      INTEGER       I,J,K,NHEAV,IHEAV,NFISS,IFISS,NFIS2,IFIS2,IFIS3
      LOGICAL       ELIM(2),VERIF
      INTEGER       IGRP,NBMA,JNFIS,JMOFIS,NMAENR,JINDIC,JG
      INTEGER       NFACE,NINTER,INTER,ZXAIN,IA,IFH,NFH,NMASUP,JMASUP
      INTEGER       HEAV,HE,XXMMVD,AR(12,3),NBAR,NNO2,NNGL,INTE2,NINTE2

C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ON VERIFIE QUE L'ON A CONTACT P1P1
      CALL JEVEUO(MODELE//'.XFEM_CONT','L',JXC)
      IF (ZI(JXC).NE.1) GOTO 999
C
      ZXAIN  =XXMMVD('ZXAIN')

C --- RECUPERATION DES DONNEES SUR LE MAILLAGE
C      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8BID,IBID)
C      CALL JEVEUO(MODELE//'.MAILLE','L',JMAIL)
      CALL JEVEUO('&CATA.TM.TMDIM','L',JTMDIM)
      CALL DISMOI('F','DIM_GEOM',NOMA,'MAILLAGE',NDIM,K8BID,IBID)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IBID)
      CALL JEVEUO(NOMA//'.TYPMAIL','L',JTYPMA)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)

C --- CONNECTIVITE INVERSEE
      CNXINV = '&&XAINT2.CNCINV'
      CALL CNCINV(NOMA,IBID,0,'V',CNXINV)
      LIGREL = MODELE//'.MODELE'
C
C --- RECUPERATION DES DONNEES ELEMENTAIRES XFEM
C
      CALL JEVEUO('&&XTYELE.NBSP' ,'L',JNBSP)
      CEL(1) = MODELE//'.TOPOFAC.LO'
      CEL(2) = MODELE//'.TOPOFAC.AI'
      CEL(3) = MODELE//'.STNO'
      CEL(4) = MODELE//'.FISSNO'
      CEL(5) = MODELE//'.TOPOFAC.HE'
      DO 10 I=1,5
        CALL CODENT(I,'G',CH2)
        CES(I) = '&&XAINT2.CES'//CH2
        CALL JEEXIN(CEL(I)//'.CELD',IER)
        IF (IER.EQ.0) GOTO 10
        CALL CELCES(CEL(I),'V',CES(I))
        CALL JEVEUO(CES(I)//'.CESD','L',JCESD(I))
        CALL JEVEUO(CES(I)//'.CESL','L',JCESL(I))
        CALL JEVEUO(CES(I)//'.CESV','E',JCESV(I))
  10  CONTINUE
C
C --- COMPTEUR LOCAL DES FISSURES DANS LES ÉLÉMENTS
C
      CALL WKVECT('&&XAIN2.NBSP' ,'V V I',NBMA,JNBSP2)
C
C     BOUCLE SUR LES FISSURES
C
      CALL JEVEUO(MODELE//'.NFIS','L',JNFIS)
      NFIS = ZI(JNFIS)
      CALL JEVEUO(MODELE//'.FISS','L',JMOFIS)
      DO 20 IFIS = 1,NFIS
        NOMFIS = ZK8(JMOFIS-1 + IFIS)
        GRP(1)=NOMFIS//'.MAILFISS  .HEAV'
        GRP(2)=NOMFIS//'.MAILFISS  .CTIP'
        GRP(3)=NOMFIS//'.MAILFISS  .HECT'
        INDIC=NOMFIS//'.MAILFISS .INDIC'
        CALL JEVEUO(INDIC,'L',JINDIC)
C      BOUCLE SUR LES GROUPES
        DO 100 IGRP = 1,3
          IF (ZI(JINDIC-1+2*(IGRP-1)+1).EQ.1) THEN
            CALL JEVEUO(GRP(IGRP),'L',JG)
            NMAENR=ZI(JINDIC-1+2*IGRP)
C      BOUCLE SUR LES MAILLES DU GROUPE
            DO 120 I=1,NMAENR
              IMA   = ZI(JG-1+I)
C      ON INCRÉMENTE LE COMPTEUR LOCAL
              ZI(JNBSP2-1+IMA) = ZI(JNBSP2-1+IMA)+1
 120        CONTINUE
          ENDIF
 100    CONTINUE
C      BOUCLE SUR LES GROUPES
        DO 200 IGRP = 1,3
          IF (ZI(JINDIC-1+2*(IGRP-1)+1).EQ.1) THEN
            CALL JEVEUO(GRP(IGRP),'L',JG)
            NMAENR=ZI(JINDIC-1+2*IGRP)
C      BOUCLE SUR LES MAILLES DU GROUPE
            DO 220 I=1,NMAENR
              IMA   = ZI(JG-1+I)
              ITYPMA=ZI(JTYPMA-1+IMA)
              NDIME= ZI(JTMDIM-1+ITYPMA)
              IF (NDIME.LT.NDIM) GOTO 220
              IFISS = ZI(JNBSP2-1+IMA)
              CALL CESEXI('S',JCESD(1),JCESL(1),IMA,1,IFISS,2,IAD)
              NFACE = ZI(JCESV(1)-1+IAD)
              CALL CESEXI('S',JCESD(1),JCESL(1),IMA,1,IFISS,1,IAD)
              NINTER = ZI(JCESV(1)-1+IAD)
              NFISS = ZI(JNBSP-1+IMA)
C      ON NE TRAITE QUE LES ELEMENTS MULTI-HEAVISIDE COUPÉS
              IF (NFISS.LE.1.OR.NINTER.EQ.0) GOTO 220
              IF (NFACE.EQ.0) THEN
C      SI PAS DE FACETTES, ON VERIFIE TOUTES LES ARETES
                VERIF = .FALSE.
              ELSE
C      SINON ON VERIFIE UNIQUEMENT LES ARETES NÉGATIVE 
                VERIF = .TRUE.
              ENDIF

              CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
              CALL CONARE(TYPMA,AR,NBAR)
              DO 230 INTER=1,NINTER
C       BOUCLE SUR LES ARETES DE L'ÉLÉMENT CONTENANT LA JONCTION
                CALL CESEXI('S',JCESD(2),JCESL(2),IMA,1,IFISS,
     &                                            (INTER-1)*ZXAIN+1,IAD)
                IA = NINT(ZR(JCESV(2)-1+IAD))
                IF (IA.EQ.0.OR.IA.GT.0.AND.VERIF) GOTO 230
                IA = ABS(IA)
C       RÉCUP DES NOEUDS J DE L'ARETE
                DO 232 J = 1,2
                  INO(J)  = AR(IA,J)
                  NUNO(J) = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INO(J)-1)
                  ELIM(J) = .FALSE.
 232            CONTINUE

                DO 235 J = 1,2
C       RECUPERATION DU NOMBRE DE DDL HEAVISIDES ACTIFS SUR LE NOEUD J
                  NFH = 0
                  DO 240 IFIS2 = 1,NFISS
                    CALL CESEXI('S',JCESD(3),JCESL(3),IMA,INO(J),
     &                                                      IFIS2,1,IAD)
                    IF (ZI(JCESV(3)-1+IAD).EQ.1) NFH = NFH + 1
 240              CONTINUE
                  CALL ASSERT(NFH.GT.1)
C
C     RECUPÉRATION DE LA CONNECTIVITÉ DU NOEUD J
C        
                  CALL JELIRA(JEXNUM(CNXINV,NUNO(J)),'LONMAX',NMASUP,
     &                               K8BID)
                  CALL JEVEUO(JEXNUM(CNXINV,NUNO(J)),'L',JMASUP)
                  HEAV = 0
                  DO 250 K=1,NMASUP
                    IMA2 = ZI(JMASUP-1+K)
                    ITYPMA=ZI(JTYPMA-1+IMA2)
                    NDIME= ZI(JTMDIM-1+ITYPMA)
                    IF (NDIME.LT.NDIM) GOTO 250
                    IFIS2 = ZI(JNBSP2-1+IMA2)
                    NFIS2 = ZI(JNBSP-1+IMA2)
                    CALL CESEXI('S',JCESD(1),JCESL(1),IMA2,1,IFIS2,2,
     &                                                              IAD)
C      SI PAS DE FACETTE DANS CETTE MAILLE POUR CETTE FISSURE, ON SORT
                    IF (ZI(JCESV(1)-1+IAD).EQ.0) GOTO 250
C      RECUPÉRATION DU NUMÉRO DE NOEUD INO2 CORRESPONDANT À J DANS IMA2
                    CALL JELIRA(JEXNUM(NOMA//'.CONNEX',IMA2),
     &                     'LONMAX',NNO2,K8BID)
                    DO 260 INO2=1,NNO2
                      NNGL=ZI(JCONX1-1+ZI(JCONX2+IMA2-1)+INO2-1)
                      IF (NNGL.EQ.NUNO(J)) GOTO 270
 260                CONTINUE
 270                CONTINUE
C
C     CALCUL DE LA VALEUR DE LA FONCTION HEAVISIDE EN TRINAIRE :
C     HE =(-1,0 OU 1) EN IFH DONNE (0,1 OU 2) POUR LA IFH-1 TRICIMALE
C
                    HE = 0
                    DO 280 IFH = 1,NFH
C     NUMÉRO DE FISSURE ASSOCIÉ À NUNO(J) ET IFH DANS IMA2
                      CALL CESEXI('S',JCESD(4),JCESL(4),IMA2,INO2,IFH,1,
     &                             IAD)
                      IFIS3 = ZI(JCESV(4)-1+IAD)
C     RECUPÉRATION DE LA FONCTION HEAVISIDE (ESCLAVE ET MAITRE)
                      CALL CESEXI('S',JCESD(5),JCESL(5),IMA2,1,
     &                                      NFIS2*(IFIS2-1)+IFIS3,1,IAD)
                      HE = HE + (1+ZI(JCESV(5)-1+IAD))*3**(IFH-1)

                      CALL CESEXI('S',JCESD(5),JCESL(5),IMA2,1,
     &                                      NFIS2*(IFIS2-1)+IFIS3,2,IAD)
                      HE = HE + (1+ZI(JCESV(5)-1+IAD))*3**(NFH+IFH-1)
 280                CONTINUE
                    IF (HEAV.EQ.0) HEAV = HE
C     SI LA FONC HEAV EST DIFF D'UN ELEM À L'AUTRE, ON DÉSATCTIVE LE NO
                    IF (HEAV.NE.HE) ELIM(J) = .TRUE.
 250              CONTINUE

 235            CONTINUE
C     ELIM = TT <=> L'ARETE DOIT ÊTRE ENTIÈREMENT ÉLIMINÉE
C     SINON IL FAUDRA RECONSIDÉRER PROPREMENT L'ELIM COMPLÈTE DE L'ARETE
                CALL ASSERT(VERIF.EQV.(ELIM(1).AND.ELIM(2)))
                DO 300 J = 1,2
                  IF (.NOT.ELIM(J)) GOTO 300
C
C     RECUPÉRATION DE LA CONNECTIVITÉ DU NOEUD J
C        
                  CALL JELIRA(JEXNUM(CNXINV,NUNO(J)),'LONMAX',NMASUP,
     &                               K8BID)
                  CALL JEVEUO(JEXNUM(CNXINV,NUNO(J)),'L',JMASUP)

                  DO 350 K=1,NMASUP
C       BOUCLE SUR LES ELEM CONEXES
                    IMA2 = ZI(JMASUP-1+K)
                    ITYPMA=ZI(JTYPMA-1+IMA2)
                    NDIME= ZI(JTMDIM-1+ITYPMA)
                    IF (NDIME.LT.NDIM) GOTO 350
                    CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
                    CALL CONARE(TYPMA,AR,NBAR)
                    IFIS2 = ZI(JNBSP2-1+IMA2)
                    CALL CESEXI('S',JCESD(1),JCESL(1),IMA2,1,IFIS2,1,
     &                                                              IAD)
                    NINTE2 = ZI(JCESV(1)-1+IAD)
                    DO 360 INTE2 = 1,NINTE2
C         BOUCLE SUR LES ARETES AINTER
                      CALL CESEXI('S',JCESD(2),JCESL(2),IMA2,1,IFIS2,
     &                                            (INTE2-1)*ZXAIN+1,IAD)
                      IA = ABS(NINT(ZR(JCESV(2)-1+IAD)))

                      IF (IA.EQ.0) GOTO 360
                      NUNO2(1)=ZI(JCONX1-1+ZI(JCONX2+IMA2-1)+AR(IA,1)-1)
                      NUNO2(2)=ZI(JCONX1-1+ZI(JCONX2+IMA2-1)+AR(IA,2)-1)
C         SI LE NOEUD J N'APPARTIENT PAS À L'ARETE, ON SORT
                      IF (NUNO(J).NE.NUNO2(1).AND.NUNO(J).NE.NUNO2(2))
     &                  GOTO 360
C         MISE À ZÉRO DE L'ARETE
                      ZR(JCESV(2)-1+IAD) = 0
                      IF(NUNO(3-J).NE.NUNO2(1).AND.NUNO(3-J).NE.NUNO2(2)
     &                  .OR.(.NOT.VERIF)) THEN
C         SI L'ARETE N'EST PAS TT, ON LA REPORTE SUR SON NOEUD ACTIF
                        CALL CESEXI('S',JCESD(2),JCESL(2),IMA2,1,IFIS2,
     &                                            (INTE2-1)*ZXAIN+2,IAD)
                        IF (NUNO2(1).EQ.NUNO(J)) THEN
C     ON REPORTE L'ARETE SUR LE NOEUD 2
                          ZR(JCESV(2)-1+IAD) = AR(IA,2)
                        ELSE
C     ON REPORTE L'ARETE SUR LE NOEUD 1
                          ZR(JCESV(2)-1+IAD) = AR(IA,1)
                        ENDIF
                      ENDIF
 360                CONTINUE
 350              CONTINUE
 300            CONTINUE
 230          CONTINUE

 220        CONTINUE
          ENDIF
 200    CONTINUE
  20  CONTINUE
C
C --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM POUR MODELE.TOPOFAC.AI
C
      CALL CESCEL(CES(2),LIGREL,'TOPOFA','PAINTER','OUI',
     &            NNCP,'G',CEL(2),'F',IBID)
C
C --- MENAGE
C
      CALL JEDETR(CNXINV)
      CALL JEDETR('&&XAIN2.NBSP')
      DO 130 I=1,5
        CALL JEEXIN(CES(I)//'.CESD',IER)
        IF (IER.EQ.0) GOTO 130
        CALL DETRSD('CHAM_ELEM_S',CES(I))
 130  CONTINUE

 999  CONTINUE

      CALL JEDEMA()
      END
