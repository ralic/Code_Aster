      SUBROUTINE XDELDL(MOD,MA,GRMAEN,JSTANO,LISREL,NREL)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 25/01/2005   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE

      INTEGER      JSTANO,NREL
      CHARACTER*8  MOD,MA 
      CHARACTER*19 LISREL
      CHARACTER*24 GRMAEN 

C     BUT: SUPPRIMER LES DDLS "EN TROP" (VOIR BOOK III 09/06/04
C                                         ET  BOOK IV  30/07/07)

C ARGUMENTS D'ENTRÉE:

C      MAILLE  : NOM DE LA COMMANDE
C      CHAR    : NOM UTILISATEUR DU RESULTAT DE CHARGE

C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C-----------------------------------------------------------------------
C---------------- DECLARATION DES VARIABLES LOCALES  -------------------

      REAL*8       RBID,BETAR,COEFR(2)
      INTEGER      IER,NBNO,JMA,LONG,IMA,NMAABS,NBNOMA,JCONX1,JCONX2
      INTEGER      INO,NUNO,ISTATU,K,IRET,NDIM(2),JPRNM,NBEC,NBCMP,INOM
      INTEGER      ICMP,INDIK8,NNOS
      CHARACTER*1  K1BID
      CHARACTER*8  K8BID,NOMNO,DDLM(3),DDLC(3),DDLH(3),DDLE(12),DDL(2)
      CHARACTER*8  NOEUD(2),NOMG
      CHARACTER*19 LIGRMO
      COMPLEX*16   CBID
      LOGICAL      EXISDG
      DATA         DDLM / 'DX'  , 'DY'  , 'DZ'  /
      DATA         DDLC / 'DCX' , 'DCY' , 'DCZ' /
      DATA         DDLH / 'H1X' , 'H1Y' , 'H1Z' /
      DATA         DDLE / 'E1X' , 'E1Y' , 'E1Z' , 'E2X' , 'E2Y' , 'E2Z',
     &                    'E3X' , 'E3Y' , 'E3Z' , 'E4X' , 'E4Y' , 'E4Z'/
      DATA         BETAR / 0.D0 /
C-------------------------------------------------------------

      CALL JEMARQ()

      CALL JEEXIN(GRMAEN,IER)
      IF (IER.EQ.0) GOTO 9999

      CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNO,K8BID,IRET)
      CALL JEVEUO(MA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(MA//'.CONNEX','LONCUM'),'L',JCONX2)

      CALL JEVEUO(GRMAEN,'L',JMA)
      CALL JELIRA(GRMAEN,'LONMAX',LONG,K8BID)

C     RÉCUPÉRATION DES DONNÉES SERVANT À VÉRIFIER L'EXISTENCE D'UN DDL
C     POUR UN NOEUD (COMME DANS AFLRCH.F)
      LIGRMO = MOD//'.MODELE'
      NOMG='DEPL_R'
      CALL DISMOI('F','NB_EC',NOMG,'GRANDEUR',NBEC,K8BID,IER)
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMG),'L',INOM)
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMG),'LONMAX',NBCMP,K1BID)
      CALL JEVEUO(LIGRMO//'.PRNM','L',JPRNM)
      ICMP = INDIK8(ZK8(INOM),'DX',1,NBCMP)

C     BOUCLE SUR LES MAILLES
      DO 100 IMA=1,LONG

C       NUMERO ABS DE LA MAILLE TRAITÉE
        NMAABS=ZI(JMA-1+IMA)
        NBNOMA=ZI(JCONX2+NMAABS) - ZI(JCONX2+NMAABS-1)

C       ATTENTION : COMME MAINTENANT, ON A DES MAILLES À 10,15,20 NOEUDS
C       IL FAUT BOUCLER SEULEMENT SUR LES NOEUDS SOMMETS : NNOS
        NNOS=2*NBNOMA/5

        DO 110 INO=1,NNOS

          NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INO-1)
          CALL JENUNO(JEXNUM(MA//'.NOMNOE',NUNO),NOMNO)
          ISTATU=ZI(JSTANO-1+NUNO)

          IF (GRMAEN(21:24).EQ.'HEAV') THEN

C           1) CAS DES MAILLES 'ROND'
C           -------------------------

            IF (ISTATU.GT.1) THEN
            CALL UTMESS('F','XDELDL','PB DE STATUT DES NOEUDS ENRICHIS')
            ELSEIF (ISTATU.EQ.1) THEN
C             ON NE SUPPRIME AUCUN DDL  
            ELSEIF (ISTATU.EQ.0) THEN
C             ON SUPPRIME LES DDLS H
              DO 120 K=1,3         
                CALL AFRELA (1.D0,CBID,DDLH(K),NOMNO,0,RBID,1,BETAR,
     +                           CBID,K8BID,'REEL','REEL','12',LISREL)
                NREL=NREL+1
 120          CONTINUE
            ENDIF

          ELSEIF (GRMAEN(21:24).EQ.'CTIP') THEN

C           2) CAS DES MAILLES 'CARRÉ'
C           --------------------------

            IF (ISTATU.GT.2.OR.ISTATU.EQ.1) THEN
            CALL UTMESS('F','XDELDL','PB DE STATUT DES NOEUDS ENRICHIS')
            ELSEIF (ISTATU.EQ.2) THEN
C             ON NE SUPPRIME AUCUN DDL  
            ELSEIF (ISTATU.EQ.0) THEN
C             ON SUPPRIME LES DDLS E
              DO 130 K=1,12    
                CALL AFRELA (1.D0,CBID,DDLE(K),NOMNO,0,RBID,1,BETAR,
     +                           CBID,K8BID,'REEL','REEL','12',LISREL)
                NREL=NREL+1
 130          CONTINUE
            ENDIF

          ELSEIF (GRMAEN(21:24).EQ.'HECT') THEN

C           3) CAS DES MAILLES 'ROND-CARRÉ'
C           ------------------------------

            IF (ISTATU.GT.3) THEN
            CALL UTMESS('F','XDELDL','PB DE STATUT DES NOEUDS ENRICHIS')
            ELSEIF (ISTATU.EQ.3) THEN
C             ON NE SUPPRIME AUCUN DDL  
            ELSEIF (ISTATU.EQ.2) THEN
C             ON SUPPRIME LES DDLS H 
              DO 140 K=1,3         
                CALL AFRELA (1.D0,CBID,DDLH(K),NOMNO,0,RBID,1,BETAR,
     +                           CBID,K8BID,'REEL','REEL','12',LISREL)
                NREL=NREL+1
 140          CONTINUE
            ELSEIF (ISTATU.EQ.1) THEN
C             ON SUPPRIME LES DDLS E 
              DO 150 K=1,12 
                CALL AFRELA (1.D0,CBID,DDLE(K),NOMNO,0,RBID,1,BETAR,
     +                           CBID,K8BID,'REEL','REEL','12',LISREL)
                NREL=NREL+1
 150          CONTINUE
            ELSEIF (ISTATU.EQ.0) THEN
C             ON SUPPRIME LES DDLS H ET E
              DO 160 K=1,3         
                CALL AFRELA (1.D0,CBID,DDLH(K),NOMNO,0,RBID,1,BETAR,
     +                           CBID,K8BID,'REEL','REEL','12',LISREL)

                NREL=NREL+1
 160          CONTINUE
              DO 170 K=1,12
                CALL AFRELA (1.D0,CBID,DDLE(K),NOMNO,0,RBID,1,BETAR,
     +                           CBID,K8BID,'REEL','REEL','12',LISREL)
                NREL=NREL+1
 170          CONTINUE
            ENDIF

          ELSE
            CALL UTMESS('F','XDELDL','PB DE GROUPE MAILLES ENRICHIES')
          ENDIF

C         POUR LES NOEUDS OÙ LE STATUT VAUT 0, IL FAUT IMPOSER
C         DX = DCX , DY = DCY ET DZ = DCZ SOUS RÉSERVE QUE CES NOEUDS
C         PORTENT BIEN LES DDLS DX, DY ET DZ (ON VÉRIFIE QUE 'DX')
          IF (ISTATU.EQ.0) THEN
            IF (EXISDG(ZI(JPRNM-1+(NUNO-1)*NBEC+1),ICMP)) THEN
              DO 180 K=1,3
                DDL(1)=DDLM(K)
                DDL(2)=DDLC(K)
                COEFR(1)=1
                COEFR(2)=-1
                NOEUD(1)=NOMNO
                NOEUD(2)=NOMNO
                NDIM(1)=0
                NDIM(2)=0
                CALL AFRELA (COEFR,CBID,DDL,NOEUD,NDIM,RBID,2,BETAR,
     +                           CBID,K8BID,'REEL','REEL','12',LISREL)
                NREL=NREL+1
 180          CONTINUE
            ENDIF
          ENDIF

 110    CONTINUE

 100  CONTINUE
      
9999  CONTINUE
      CALL JEDEMA()
      END
