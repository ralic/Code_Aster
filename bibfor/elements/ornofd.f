      SUBROUTINE ORNOFD ( RESU, NOMAIL, NBMA, NOEORD, 
     &                    NDORIG, NDEXTR, BASE)
      IMPLICIT   NONE
      INTEGER             NBMA
      CHARACTER*8         RESU,  NOMAIL, NDORIG, NDEXTR 
      CHARACTER*24        NOEORD
      CHARACTER*1         BASE      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/06/2011   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C       ORNOFD -- ORDONNANCEMENT D'UNE LISTE DE NOEUD
C                 A PARTIR D'UN NOEUD ORIGINE 
C                 UTILISE DANS DEFI_GROUP ET DEFI_FOND_FISS
C
C     ENTREES:
C        RESU       : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
C        NOMAIL     : NOM DU MAILLAGE
C        NBMT       : NOMBRE DE MAILLES TRAITEES
C        NOEORD     : NOM DE L'OBJET
C        NDORIG     : NOM DU NOEUD ORIGINE
C        NDEXTR     : NOM DU NOEUD EXTREMITE
C        BASE       : TYPE DE BASE DE SAUVEGARDE
C 
C-----------------------------------------------------------------------
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
      CHARACTER*32       JEXNUM, JEXNOM
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER       IATYMA, JTYPM, JCOUR1, JCOUR2, JMAIL
      INTEGER       IM, NID, NIG, NJONC, N, I, K, NBNO
      INTEGER       JRDM, JNOE
      CHARACTER*6   NOMPRO
      CHARACTER*8   K8B, NOMMA, TYPM
      CHARACTER*8   NOEUD, NOEUD1, NOEUD2, NOEUD3, NOEUD4
      CHARACTER*24  CONEC, TYPP, NOMMAI, NOMNOE
C DEB-------------------------------------------------------------------
      CALL JEMARQ()
      NOMPRO = 'ORNOFD'

C     OBJETS DE TRAVAIL
      CONEC  = NOMAIL//'.CONNEX         '
      TYPP   = NOMAIL//'.TYPMAIL        '
      NOMNOE = NOMAIL//'.NOMNOE         '

C     RECUPERATION DES NOEUDS DESORDONNES
      CALL JEVEUO ( RESU//'.MAILLESFOURNIES', 'L', JMAIL )
      
C     ------------------------------------------------------------------
C     RECUPERATION DU TYPE DE MAILLE 
C     ------------------------------------------------------------------
      CALL JEVEUO ( TYPP, 'L', IATYMA )
      JTYPM = IATYMA-1+ZI(JMAIL-1 + 1)
      CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM)),TYPM)

C     ------------------------------------------------------------------
C     CONSTRUCTION D'UN VECTEUR DE TRAVAIL LOCAL CONTENANT 
C     LES NOEUDS EXTREMITES  DE CHAQUE MAILLE
C     ------------------------------------------------------------------
      CALL WKVECT('&&'//NOMPRO//'.NOEUDS_EXTREM','V V I',2*NBMA,JCOUR2)
      DO 30 IM = 1 , NBMA
         CALL I2EXTF (ZI(JMAIL-1+IM),1,CONEC(1:15),TYPP(1:16),NIG,NID)
         ZI(JCOUR2-1 +     IM) = NIG
         ZI(JCOUR2-1 +NBMA+IM) = NID
 30   CONTINUE
      
C
C     ------------------------------------------------------------------
C     --- ORDONNANCEMENT DES MAILLES EN PARTANT DU NOEUD ORIGINE 
C     ------------------------------------------------------------------
C
      CALL JENONU ( JEXNOM(NOMNOE,NDORIG), NJONC )
      N = 1
C     ------------------------------------------------------------------
C     CONSTRUCTION D'UN VECTEUR DE TRAVAIL LOCAL POUR 
C     TRIER LES NOEUDS ET CONTENANT 
C     LES MAILLES, LES NOEUDS SOMMET 1 ET LES NOEUDS SOMMET 2
C     ------------------------------------------------------------------
      CALL WKVECT('&&'//NOMPRO//'.MAILLES_TRIEE','V V I',3*NBMA,JCOUR1)
C     EQUIVALENT D'UNE BOUCLE WHILE
550   CONTINUE
      DO 552 I=N,NBMA
        IF (ZI(JCOUR2-1 + I).EQ.NJONC) THEN
          ZI(JCOUR1-1 +        N)=ZI(JMAIL-1  +      I)
          ZI(JCOUR1-1 +   NBMA+N)=ZI(JCOUR2-1 +      I)
          ZI(JCOUR1-1 + 2*NBMA+N)=ZI(JCOUR2-1 + NBMA+I)
          NJONC                  =ZI(JCOUR2-1 + NBMA+I)
          GOTO 555
        ENDIF
        IF (ZI(JCOUR2-1 + NBMA+I).EQ.NJONC) THEN
          ZI(JCOUR1-1 +        N)=ZI(JMAIL-1  +      I)
          ZI(JCOUR1-1 +   NBMA+N)=ZI(JCOUR2-1 + NBMA+I)
          ZI(JCOUR1-1 + 2*NBMA+N)=ZI(JCOUR2-1 +      I)
          NJONC                  =ZI(JCOUR2-1 +      I)
          GOTO 555
        ENDIF
552   CONTINUE
C
555   CONTINUE
      DO 557 K=N,I-1
        ZI(JCOUR1-1 +        1+K)=ZI(JMAIL-1  +      K)
        ZI(JCOUR1-1 +   NBMA+1+K)=ZI(JCOUR2-1 +      K)
        ZI(JCOUR1-1 + 2*NBMA+1+K)=ZI(JCOUR2-1 + NBMA+K)
557   CONTINUE
      DO 558 K=I+1,NBMA
        ZI(JCOUR1-1 +        K)=ZI(JMAIL-1  +      K)
        ZI(JCOUR1-1 +   NBMA+K)=ZI(JCOUR2-1 +      K)
        ZI(JCOUR1-1 + 2*NBMA+K)=ZI(JCOUR2-1 + NBMA+K)
558   CONTINUE
      DO 559 K=N,NBMA
        ZI(JMAIL-1  +      K)=ZI(JCOUR1-1 +       K)
        ZI(JCOUR2-1 +      K)=ZI(JCOUR1-1 +  NBMA+K)
        ZI(JCOUR2-1 + NBMA+K)=ZI(JCOUR1-1 +2*NBMA+K)
559   CONTINUE
      N=N+1
      IF (N.GT.NBMA) GOTO 560
      GOTO 550
C

560   CONTINUE
C
C     ------------------------------------------------------------------
C     --- SAUVEGARDE DES NOEUDS ORDONNES DANS LA STRUCTURE DE DONNEES
C     --- AVEC RAJOUT DES NOEUDS MILIEUX SI SEG3
C     ------------------------------------------------------------------
C
      IF ( TYPM(1:4) .EQ. 'SEG2' ) THEN
      
        NBNO=NBMA+1
        CALL WKVECT(NOEORD,BASE//' V I',NBNO,JNOE)
        DO 570 I=1,NBMA
          ZI(JNOE-1 + I) = ZI(JCOUR2-1 + I)
570     CONTINUE
        ZI(JNOE-1 + NBMA+1) = ZI(JCOUR2-1 + 2*NBMA)
C
      ELSEIF ( TYPM(1:4) .EQ. 'SEG3' ) THEN
      
        NBNO=2*NBMA+1
        CALL WKVECT(NOEORD,BASE//' V I',NBNO,JNOE)
        DO 575 I=1,NBMA
          ZI(JNOE-1 + 2*I-1)   = ZI(JCOUR2-1 + I)
          CALL JEVEUO(JEXNUM(CONEC,ZI(JMAIL-1 + I)),'L',JRDM)
          ZI(JNOE-1 + 2*I) = ZI(JRDM-1 + 3)
575     CONTINUE
        ZI(JNOE-1 + 2*NBMA+1) = ZI(JCOUR2-1 + 2*NBMA)
CJMP
      ELSEIF ( TYPM(1:4) .EQ. 'SEG4' ) THEN
      
        NBNO=3*NBMA+1
        CALL WKVECT(NOEORD,BASE//' V I',NBNO,JNOE)
        DO 580 I=1,NBMA
          ZI(JNOE-1 + 3*I-2)   = ZI(JCOUR2-1 + I)
          CALL JEVEUO(JEXNUM(CONEC,ZI(JMAIL-1 + I)),'L',JRDM)
          CALL ASSERT((ZI(JRDM-1 + 1).EQ.ZI(JCOUR2-1 + I)).OR.
     &                (ZI(JRDM-1 + 2).EQ.ZI(JCOUR2-1 + I)))
          IF (ZI(JRDM-1 + 1).EQ.ZI(JCOUR2-1 + I)) THEN
             ZI(JNOE-1 + 3*I-1) = ZI(JRDM-1 + 3)
             ZI(JNOE-1 + 3*I  ) = ZI(JRDM-1 + 4)
          ELSEIF (ZI(JRDM-1 + 2).EQ.ZI(JCOUR2-1 + I)) THEN
             ZI(JNOE-1 + 3*I-1) = ZI(JRDM-1 + 4)
             ZI(JNOE-1 + 3*I  ) = ZI(JRDM-1 + 3)
          ENDIF
580     CONTINUE
        ZI(JNOE-1 + 3*NBMA+1) = ZI(JCOUR2-1 + 2*NBMA)

      ENDIF
C
C     ------------------------------------------------------------------
C     --- VERIFICATION DU NOEUD EXTREMITE LORSQU'IL EST DONNE
C     --- DANS LE CAS D UNE COURBE NON FERMEE
C     ------------------------------------------------------------------
C

      IF (NDEXTR.NE.' ') THEN
        CALL JEVEUO(NOEORD,'L',JNOE)
        CALL JELIRA(NOEORD,'LONUTI',NBNO,K8B)
        CALL JENUNO(JEXNUM(NOMNOE,ZI(JNOE-1 + NBNO)),NOEUD)
        IF (NDEXTR.NE.' ') THEN
          IF ( NOEUD .NE. NDEXTR )
     &      CALL U2MESK('F','ELEMENTS_77',1,NDEXTR)
        ELSE
          CALL U2MESK('I','ELEMENTS_78',1,NOEUD)
        END IF
      ENDIF
      CALL JEDETR ( '&&'//NOMPRO//'.MAILLES_TRIEE' )
      CALL JEDETR ( '&&'//NOMPRO//'.NOEUDS_EXTREM' )
      CALL JEDETR ( RESU//'.MAILLESFOURNIES' )
C
      CALL JEDEMA()
      END
