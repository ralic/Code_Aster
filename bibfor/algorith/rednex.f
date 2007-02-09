      SUBROUTINE REDNEX(NU, NEQ, VECNTD,
     &                  VECNTX, VECNTY, VECNTZ)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2007   AUTEUR TORKHANI M.TORKHANI 
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
      IMPLICIT  NONE
      INTEGER   NEQ
      INTEGER   VECNTD(1,*),VECNTX(1,*),VECNTY(1,*),VECNTZ(1,*)
      CHARACTER*(*)     NU
C
C ROUTINE APPELEE PAR : NMIBLE
C ---------------------------------------------------------------------
C     STOCKER LES NOM DES NOEUDS ET DES DDLS IMPLIQUES DANS  
C     UNE EQUATION DE SYSTEME LINEAIRE DE TYPE LAGRANGE / 
C     LIAISON_DDL EN VUE DU TRAITEMENT AUTOMATIQUE DES 
C     RELATIONS SURABONDANTES AVEC LE CONTACT
C ----------------------------------------------------------------------
C IN   NU        : NOM D'UN NUME_DDL OU D'UN PROF_CHNO
C IN   NEQ       :
C OUT VECLID     : VECTEUR CONTENANT LES NOEUDS IMPLIQUES DANS 
C                  UNE EQUATION DE SYSTEME LINEAIRE DE TYPE 
C
C OUT VECNOD     : VECTEUR CONTENANT LES NOEUDS IMPLIQUES
C                  DANS DES RELATIONS DDL_IMPO OU LIAISON_DDL
C OUT VECNOX    : VECTEUR CONTENANT LES NOEUDS IMPLIQUES
C                  DANS DES RELATIONS DDL_IMPO OU LIAISON_DDL
C                  PORTANT SUR LE DDL 'DX'
C OUT VECNOY    : VECTEUR CONTENANT LES NOEUDS IMPLIQUES
C                  DANS DES RELATIONS DDL_IMPO OU LIAISON_DDL
C                  PORTANT SUR LE DDL 'DY'
C OUT VECNOZ    : VECTEUR CONTENANT LES NOEUDS IMPLIQUES
C                  DANS DES RELATIONS DDL_IMPO OU LIAISON_DDL
C                  PORTANT SUR LE DDL 'DZ'
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       IBID, GD, NBEC, NEC, JPRNO, JNUEQ ,IFM,IUNIFI
      INTEGER       IER,NLILI,I,ILO,NBNO,INO,IDEB,NCMP,ICMP,IIEQ,NULI
      INTEGER       NUNO,NCMPMX,INOCMP,IADG1,JDEEQ,NUDDL,ICO
      INTEGER       NBMAS,K,JNUNO,KNO,KKK
      INTEGER       II,JJ,KK,ITYP,IDEEQ,JVECLI
      CHARACTER*8   NOMA, K8B, NOMEQ, NOMNO,NOMCMP
      CHARACTER*19  PRNO, LIGREL
      CHARACTER*24  DEEQ
      CHARACTER*80  INFOBL,TARDIF
      LOGICAL       TROUVE, EXISDG
C
C DEB-------------------------------------------------------------------
C
      CALL JEMARQ()
C
      
        CALL WKVECT ('&&REDNEX.VECLID','V V I',NEQ,JVECLI )
        DO 90 II = 1, NEQ
          ZI(JVECLI-1+II) = 0
 90     CONTINUE
 
        DEEQ = NU//'.NUME.DEEQ'
        CALL JEVEUO(DEEQ,'L',IDEEQ)
      
        DO 2902 II = 1,NEQ
          ITYP = ZI(IDEEQ-1+2*II)
          IF(ITYP.LT.0)  THEN 
            VECNTD(1,II)=ZI(IDEEQ-1+2*II-1)
            IF(ITYP.EQ.-1) VECNTX(1,II)=1
            IF(ITYP.EQ.-2) VECNTY(1,II)=1
            IF(ITYP.EQ.-3) VECNTZ(1,II)=1        
          END IF
         
          IF(ITYP.EQ.0) THEN
C
          CALL DISMOI('F','NOM_MAILLA',NU,'NUME_DDL',IBID,NOMA,IER)
          CALL DISMOI('F','NUM_GD_SI' ,NU,'NUME_DDL',GD  ,K8B ,IER)
          PRNO( 1:14) = NU
          PRNO(15:19) = '.NUME'
          NEC = NBEC(GD)
C
          CALL JEVEUO ( PRNO//'.NUEQ', 'L', JNUEQ )
C
          CALL JELIRA(PRNO//'.PRNO','NMAXOC',NLILI,K8B)
          TROUVE = .FALSE.
          DO 10 I = 1 , NLILI
            CALL JENUNO ( JEXNUM(PRNO//'.LILI',I), LIGREL )
            CALL JELIRA ( JEXNUM(PRNO//'.PRNO',I), 'LONMAX', ILO, K8B)
            IF ( ILO .EQ. 0 ) GOTO 10
              CALL JEVEUO ( JEXNUM(PRNO//'.PRNO',I), 'L', JPRNO )
              NBNO = ILO / ( NEC + 2 )
              DO 20 INO = 1 , NBNO
                IDEB = ZI(JPRNO-1+(INO-1)*(NEC+2)+1)
                NCMP = ZI(JPRNO-1+(INO-1)*(NEC+2)+2)
                DO 30 ICMP = 1 , NCMP
                  IIEQ = ZI(JNUEQ-1+IDEB-1+ICMP)
                  IF ( II .EQ. IIEQ ) THEN
                    TROUVE = .TRUE.
                    NULI = I
                    NUNO = INO
                    GO TO 9998
                  ENDIF
 30             CONTINUE
 20          CONTINUE
 10       CONTINUE
C
 9998     CONTINUE

          IF ( .NOT.TROUVE ) THEN
            CALL CODENT(II,'D',NOMEQ)
            CALL U2MESK('F','UTILITAI2_31',1,NOMEQ)
          END IF

          IF ( NULI .EQ. 1 ) CALL U2MESS('F','CALCULEL_2')

C     ON PARCOURT LES MAILLES SUPPLEMENTAIRES DU LIGREL TROUVE
C     POUR IMPRIMER LES CONNECTIVITES DE CES MAILLES :
          CALL JENUNO ( JEXNUM(PRNO//'.LILI',NULI), LIGREL )
          CALL JELIRA ( LIGREL//'.NEMA','NMAXOC', NBMAS, K8B)

          DO 777,K=1,NBMAS
            CALL JELIRA ( JEXNUM(LIGREL//'.NEMA',K),'LONMAX', NBNO, K8B)
C       -- L'OBJET .NEMA CONTIENT LE TYPE_MAILLE AU BOUT :
            IF (NBNO.EQ.0) GO TO 777
            NBNO=NBNO-1
            CALL JEVEUO ( JEXNUM(LIGREL//'.NEMA',K),'L', JNUNO)
            TROUVE=.FALSE.
            DO 778,KK=1,NBNO
              IF (ZI(JNUNO-1+KK).EQ.-NUNO) TROUVE=.TRUE.
778         CONTINUE
            IF (.NOT.TROUVE) GO TO 777
            DO 779,KK=1,NBNO
              KNO=ZI(JNUNO-1+KK)
              KKK=KK+(K-1)*NBNO    
              IF (KNO.GT.0) THEN
                ZI(JVECLI-1+KKK)=KNO 
              END IF
779         CONTINUE
777       CONTINUE
C          
          DO 1977 JJ=1,NEQ
            IF (ZI(JVECLI-1+JJ) .GT. 0) THEN 
              DO 1966 KK=NEQ+1,2*NEQ
                IF (ZI(JVECLI-1+JJ) .NE. VECNTD(1,KK)) THEN  
                  VECNTD(1,JJ+NEQ)=ZI(JVECLI-1+JJ)
                ENDIF
 1966         CONTINUE    
            ENDIF
 1977     CONTINUE           
          ENDIF
 2902   CONTINUE
      

      CALL JEDETR('&&REDNEX.VECLID')
      CALL JEDEMA()
      END
