      SUBROUTINE PTEDDL ( TYPE, NUM, NBCMP, LNOCMP, NEQ, IVEC )
      IMPLICIT  NONE
      INTEGER           NBCMP, NEQ, IVEC(NEQ,*)
      CHARACTER*(*)     TYPE, NUM
      CHARACTER*8       LNOCMP(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/04/2000   AUTEUR ACBHHCD G.DEVESA 
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
C IN : TYPE   : TYPE DU NUM
C IN : NUM    : NOM D'UN NUME_DDL OU D'UN PROF_CHNO
C IN : NBCMP  : NOMBRE DE CMP DE LA LISTE LNOCMP
C IN : LNOCMP : LISTE DE NOMS DE CMP
C IN : NEQ    : NOMBRE D'EQUATIONS DE NUM
C OUT: IVEC   : TABLEAU DE POINTEURS DE DDLS DEJA ALLOUE.
C      IVEC(IEQ,ICMP) =
C                   1 SI LE IEQ-EME CMP DE NUM A POUR NOM: LNOCMP(ICMP)
C                   0 SINON
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
      INTEGER       IBID, I, J, TABEC(10), IER, IDESC, INUDES, NCMPMX
      INTEGER       JDESC, NEC, NBEC, GD, IAD, IEC, JNUCMP, INDIK8
      INTEGER       JNUEQ, NLILI, JPRNO, NBNO, IVAL, NCMP, ICOMPT
      INTEGER       ICMP, IEQ, NUCMP, JDEEQ, NLEQ, NUMNO, JLILI, INO
      CHARACTER*8   K8B, NOMMA
      CHARACTER*19  NOMNU, PRNO
      CHARACTER*24  NOLILI
      LOGICAL       EXISDG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      DO 10 I = 1 , NEQ
         DO 10 J = 1 , NBCMP
            IVEC(I,J) = 0
 10   CONTINUE
C
      NOMNU( 1:14) = NUM
      NOMNU(15:19) = '.NUME'
C
C        CALL JEVEUO ( NOMNU//'.DEEQ', 'L', JDEEQ )
C        CALL JELIRA ( NOMNU//'.DEEQ', 'LONMAX', NLEQ, K8B )
C        NLEQ = NLEQ / 2
C
C DETERMINATION TYPE CONCEPT SI DESC=1 NUMEDDL
C                            SI DESC=2 NUMEDDL_GENE
C PAR DEFAUT ON MET IDESC=1 (CAR LE .DESC DU NUME_DDL N'EXISTE PAS)
C
C
      IDESC = 1
      CALL JEEXIN ( NOMNU//'.DESC', INUDES )
      IF ( INUDES .GT. 0 ) THEN
         CALL JEVEUO ( NOMNU//'.DESC', 'L', JDESC )
         IDESC = ZI(JDESC)
      ENDIF
C
      IF ( IDESC .EQ. 1 ) THEN
         IF ( TYPE(1:8) .EQ. 'NUME_DDL' ) THEN
            CALL DISMOI('F','NOM_MAILLA',NUM,'NUME_DDL',IBID,NOMMA,IER)
            CALL DISMOI('F','NUM_GD_SI' ,NUM,'NUME_DDL',GD  ,K8B  ,IER)
            PRNO( 1:14) = NUM
            PRNO(15:19) = '.NUME'
         ELSEIF ( TYPE(1:7) .EQ. 'CHAM_NO' ) THEN
            CALL DISMOI('F','NOM_MAILLA', NUM,'CHAM_NO',IBID,NOMMA,IER)
            CALL DISMOI('F','PROF_CHNO' , NUM,'CHAM_NO',IBID,PRNO ,IER)
            CALL DISMOI('F','NUM_GD'    , NUM,'CHAM_NO',GD  ,K8B  ,IER)
         ELSE
            CALL UTMESS('F','PTEDDL',' TYPE INCONNU')
         ENDIF
         NEC = NBEC( GD )
         IF ( NEC .GT. 10 ) CALL UTMESS('F','PTEDDL','NEC TROP GRAND')
C
         CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',IAD)
         CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K8B)
         CALL WKVECT( '&&PTEDDL.NUME_CMP','V V I',NCMPMX,JNUCMP)
         DO 20 I = 0 , NCMPMX-1
            ZI(JNUCMP+I) = INDIK8(LNOCMP,ZK8(IAD+I),1,NBCMP)
 20      CONTINUE
C
         CALL JEVEUO(PRNO//'.NUEQ','L',JNUEQ)
C
         CALL JELIRA(PRNO//'.PRNO','NMAXOC',NLILI,K8B)
         DO 30,I = 1 , NLILI
            CALL JENUNO(JEXNUM(PRNO//'.LILI',I),NOLILI)
            CALL JELIRA(JEXNUM(PRNO//'.PRNO',I),'LONMAX',IBID,K8B)
            IF ( IBID .EQ. 0 ) GO TO 30
            CALL JEVEUO(JEXNUM(PRNO//'.PRNO',I),'L',JPRNO)
            IF ( IBID.EQ.1.AND.ZI(JPRNO).EQ.0 ) GO TO 30
C
C           --RECHERCHE DU NOMBRE DE NOEUDS : NBNO
            IF (NOLILI(1:8).EQ.'&MAILLA ') THEN
               CALL JELIRA(NOMMA//'.NOMNOE','NOMMAX',NBNO,K8B)
            ELSE
               CALL JEVEUO(NOLILI(1:19)//'.NBNO','L',JLILI)
               NBNO = ZI(JLILI-1+1)
            END IF
            DO 32,INO = 1 , NBNO
C              NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
C              IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
               IVAL = ZI(JPRNO-1 + (INO-1)* (NEC+2) + 1 )
               NCMP = ZI(JPRNO-1 + (INO-1)* (NEC+2) + 2 )
               IF ( NCMP .EQ. 0 ) GO TO 32
               DO 34 IEC = 1 , NEC
                  TABEC(IEC)= ZI(JPRNO-1+(INO-1)*(NEC+2)+2+IEC )
 34            CONTINUE
               IF (NCMP.EQ.0) GO TO 32
C
               ICOMPT = 0
               DO 36 ICMP = 1 , NCMPMX
                  IF ( EXISDG( TABEC , ICMP ) ) THEN
                     ICOMPT = ICOMPT + 1
                     IEQ = ZI(JNUEQ-1+IVAL-1+ICOMPT)
                     NUCMP = ZI(JNUCMP+ICMP-1)
                     IF ( NUCMP .GT. 0 ) IVEC(IEQ,NUCMP) = 1
                  ENDIF
 36            CONTINUE
 32         CONTINUE
 30      CONTINUE
         CALL JEDETR ( '&&PTEDDL.NUME_CMP' )
 9999    CONTINUE
C
C
      ELSEIF ( IDESC .EQ. 2 ) THEN
        CALL JEVEUO ( NOMNU//'.DEEQ', 'L', JDEEQ )
        CALL JELIRA ( NOMNU//'.DEEQ', 'LONMAX', NLEQ, K8B )
        NLEQ = NLEQ / 2
        IF( NLEQ .NE. NEQ ) CALL UTMESS('F','PTEDDL',
     +                     'INCOMPATIBILITE DE NOMBRE EQUATIONS')
        DO 40 IEQ = 1 , NEQ
           NUMNO = ZI(JDEEQ+2*IEQ-1)
           DO 42 J = 1 , NBCMP
              IF(LNOCMP(J).EQ.'LAGR'.AND.NUMNO.LT.0) IVEC(IEQ,J)=1
              IF(LNOCMP(J).EQ.'GENE'.AND.NUMNO.GT.0) IVEC(IEQ,J)=1
 42        CONTINUE
 40     CONTINUE
C
      ELSE
        CALL UTDEBM('F','PTEDDL','TYPE DE NUMEROTATION NON CONNUE')
        CALL UTIMPK('L',' NUMEROTATION:',1,NOMNU)
        CALL UTFINM
C
      ENDIF
C
      CALL JEDEMA()
      END
