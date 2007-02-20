      SUBROUTINE RGNDAS(TYPE,NU,IEQ,NOMNO,NOMCMP,TARDIF,LIGREL,INFOBL)
      IMPLICIT   NONE
      INTEGER           IEQ
      CHARACTER*(*)     TYPE,NU,NOMNO,NOMCMP,TARDIF,LIGREL,INFOBL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     RETROUVER LE NOM DU NOEUD ET LE CMP CORRESPONDANT A UN NUMERO
C     D'EQUATION DANS UN SYSTEME ASSEMBLE.
C ----------------------------------------------------------------------
C IN  : TYPE   : TYPE DU NUM
C IN  : NU     : NOM D'UN NUME_DDL OU D'UN PROF_CHNO
C IN  : IEQ    : NUMERO D'UNE EQUATION DANS UN SYSTEME ASSEMBLE
C OUT : NOMNO  : NOM DU NOEUD ASSOCIE A IEQ
C OUT : NOMCMP : NOM DU CMP ASSOCIE A IEQ
C OUT : TARDIF : '        ' SI LE NOEUD N'EST PAS TARDIF
C                ' TARDIF ' SI LE NOEUD EST TARDIF
C OUT : LIGREL : NOM DU LIGREL SI LE NOEUD EST TARDIF. (BLANC SINON)
C OUT : INFOBL : NOM DU NOEUD BLOQUE SUIVI DE LA CMP BLOQUEE.
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
      INTEGER       IBID, GD, NBEC, NEC, JPRNO, JNUEQ, IDESC, IRET, ICO
      INTEGER       JDESC, IER, NLILI, I, ILO, NBNO, INO, IDEB, NCMP
      INTEGER       ICMP, IIEQ, NULI, NUNO, NUCMP, NCMPMX, IADG1, JREFE
      INTEGER       INOCMP, NUDDL, NEQ, NUSST, NULIA, JDEEQ, JORIG
      CHARACTER*8   NOMA, K8B, NOMEQ, NOMNO2, NOMCM2, MODGEN, KN1, KN2
      CHARACTER*24 VALK
      CHARACTER*19  PRNO, NUME
      LOGICAL       TROUVE, EXISDG
C
C DEB-------------------------------------------------------------------
C
      CALL JEMARQ()
      NUME( 1:14) = NU
      NUME(15:19) = '.NUME'
C
C
C DETERMINATION TYPE CONCEPT SI DESC=1 NUMEDDL
C                            SI DESC=2 NUMEDDL_GENE
C PAR DEFAUT ON MET IDESC=1 (CAR LE .DESC DU NUME_DDL N'EXISTE PAS)
C
C
      IDESC = 1
      CALL JEEXIN (NUME//'.DESC',IRET )
      IF ( IRET .GT. 0 ) THEN
         CALL JEVEUO(NUME//'.DESC','L',JDESC)
         IDESC = ZI(JDESC)
      ENDIF
C
C
      IF ( IDESC .EQ. 1 ) THEN
         IF ( TYPE(1:8) .EQ. 'NUME_DDL' ) THEN
            CALL DISMOI('F','NOM_MAILLA',NU,'NUME_DDL',IBID,NOMA,IER)
            CALL DISMOI('F','NUM_GD_SI' ,NU,'NUME_DDL',GD  ,K8B ,IER)
            PRNO( 1:14) = NU
            PRNO(15:19) = '.NUME'
         ELSEIF ( TYPE(1:7) .EQ. 'CHAM_NO' ) THEN
            CALL DISMOI('F','NOM_MAILLA', NU,'CHAM_NO',IBID,NOMA,IER)
            CALL DISMOI('F','PROF_CHNO' , NU,'CHAM_NO',IBID,PRNO,IER)
            CALL DISMOI('F','NUM_GD'    , NU,'CHAM_NO',GD  ,K8B ,IER)
         ELSE
            CALL U2MESS('F','ALGELINE_29')
         ENDIF
         NEC = NBEC(GD)
C
         CALL JEVEUO ( PRNO//'.NUEQ', 'L', JNUEQ )
C
         CALL JELIRA(PRNO//'.PRNO','NMAXOC',NLILI,K8B)
         TROUVE = .FALSE.
         DO 10 I = 1 , NLILI
            CALL JENUNO ( JEXNUM(PRNO//'.LILI',I), LIGREL )
            CALL JELIRA ( JEXNUM(PRNO//'.PRNO',I), 'LONMAX', ILO, K8B)
            IF ( ILO .LE. 0 ) GOTO 10
            CALL JEVEUO ( JEXNUM(PRNO//'.PRNO',I), 'L', JPRNO )
            NBNO = ILO / ( NEC + 2 )
            DO 20 INO = 1 , NBNO
               IDEB = ZI(JPRNO-1+(INO-1)*(NEC+2)+1)
               NCMP = ZI(JPRNO-1+(INO-1)*(NEC+2)+2)
               DO 30 ICMP = 1 , NCMP
                  IIEQ = ZI(JNUEQ-1+IDEB-1+ICMP)
                  IF ( IEQ .EQ. IIEQ ) THEN
                     TROUVE = .TRUE.
                     NULI = I
                     NUNO = INO
                     NUCMP = ICMP
                     GO TO 9998
                  ENDIF
 30            CONTINUE
 20         CONTINUE
 10      CONTINUE
C
 9998    CONTINUE
         IF ( TROUVE ) THEN
            CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',
     &                                                NCMPMX,K8B)
            CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',INOCMP)
            IADG1 = JPRNO - 1 + (NUNO-1)* (NEC+2) + 3
            ICO = 0
            DO 40 ICMP = 1 , NCMPMX
               IF ( EXISDG( ZI(IADG1) , ICMP ) ) THEN
                  ICO = ICO + 1
                  IF ( ICO .EQ. NUCMP ) GOTO 42
               ENDIF
 40         CONTINUE
 42         CONTINUE
            NOMCMP = ZK8(INOCMP-1+ICMP)
C
            IF ( NULI .EQ. 1 ) THEN
               TARDIF = '  '
               LIGREL = '  '
               CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO),NOMNO)
            ELSE
               TARDIF = 'TARDIF  '
               CALL CODENT(-NUNO,'D',NOMNO)
            ENDIF
         ELSE
            CALL CODENT(IEQ,'D',NOMEQ)
            CALL U2MESK('A','UTILITAI2_31',1,NOMEQ)
         ENDIF
C
 9999    CONTINUE
C
C        SI LE NOEUD EST TARDIF ON REMPLIT INFOBL:
         IF ( TARDIF .NE. '  ' ) THEN
            CALL JEVEUO ( PRNO//'.DEEQ','L',JDEEQ)
            NUNO  = ZI(JDEEQ-1+2*(IEQ-1)+1)
            NUDDL = ZI(JDEEQ-1+2*(IEQ-1)+2)
C           --- SI NUNO = 0  C'EST UNE LIAISON_DDL ---
            IF ( NUNO .EQ. 0 ) THEN
               INFOBL = 'NOEUD DE LIAISON_DDL'
            ELSE
               CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO),NOMNO2)
               NOMCM2 = ZK8(INOCMP-1+ (-NUDDL) )
               INFOBL = 'NOEUD: '//NOMNO2//' CMP: '//NOMCM2
            ENDIF
         ELSE
            INFOBL = ' '
         ENDIF
C
      ELSEIF( IDESC .EQ. 2 ) THEN
        CALL JEVEUO(NUME//'.DEEQ','L',JDEEQ)
        CALL JELIRA(NUME//'.DEEQ','LONMAX',NEQ,K8B)
        NEQ   = NEQ / 2
        NUNO  = ZI(JDEEQ+2*IEQ-1)
        NUCMP = ZI(JDEEQ+2*IEQ-2)
        IF ( NUNO .GT. 0 ) THEN
           CALL JEVEUO(JEXNUM(NUME//'.ORIG',1),'L',JORIG)
           NUSST = ZI(JORIG+NUNO-1)
           CALL JEVEUO(NUME//'.REFE','L',JREFE)
           MODGEN = ZK24(JREFE)
           CALL JENUNO(JEXNUM(MODGEN//'      .MODG.SSNO',NUSST),NOMNO)
           NOMCMP(1:3) = 'GEN'
           WRITE(NOMCMP(4:8),'(I5)') NUCMP
           TARDIF = '         '
           INFOBL = ' '
           LIGREL = ' '
        ELSE
           NUNO  = -NUNO
           TARDIF = 'TARDIF'
           CALL JEVEUO(JEXNUM(NUME//'.ORIG',2),'L',JORIG)
           NULIA = ZI(JORIG+NUNO+1)
           NOMNO(1:3)='TARD'
           WRITE(NOMNO(4:8),'(I5)') NUNO
           NOMCMP(1:3) = 'LAG'
           WRITE(NOMCMP(4:8),'(I5)') NUCMP
           WRITE(NOMCMP(4:8),'(I5)') NUCMP
           WRITE(KN1(1:7),'(I7)') IEQ
           WRITE(KN2(1:4),'(I4)') NULIA
           INFOBL = 'EQUATION:'//KN1//'   LIAISON:'//KN2
           LIGREL = ' '
        ENDIF
C
      ELSE
        VALK = NUME
        CALL U2MESG('F', 'UTILITAI6_76',1,VALK,0,0,0,0.D0)
      ENDIF
C
      CALL JEDEMA()
      END
