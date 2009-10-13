      SUBROUTINE IREDCA ( MACR )
      IMPLICIT NONE
      CHARACTER*8         MACR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/10/2009   AUTEUR DELMAS J.DELMAS 
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
C     IMPRESSION D'UN CONCEPT MACR_ELEM_DYNA AU FORMAT "CADYRO"
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C TOLE CRP_20
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
      CHARACTER*32      JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      NBCMPM
      INTEGER VALI
      PARAMETER    (NBCMPM=10)
      INTEGER       N1, IFMA, NBMAIL, NBNOEU, IER, IMA, INO, NBT, NBM
      INTEGER       JVALE, JREFE, JTYPE, IM, IFMS, IFMM, I, J, K, L
      INTEGER       NBMODE, NBSTAT, JNOEB, NEQF, IREC, IAD, JANGL
      INTEGER       IBID, JNUME, JIEEE, JNODDL, JCRAY, NUNOE, NUDDL
      INTEGER       JPARM, JPARA, JNODDE, ICRAY(2), IEEE(2)
      INTEGER       NUMENO, JNOE1, JNOE2, JNOEU, NUME1, NUME2
      INTEGER       CAS, INDIRC(6)
      REAL*8        X, Y, Z , ANGL(3), X1(6), XR1(6)
      REAL*8        MAT1(NBCMPM,NBCMPM),MAT2(NBCMPM,NBCMPM)
      REAL*8        MAT3(NBCMPM,NBCMPM),ZERO,MATTMP(NBCMPM,NBCMPM)
      REAL*8        MATROT(NBCMPM,NBCMPM)
      REAL*8        TOL
      CHARACTER*3   OUINON
      CHARACTER*6   K6B
      CHARACTER*8   K8B, NOMA, NONOEU, NOMAIL, INTF, CMP, NOMCMP(6)
      CHARACTER*19  BAMO, NUMDDL, CHAMNO
      CHARACTER*24   COOVAL, NOMNOE
      CHARACTER*24 VALK(2)
      CHARACTER*24  NOMVE1, NOMVE2, NOMVE3
C
      REAL*8        PI, R8PI
CS
CS DEFINITION DU TYPE D'ECRITURE (BINAIR = .TRUE.  => binaire)
CS ------------------------------(BINAIR = .FALSE. => ASCII)
CS
      LOGICAL BINAIR
      DATA BINAIR /.FALSE./
CS
C     ------------------------------------------------------------------
      DATA ZERO /0.0D+00/
      DATA NOMCMP / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
      DATA TOL / 1.0D-10 /
C     ------------------------------------------------------------------
C
      PI = R8PI()
C
      CALL JEMARQ ( )
C
C                --- IMPRESSION DU MAILLAGE ---
C
      CALL GETVID ( ' ', 'SQUELETTE'     , 1,1,1, NOMA, N1 )
      CALL GETVIS ( ' ', 'UNITE_MAILLAGE', 1,1,1, IFMA, N1 )
      CALL ULOPEN ( IFMA,' ',' ','NEW','O' )
C
C --- RECUPERATION DES ANGLES NAUTIQUES ET CALCUL MATRICE DE ROTATION
C
      CALL JEVEUO(NOMA//'.ANGL_NAUT      ','L',JANGL)
      ANGL(1) = ZR(JANGL-1+1)
      ANGL(2) = ZR(JANGL-1+2)
      ANGL(3) = ZR(JANGL-1+3)
C
C --- DEBUT MODIF 1
C
C --- TEST SUR LA VALEUR DES ANGLES NAUTIQUES
C LES TRIPLETS ( ANGL(1) ANGL(2) ANGL(3) ) ACCEPTABLES SONT :
C           ( 0.0   0.0   0.0 ) ---> CAS = 0
C           ( 0.0 -90.0   0.0 ) ---> CAS = 1
C           ( 0.0 -90.0 180.0 ) ---> CAS = 2
C
      IF (
     &     .NOT. ( ( (ABS(ANGL(1)) .LT. TOL) .AND.
     &               (ABS(ANGL(2)) .LT. TOL) .AND.
     &               (ABS(ANGL(3)) .LT. TOL)
     &             ) .OR.
     &             ( (ABS(ANGL(1)) .LT. TOL)            .AND.
     &               (ABS(ANGL(2)+(PI/2.0D0)) .LT. TOL) .AND.
     &               (ABS(ANGL(3)) .LT. TOL)
     &             ) .OR.
     &             ( (ABS(ANGL(1)) .LT. TOL)            .AND.
     &               (ABS(ANGL(2)+(PI/2.0D0)) .LT. TOL) .AND.
     &               (ABS(ANGL(3)-PI) .LT. TOL)
     &             )
     &           )
     &   ) CALL ASSERT(.FALSE.)
C
        CAS = 0
C
        IF (
     &       (ABS(ANGL(1)) .LT. TOL)            .AND.
     &       (ABS(ANGL(2)+(PI/2.0D0)) .LT. TOL) .AND.
     &       (ABS(ANGL(3)) .LT. TOL)
     &     ) CAS = 1
C
        IF (
     &       (ABS(ANGL(1)) .LT. TOL)            .AND.
     &       (ABS(ANGL(2)+(PI/2.0D0)) .LT. TOL) .AND.
     &       (ABS(ANGL(3)-PI) .LT. TOL)
     &     ) CAS = 2
C
C --- FIN TEST
      DO 3 I = 1, 6
         INDIRC(I) = I
 3    CONTINUE
C
      IF ( CAS .NE. 0 ) THEN
         INDIRC(1) = 3
         INDIRC(3) = 1
         INDIRC(4) = 6
         INDIRC(6) = 4
      END IF
C
C --- FIN MODIF 1
C
      CALL INTET0(ANGL(1),MAT1,3)
      CALL INTET0(ANGL(2),MAT2,2)
      CALL INTET0(ANGL(3),MAT3,1)
      CALL R8INIR(NBCMPM*NBCMPM,ZERO,MATTMP,1)
      CALL PMPPR(MAT3,NBCMPM,NBCMPM,1,MAT2,NBCMPM,NBCMPM,1,
     &           MATTMP,NBCMPM,NBCMPM)
      CALL R8INIR(NBCMPM*NBCMPM,ZERO,MATROT,1)
      CALL PMPPR(MATTMP,NBCMPM,NBCMPM,1,MAT1,NBCMPM,NBCMPM,1,
     &           MATROT,NBCMPM,NBCMPM)
C
C --- RECUPERATION DES NOEUDS
C
      NOMVE1 = '&&IREDCA.MAIL_NOEU1'
      NOMVE2 = '&&IREDCA.MAIL_NOEU2'
      NOMVE3 = '&&IREDCA.LIST_NOEUD'
      CALL SQUSEG ( NOMA, NOMVE1, NOMVE2, NOMVE3 )
      CALL JELIRA ( NOMVE1 , 'LONUTI' , NBMAIL , K8B )
      CALL JELIRA ( NOMVE3 , 'LONUTI' , NBNOEU , K8B )
      CALL JEVEUO ( NOMVE1 , 'L' , JNOE1 )
      CALL JEVEUO ( NOMVE2 , 'L' , JNOE2 )
      CALL JEVEUO ( NOMVE3 , 'L' , JNOEU )
C
      COOVAL = NOMA//'.COORDO    .VALE'
      NOMNOE = NOMA//'.NOMNOE         '
      CALL JEVEUO ( COOVAL , 'L' , JVALE )
C
      WRITE(IFMA,'(2I10)') NBNOEU , NBMAIL
C
      DO 10 I = 0 , NBNOEU-1
         INO = ZI(JNOEU+I)
         CALL JENUNO ( JEXNUM(NOMNOE,INO) , NONOEU )
         X = ZR(JVALE+3*(INO-1)  )
         Y = ZR(JVALE+3*(INO-1)+1)
         Z = ZR(JVALE+3*(INO-1)+2)
         WRITE(IFMA,'(1X,A8,3(1X,F10.4))') NONOEU, X, Y, Z
 10   CONTINUE
C
      DO 20 IMA = 1 , NBMAIL
         CALL CODENT( IMA , 'G' , K6B  )
         NOMAIL = 'AR'//K6B
         INO = ZI(JNOE1+IMA-1)
         DO 22 NUME1 = 1 , NBNOEU
            IF ( INO .EQ. ZI(JNOEU+NUME1-1) ) GOTO 24
 22      CONTINUE
         CALL ASSERT(.FALSE.)
 24      CONTINUE
         INO = ZI(JNOE2+IMA-1)
         DO 26 NUME2 = 1 , NBNOEU
            IF ( INO .EQ. ZI(JNOEU+NUME2-1) ) GOTO 28
 26      CONTINUE
         CALL ASSERT(.FALSE.)
 28      CONTINUE
         WRITE(IFMA,'(1X,A8,2(1X,I10))') NOMAIL, NUME1, NUME2
 20   CONTINUE
C
C ---------------------------------------------------------------------
C
      CALL JEVEUO ( MACR//'.MAEL_REFE' , 'L' , JREFE )
      BAMO = ZK24(JREFE)(1:19)
      CALL JEVEUO ( BAMO//'.REFD' , 'L' , JREFE )
      INTF = ZK24(JREFE+4)(1:19)
      CALL JEVEUO ( INTF//'.IDC_TYPE' , 'L' , JTYPE )
      CALL JELIRA ( INTF//'.IDC_TYPE' , 'LONMAX' , NBT , K8B )
      IER = 0
      DO 100 IM = 0 , NBT-1
         IF ( ZK8(JTYPE+IM) .NE. 'MNEAL   ' ) THEN
            IER = IER + 1
            VALK (1) = ZK8(JTYPE+IM)
            CALL U2MESG('E', 'UTILITAI6_39',1,VALK,0,0,0,0.D0)
         ENDIF
 100  CONTINUE
      CALL ASSERT ( IER .EQ. 0 )
      CALL RSLIPA(BAMO,'TYPE_DEFO','&&IREDCA.LITDEF',JTYPE,NBM)
      NBMODE = 0
      DO 102 IM = 0 , NBM-1
         IF ( ZK16(JTYPE+IM)(1:6) .EQ. 'PROPRE' ) NBMODE = NBMODE + 1
 102  CONTINUE
      NBSTAT = NBM - NBMODE
C
C     --- CREATION NUMEROTATION CADYRO DES DDL ---
C
      CALL DISMOI('F','NOM_NUME_DDL',INTF,'INTERF_DYNA',IBID,NUMDDL,IER)
      NEQF = 6 * NBNOEU
      CALL WKVECT ('&&IREDCA.NUME_CADYRO', 'V V I', NEQF, JNUME )
      IER = 0
      DO 104 I = 1 , NBNOEU
         CALL JENUNO ( JEXNUM(NOMNOE,ZI(JNOEU+I-1)) , NONOEU )
         DO 106 J = 1 , 6
            CMP = NOMCMP(J)
            CALL POSDDL ('NUME_DDL', NUMDDL, NONOEU, CMP, NUNOE,NUDDL)
            IF ( NUNOE .EQ. 0 ) THEN
               IER = IER + 1
               VALK (1) = NONOEU
               CALL U2MESG('E', 'UTILITAI6_40',1,VALK,0,0,0,0.D0)
               GOTO 104
            ENDIF
            IF ( NUDDL .EQ. 0 ) THEN
               ZI(JNUME-1+6*(I-1)+J) = 0
            ELSE
               ZI(JNUME-1+6*(I-1)+J) = NUDDL
            ENDIF
 106     CONTINUE
 104  CONTINUE
      CALL ASSERT ( IER .EQ. 0 )
C
C              --- IMPRESSION DES MODES STATIQUES ---
C
      CALL GETVIS ( ' ', 'UNITE_MODE_STAT', 1,1,1, IFMS  , N1 )
      CALL GETVTX ( ' ', 'IMPR_MODE_STAT' , 1,1,1, OUINON, N1 )
      IF ( OUINON .EQ. 'NON' ) GOTO 200
      IF ( NBSTAT .EQ. 0 ) GOTO 200
      CALL WKVECT ( '&&IREDCA.NODDL_CRAY', 'V V I', 2*NBSTAT, JNODDL )
      CALL WKVECT ( '&&IREDCA.NODDL_IEEE', 'V V I', 2*NBSTAT, JNODDE )
      CALL RSLIPA(BAMO,'NOEUD_CMP','&&IREDCA.LINOEU',JNOEB,NBM)
      IER = 0
      INO = 0
      DO 110 IM = NBMODE+1 , NBM
         NONOEU = ZK16(JNOEB+IM-1)(1:8)
         CMP    = ZK16(JNOEB+IM-1)(9:16)
C A REMPLACER PAR UNE VERIFICATION EN TENANT COMPTE DES ROTATIONS !!
C         IF ( CMP .NE. 'DX'  .AND.  CMP .NE. 'DY' ) THEN
C            IER = IER + 1
C         ENDIF
         CALL JENONU ( JEXNOM(NOMNOE,NONOEU), NUMENO )
         DO 112 I = 1 , NBNOEU
            IF ( NUMENO .EQ. ZI(JNOEU+I-1) ) GOTO 114
 112     CONTINUE
         IER = IER + 1
            VALK (1) = NONOEU
            VALK (2) = NOMA
         CALL U2MESG('E', 'UTILITAI6_41',2,VALK,0,0,0,0.D0)
 114     CONTINUE
         INO = INO + 1
C         ZI(JNODDL+2*(INO-1)) = NUMENO
C --- DEBUT MODIF 2
CCC         ZI(JNODDL+2*(INO-1)) = I
CCC         IF ( CMP .EQ. 'DX'  ) THEN
CCC            ZI(JNODDL+2*(INO-1)+1) = 1
CCC         ELSEIF ( CMP .EQ. 'DY'  ) THEN
CCC            ZI(JNODDL+2*(INO-1)+1) = 2
CCC         ELSEIF ( CMP .EQ. 'DZ'  ) THEN
CCC            ZI(JNODDL+2*(INO-1)+1) = 3
CCC         ELSEIF ( CMP .EQ. 'DRX' ) THEN
CCC            ZI(JNODDL+2*(INO-1)+1) = 4
CCC         ELSEIF ( CMP .EQ. 'DRY' ) THEN
CCC            ZI(JNODDL+2*(INO-1)+1) = 5
CCC        ELSEIF ( CMP .EQ. 'DRZ' ) THEN
CCC            ZI(JNODDL+2*(INO-1)+1) = 6
CCC         ENDIF
         ZI(JNODDL+2*(INO-1)) = I
         IF ( CMP .EQ. 'DX'  ) THEN
            ZI(JNODDL+2*(INO-1)+1) = INDIRC(1)
         ELSEIF ( CMP .EQ. 'DY'  ) THEN
            ZI(JNODDL+2*(INO-1)+1) = INDIRC(2)
         ELSEIF ( CMP .EQ. 'DZ'  ) THEN
            ZI(JNODDL+2*(INO-1)+1) = INDIRC(3)
         ELSEIF ( CMP .EQ. 'DRX' ) THEN
            ZI(JNODDL+2*(INO-1)+1) = INDIRC(4)
         ELSEIF ( CMP .EQ. 'DRY' ) THEN
            ZI(JNODDL+2*(INO-1)+1) = INDIRC(5)
        ELSEIF ( CMP .EQ. 'DRZ' ) THEN
            ZI(JNODDL+2*(INO-1)+1) = INDIRC(6)
         ENDIF
C --- FIN MOFIF 2
 110  CONTINUE
      CALL ASSERT ( IER .EQ. 0 )
C
CCC      OPEN(UNIT=IFMS,ACCESS='DIRECT',RECL=4096)
      IF (BINAIR) THEN
      OPEN(UNIT=IFMS,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      ELSE
      OPEN(UNIT=IFMS,ACCESS='SEQUENTIAL',FORM='FORMATTED')
      ENDIF
C
      ICRAY(1) = NBSTAT
      ICRAY(2) = NEQF
      CALL ECRI3E ( 2 , ICRAY , IEEE )
CCC     IREC = 1
CCC      WRITE(UNIT=IFMS,REC=IREC) ( IEEE(I),I=1,2)
      IF (BINAIR) THEN
      WRITE(UNIT=IFMS) ( IEEE(I),I=1,2)
      ELSE
      WRITE(UNIT=IFMS, FMT=1100) (IEEE(I),I=1,2)
      ENDIF
C
      CALL ECRI3E ( 2*NBSTAT , ZI(JNODDL) , ZI(JNODDE) )
CCC      IREC = IREC + 1
CCC      WRITE(UNIT=IFMS,REC=IREC) ( ZI(JNODDE+I-1) , I=1,2*NBSTAT)
      IF (BINAIR) THEN
      WRITE(UNIT=IFMS) ( ZI(JNODDE+I-1) , I=1,2*NBSTAT)
      ELSE
      WRITE(UNIT=IFMS, FMT=1100) ( ZI(JNODDE+I-1) , I=1,2*NBSTAT)
      ENDIF
C
      CALL JEDETR ( '&&IREDCA.NODDL_CRAY' )
      CALL JEDETR ( '&&IREDCA.NODDL_IEEE' )
C
      CALL WKVECT ( '&&IREDCA.VALE_CRAY', 'V V R', NEQF, JCRAY )
      CALL WKVECT ( '&&IREDCA.VALE_IEEE', 'V V R', NEQF, JIEEE )
      DO 120 IM = NBMODE+1 , NBM
         CALL RSEXCH ( BAMO, 'DEPL', IM, CHAMNO, IER )
         IF ( IER .NE. 0 ) THEN
            VALK (1) = BAMO(1:8)
            VALI = IM
            CALL U2MESG('F', 'ALGORITH12_76',1,VALK,1,VALI,0,0.D0)
         ENDIF
         CALL JEVEUO ( CHAMNO//'.VALE' , 'L' , JVALE )
         DO 122 J = 1 , NEQF
            IAD = ZI(JNUME+J-1)
            IF ( IAD .NE. 0 ) THEN
               ZR(JCRAY+J-1) = ZR(JVALE+IAD-1)
            ELSE
               ZR(JCRAY+J-1) = 0.D0
            ENDIF
 122     CONTINUE
C
C ------ ROTATION DU CHAMP RESULTAT
C
         DO 30 I=1,NBNOEU
            DO 31 K=1,6
               X1(K)=ZR(JCRAY+6*(I-1)+K-1)
 31         CONTINUE
            DO 32 K=1,6
               XR1(K)=0.D0
               DO 33 L=1,6
                  XR1(K)=XR1(K)+MATROT(K,L)*X1(L)
 33            CONTINUE
 32         CONTINUE
            ZR(JCRAY+6*(I-1)  ) = XR1(1)
            ZR(JCRAY+6*(I-1)+1) = XR1(2)
            ZR(JCRAY+6*(I-1)+2) = XR1(3)
            ZR(JCRAY+6*(I-1)+3) = XR1(4)
            ZR(JCRAY+6*(I-1)+4) = XR1(5)
            ZR(JCRAY+6*(I-1)+5) = XR1(6)
C
C --- DEBUT MODIF 3
C
            IF ( CAS .NE. 0 )  ZR(JCRAY+6*(I-1)  ) = - XR1(1)
            IF ( CAS .EQ. 2 ) THEN
                ZR(JCRAY+6*(I-1)+1) = - XR1(2)
                ZR(JCRAY+6*(I-1)+2) = - XR1(3)
            ENDIF
C
C --- FIN MODIF 3
C
 30      CONTINUE

         CALL ECRR3E ( NEQF , ZR(JCRAY) , ZR(JIEEE) )
CCC         IREC = IREC + 1
CCC         WRITE(UNIT=IFMS,REC=IREC) ( ZR(JIEEE+I-1) , I=1,NEQF )
      IF (BINAIR) THEN
         WRITE(UNIT=IFMS) ( ZR(JIEEE+I-1) , I=1,NEQF )
      ELSE
         WRITE(UNIT=IFMS, FMT=1200) ( ZR(JIEEE+I-1) , I=1,NEQF )
      ENDIF
 120  CONTINUE
      CLOSE ( IFMS )
      CALL JEDETR ( '&&IREDCA.VALE_CRAY' )
      CALL JEDETR ( '&&IREDCA.VALE_IEEE' )
C
C              --- IMPRESSION DES MODES DYNAMIQUES ---
C
 200  CONTINUE
      CALL GETVIS ( ' ', 'UNITE_MODE_MECA', 1,1,1, IFMM  , N1 )
      CALL GETVTX ( ' ', 'IMPR_MODE_MECA' , 1,1,1, OUINON, N1 )
      IF ( OUINON .EQ. 'NON' ) GOTO 9999
C
      CALL WKVECT ( '&&IREDCA.VALE_IEEE', 'V V R', 2*NBMODE, JIEEE )
      CALL WKVECT ( '&&IREDCA.PARA_MODE', 'V V R', 2*NBMODE, JPARM )
      DO 210 IM = 1 , NBMODE
         CALL RSADPA ( BAMO, 'L', 1, 'FREQ', IM, 0, JPARA, K8B)
         ZR(JPARM+2*(IM-1)  ) = ZR(JPARA)
         CALL RSADPA ( BAMO, 'L', 1, 'MASS_GENE', IM, 0, JPARA, K8B)
         ZR(JPARM+2*(IM-1)+1) = ZR(JPARA)
 210  CONTINUE
C
CC      OPEN(UNIT=IFMM,ACCESS='DIRECT',RECL=4096)
      IF (BINAIR) THEN
      OPEN(UNIT=IFMS,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      ELSE
      OPEN(UNIT=IFMS,ACCESS='SEQUENTIAL',FORM='FORMATTED')
      ENDIF
C
      ICRAY(1) = NBMODE
      ICRAY(2) = NEQF
      CALL ECRI3E ( 2 , ICRAY , IEEE )
CCC      IREC = 1
CCC      WRITE(UNIT=IFMM,REC=IREC) ( IEEE(I) , I=1,2 )
      IF (BINAIR) THEN
      WRITE(UNIT=IFMM) ( IEEE(I) , I=1,2 )
      ELSE
      WRITE(UNIT=IFMM, FMT=1100) ( IEEE(I) , I=1,2 )
      ENDIF
C
      CALL ECRR3E ( 2*NBMODE , ZR(JPARM) , ZR(JIEEE) )
CCC      IREC = IREC + 1
CCC      WRITE(UNIT=IFMM,REC=IREC) ( ZR(JIEEE+I-1), I=1,2*NBMODE )
      IF (BINAIR) THEN
      WRITE(UNIT=IFMM) ( ZR(JIEEE+I-1), I=1,2*NBMODE )
      ELSE
      WRITE(UNIT=IFMM, FMT=1200) ( ZR(JIEEE+I-1), I=1,2*NBMODE )
      ENDIF
C
      CALL JEDETR ( '&&IREDCA.PARA_MODE' )
      CALL JEDETR ( '&&IREDCA.VALE_IEEE' )
C
      CALL WKVECT ( '&&IREDCA.VALE_CRAY', 'V V R', NEQF, JCRAY )
      CALL WKVECT ( '&&IREDCA.VALE_IEEE', 'V V R', NEQF, JIEEE )
      DO 212 IM = 1 , NBMODE
         CALL RSEXCH ( BAMO, 'DEPL', IM, CHAMNO, IER )
         IF ( IER .NE. 0 ) THEN
            VALK (1) = BAMO(1:8)
            VALI = IM
            CALL U2MESG('F', 'ALGORITH12_76',1,VALK,1,VALI,0,0.D0)
         ENDIF
         CALL JEVEUO ( CHAMNO//'.VALE' , 'L' , JVALE )
         DO 214 J = 1 , NEQF
            IAD = ZI(JNUME+J-1)
            IF ( IAD .NE. 0 ) THEN
               ZR(JCRAY+J-1) = ZR(JVALE+IAD-1)
            ELSE
               ZR(JCRAY+J-1) = 0.D0
            ENDIF
 214     CONTINUE
C
C ------ ROTATION DU CHAMP DE RESULTAT
C
         DO 40 I=1,NBNOEU
            DO 41 K=1,6
               X1(K)=ZR(JCRAY+6*(I-1)+K-1)
 41         CONTINUE
            DO 42 K=1,6
               XR1(K)=0.D0
               DO 43 L=1,6
                  XR1(K)=XR1(K)+MATROT(K,L)*X1(L)
 43            CONTINUE
 42         CONTINUE
            ZR(JCRAY+6*(I-1)  ) = XR1(1)
            ZR(JCRAY+6*(I-1)+1) = XR1(2)
            ZR(JCRAY+6*(I-1)+2) = XR1(3)
            ZR(JCRAY+6*(I-1)+3) = XR1(4)
            ZR(JCRAY+6*(I-1)+4) = XR1(5)
            ZR(JCRAY+6*(I-1)+5) = XR1(6)
C
C --- DEBUT MODIF 4
C
            IF ( CAS .NE. 0 )  ZR(JCRAY+6*(I-1)  ) = - XR1(1)
            IF ( CAS .EQ. 2 ) THEN
                ZR(JCRAY+6*(I-1)+1) = - XR1(2)
                ZR(JCRAY+6*(I-1)+2) = - XR1(3)
            ENDIF
C
C --- FIN MODIF 4
C
 40      CONTINUE
C
         CALL ECRR3E ( NEQF , ZR(JCRAY) , ZR(JIEEE) )
CCC         IREC = IREC + 1
CCC         WRITE(UNIT=IFMM,REC=IREC) ( ZR(JIEEE+I-1), I=1,NEQF )
      IF (BINAIR) THEN
      WRITE(UNIT=IFMM) ( ZR(JIEEE+I-1), I=1,NEQF )
      ELSE
      WRITE(UNIT=IFMM, FMT=1200) ( ZR(JIEEE+I-1), I=1,NEQF )
      ENDIF
 212  CONTINUE
      CLOSE ( IFMM )
      CALL JEDETR ( '&&IREDCA.VALE_CRAY' )
      CALL JEDETR ( '&&IREDCA.VALE_IEEE' )
C
C      CALL RELIT ( 'IEEE', IFMM, 1, NOMNOE, ZI(JNOEU) )
C
 9999 CONTINUE
CS
 1100 FORMAT(I6)
 1200 FORMAT(G14.7)
CS
      CALL JEDETR ( '&&IREDCA.LITDEF' )
      CALL JEDETR ( '&&IREDCA.LINOEU' )
      CALL JEDETR ( '&&IREDCA.NUME_CADYRO' )
      CALL JEDETR ( NOMVE1 )
      CALL JEDETR ( NOMVE2 )
      CALL JEDETR ( NOMVE3 )
      CALL JEDEMA ( )
      END
