      SUBROUTINE RCSTOC ( NOMMAT, NOMRC, NBOBJ, VALR, VALC, VALK,
     &                    NBR, NBC, NBK )
      IMPLICIT   NONE
      INTEGER            NBR, NBC, NBK, NBOBJ
      REAL*8             VALR(*)
      COMPLEX*16         VALC(*)
      CHARACTER*8        NOMMAT, VALK(*)
      CHARACTER*16       NOMRC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_20 CRS_513
C ----------------------------------------------------------------------
C     BUT: STOCKER DANS LES DEUX TABLEAUX VALR ET VALK LES REELS
C          ET LES K8 CARACTERISANT LA LOI DE COMPORTEMENT DE NOM NOMRC
C
C  IN  NOMMAT : NOM UTILISATEUR DU MATERIAU
C  IN  NOMRC  : NOM DE LA R.C.
C  IN  NBOBJ  : NOMBRE DE MCSIMPS
C  OUT VALR   : VECTEUR DES VALEURS REELLES
C  OUT VALK   : VECTEUR DES K8
C  OUT VALC   : VECTEUR DES COMPLEXES
C  OUT NBR    : NOMBRE DE REELS
C  OUT NBC    : NOMBRE DE COMPLEXES
C  OUT NBK    : NOMBRE DE CONCEPTS (FONCTION, TRC, TABLE, ... )
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      REAL*8             VALR8,E1,EI,PRECMA
      REAL*8             VALRR(4)
      CHARACTER*4        VALTX
      CHARACTER*8        VALCH,TYPFON,NOMPF(10),K8BID,CPROL,NOMCLE(5)
      CHARACTER*8        MCLE8,TABLE
      CHARACTER*19       RDEP,NOMFCT,CH19,MZP,NOMINT
      CHARACTER*24       PROL1,PROL2
      CHARACTER*24       VALKK(2)
      CHARACTER*16       TYPECO
      COMPLEX*16         VALC8
      INTEGER            NBPAR,JTYPO,JNOMO,JPARA,IBK,NBMAX
      INTEGER            VALI
      INTEGER            I,J,K,II,JFCT,JPRO,JRPV,JVALE,NBCOUP,N,NF
      INTEGER            IRET,NBFCT,NBPTS,JPROL,NBPTM
      INTEGER            IEXIST,LPRO1,LPRO2
      LOGICAL            EXIST,GETEXM
C ----------------------------------------------------------------------
C
      INTEGER            GTENV,FOUND,NPES,NBPTMS,ICOUNT
      CHARACTER*1        VNPES
      COMMON /ICOELJ/    NBPTMS
      FOUND = GTENV('NCPUS',VNPES)
      IF(FOUND.EQ.1) THEN
         READ(VNPES,'(I1)') NPES
      ELSE
         NPES=1
      ENDIF
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL WKVECT ( '&&RCSTOC.TYPOBJ', 'V V K8' , NBOBJ, JTYPO )
      CALL WKVECT ( '&&RCSTOC.NOMOBJ', 'V V K16', NBOBJ, JNOMO )
      CALL WKVECT ( '&&RCSTOC.PARAME', 'V V K8' , NBOBJ, JPARA )
      CALL GETMJM ( NOMRC,1,NBOBJ,ZK16(JNOMO),ZK8(JTYPO),N)

C     ON VERIFIE QUE 2 MOTS CLES DIFFERENTS N'ONT PAS LES MEMES
C     8 PREMIERS CARACTERES :
C     -----------------------------------------------------------
      CALL JECREO ( '&&RCSTOC.TEMPOR', 'V N K8')
      CALL JEECRA ( '&&RCSTOC.TEMPOR', 'NOMMAX', NBOBJ,K8BID)
      DO 777,I=1,NBOBJ

C        ON EST OBLIGE DE RECOPIER LA GLUTE ELAS_FLUI :
         IF ( ZK16(JNOMO+I-1) .EQ. 'PROF_RHO_F_INT' ) THEN
           MCLE8  = 'RHO_F_IN'
         ELSEIF ( ZK16(JNOMO+I-1) .EQ. 'PROF_RHO_F_EXT' ) THEN
           MCLE8 = 'RHO_F_EX'
         ELSEIF ( ZK16(JNOMO+I-1) .EQ. 'COEF_MASS_AJOU' ) THEN
           MCLE8  = 'CM'
         ELSE
           MCLE8= ZK16(JNOMO-1+I)(1:8)
         END IF

         CALL JEEXIN(JEXNOM('&&RCSTOC.TEMPOR',MCLE8),IRET)
         IF (IRET.GT.0) THEN
           CALL U2MESK('F','MODELISA6_69',1,ZK16(JNOMO-1+I))
         ELSE
           CALL JECROC(JEXNOM('&&RCSTOC.TEMPOR',MCLE8))
         END IF
777   CONTINUE
      CALL JEDETR('&&RCSTOC.TEMPOR')
C
      EXIST = GETEXM(NOMRC,'VERI_P')
      NBPAR = 0
      IF (EXIST) THEN
        CALL GETVTX(NOMRC,'VERI_P',1,1,0,K8BID,N)
        N = -N
        CALL GETVTX(NOMRC,'VERI_P',1,1,N,ZK8(JPARA+NBPAR),N)
        NBPAR = NBPAR + N
      ENDIF
C
      NBR = 0
      NBC = 0
      NBK = 0
C
C --- 0- GLUT META_MECA_FO ET BETON_DOUBLE_DP :
C --- ON TRAITE LES TX QUE ON CONVERTIT EN REELS
C
      DO 50  I = 1 , NBOBJ
       IF(ZK8(JTYPO+I-1)(1:2) .EQ. 'TX'  .AND.
     & (NOMRC .EQ. 'ELAS_META' .OR. NOMRC .EQ. 'ELAS_META_FO'))THEN
            CALL GETVTX ( NOMRC, ZK16(JNOMO+I-1), 1,1,1, VALTX, N )
            IF ( N .EQ. 1 ) THEN
               IF ( ZK16(JNOMO+I-1).EQ.'PHASE_REFE'  .AND.
     &                        VALTX.EQ.'CHAU'        ) THEN
                  NBR       = NBR + 1
                  VALR(NBR) = 1.D0
                  VALK(NBR) = ZK16(JNOMO+I-1)
               ELSEIF( ZK16(JNOMO+I-1).EQ.'PHASE_REFE'  .AND.
     &                           VALTX.EQ.'FROI'        ) THEN
                  NBR       = NBR + 1
                  VALR(NBR) = 0.D0
                  VALK(NBR) = ZK16(JNOMO+I-1)
               ENDIF
            ENDIF
         ENDIF
         IF ( ZK8(JTYPO+I-1)(1:2) .EQ. 'TX'  .AND.
     &        NOMRC .EQ. 'BETON_DOUBLE_DP' ) THEN
            CALL GETVTX ( NOMRC, ZK16(JNOMO+I-1), 1,1,1, VALTX, N )
            IF ( N .EQ. 1 ) THEN
               IF ( ZK16(JNOMO+I-1).EQ.'ECRO_COMP_P_PIC'
     &            .OR. ZK16(JNOMO+I-1).EQ.'ECRO_TRAC_P_PIC') THEN
                  NBR       = NBR + 1
                  VALK(NBR) = ZK16(JNOMO+I-1)
                  IF ( VALTX.EQ.'LINE'          ) THEN
                     VALR(NBR) = 0.D0
                  ELSE
                     VALR(NBR) = 1.D0
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
  50  CONTINUE
C
C --- 1- ON TRAITE LES REELS
C
      DO 100 I = 1 , NBOBJ
         IF ( ZK8(JTYPO+I-1)(1:3) .EQ. 'R8 ' ) THEN
            CALL GETVR8 ( NOMRC, ZK16(JNOMO+I-1), 1,1,1, VALR8, N )
            IF ( N .EQ. 1 ) THEN
               NBR = NBR + 1
               VALR(NBR) = VALR8
               VALK(NBR) = ZK16(JNOMO+I-1)
            ENDIF
         ENDIF
 100  CONTINUE
C
C
C --- 2- ON TRAITE LES COMPLEXES
C
      DO 115 I=1,NBOBJ
         IF ( ZK8(JTYPO+I-1)(1:3) .EQ. 'C8 ' ) THEN
            CALL GETVC8 ( NOMRC, ZK16(JNOMO+I-1), 1,1,1, VALC8, N )
            IF ( N .EQ. 1 ) THEN
               NBC = NBC + 1
               VALC(NBR+NBC) = VALC8
               VALK(NBR+NBC) = ZK16(JNOMO+I-1)
            ENDIF
         ENDIF
 115  CONTINUE
C
C
C --- 3- ON TRAITE ENSUITE LES CONCEPTS
C
      DO 110 I = 1 , NBOBJ
         IF ( ZK8(JTYPO+I-1)(1:3) .EQ. 'CO ' )THEN
            CALL GETVID ( NOMRC, ZK16(JNOMO+I-1), 1,1,1, VALCH, N )
            IF ( N .EQ. 1 ) THEN
               NBK = NBK + 1
               IF ( ZK16(JNOMO+I-1) .EQ. 'PROF_RHO_F_INT' ) THEN
                  VALK(NBR+NBC+NBK) = 'RHO_F_IN'
               ELSEIF ( ZK16(JNOMO+I-1) .EQ. 'PROF_RHO_F_EXT' ) THEN
                  VALK(NBR+NBC+NBK) = 'RHO_F_EX'
               ELSEIF ( ZK16(JNOMO+I-1) .EQ. 'COEF_MASS_AJOU' ) THEN
                  VALK(NBR+NBC+NBK) = 'CM'
               ELSE
                  VALK(NBR+NBC+NBK) = ZK16(JNOMO+I-1)
               ENDIF
            ENDIF
         ENDIF
 110  CONTINUE
C
      IBK = 0
      DO 120 I = 1 , NBOBJ
         IF ( ZK8(JTYPO+I-1)(1:3) .EQ. 'CO ' ) THEN
            CALL GETVID (NOMRC,ZK16(JNOMO+I-1),1,1,1,VALCH,N)
            IF ( N .EQ. 1 ) THEN
               CALL GETTCO ( VALCH, TYPECO )
               IBK = IBK + 1
               VALK(NBR+NBC+NBK+IBK) = VALCH
               IF ( TYPECO(1:8) .EQ. 'FONCTION' ) THEN
                  CH19 = VALCH
                  CALL JEVEUO ( CH19//'.PROL', 'L', JPRO )
                  CALL FONBPA ( CH19,ZK16(JPRO),TYPFON,10,NF,NOMPF)
                  DO 130 J = 1 , NF
                    DO 140 K = 1 , NBPAR
                     IF ( NOMPF(J) .EQ. ZK8(JPARA+K-1) ) GOTO 130
 140                CONTINUE
                    VALKK (1) = CH19(1:8)
                    CALL U2MESG('F+','MODELISA9_58',1,VALKK,0,0,0,0.D0)
                    DO 141 II = 1 , NF
                      VALKK(1) = NOMPF(II)
                  CALL U2MESG('F+','MODELISA9_91',1,VALKK,0,0,0,0.D0)
 141                CONTINUE
                   DO 142 II = 1 , NBPAR
                      VALKK(1) = ZK8(JPARA-1+II)
                   CALL U2MESG('F+','MODELISA9_92',1,VALKK,0,0,0,0.D0)
 142                CONTINUE
                    CALL U2MESS('F','MODELISA9_93')
 130              CONTINUE
               ENDIF
            ENDIF
         ENDIF
 120  CONTINUE
C
C --- 4- CREATION D'UNE FONCTION POUR STOCKER R(P)
C
       IF (( NOMRC(1:8)  .EQ. 'TRACTION'        ) .OR.
     &     ( NOMRC(1:13) .EQ. 'META_TRACTION' ) ) THEN
       IF ( NOMRC(1:8)  .EQ. 'TRACTION') THEN
              NOMCLE(1)(1:4)='SIGM'
       ENDIF
       IF ( NOMRC(1:13) .EQ. 'META_TRACTION') THEN
       NOMCLE(1)(1:7)='SIGM_F1'
       NOMCLE(2)(1:7)='SIGM_F2'
       NOMCLE(3)(1:7)='SIGM_F3'
       NOMCLE(4)(1:7)='SIGM_F4'
       NOMCLE(5)(1:7)='SIGM_C '
       ENDIF
       NBMAX = 0
       DO 149 II = 1, NBK
        DO 150 I=1,NBK
         IF ((VALK(NBR+NBC+I)(1:6) .EQ. 'SIGM  '). OR.
     &       (VALK(NBR+NBC+I)(1:7) .EQ. 'SIGM_F1'). OR.
     &       (VALK(NBR+NBC+I)(1:7) .EQ. 'SIGM_F2'). OR.
     &       (VALK(NBR+NBC+I)(1:7) .EQ. 'SIGM_F3'). OR.
     &       (VALK(NBR+NBC+I)(1:7) .EQ. 'SIGM_F4'). OR.
     &       (VALK(NBR+NBC+I)(1:7) .EQ. 'SIGM_C ')) THEN
          NOMFCT = VALK(NBR+NBC+NBK+I)
          GOTO 151
         ENDIF
 150    CONTINUE
        CALL U2MESK('F','MODELISA6_70',1,NOMCLE(II))
 151    CONTINUE

        CALL JEVEUO(NOMFCT//'.PROL','L',JFCT)
        IF (ZK16(JFCT)(1:1) .EQ. 'F' ) THEN
         CALL JELIRA(NOMFCT//'.VALE','LONMAX',NBPTM,K8BID)
        IF ( NOMRC(1:8)  .EQ. 'TRACTION') THEN
          IF ( NBPTM .LT. 4 ) THEN
           CALL U2MESK('F','MODELISA6_71',1,NOMCLE(II))
          ENDIF
        ENDIF
        IF ( NOMRC(1:13) .EQ. 'META_TRACTION') THEN
          IF ( NBPTM .LT. 2 ) THEN
           CALL U2MESK('F','MODELISA6_72',1,NOMCLE(II))
          ENDIF
        ENDIF
         NBCOUP = NBPTM / 2
        IF (NBPTM . GE . NBMAX ) NBMAX = NBPTM

         CALL JEVEUO(NOMFCT//'.VALE','L',JRPV)
         IF ( ZR(JRPV) .LE . 0.D0 ) THEN
           VALKK (1) = NOMCLE(II)
           VALKK (2) = NOMFCT
           VALRR (1) = ZR(JRPV)
          CALL U2MESG('F','MODELISA9_59',2,VALKK,0,0,1,VALRR)
         ENDIF
         IF ( ZR(JRPV+NBPTM/2) .LE. 0.D0 ) THEN
          VALKK (1) = NOMCLE(II)
          VALKK (2) = NOMFCT
          VALRR (1) = ZR(JRPV+NBPTM/2)
          CALL U2MESG('F','MODELISA9_60',2,VALKK,0,0,1,VALRR)
         ENDIF
C        VERIF ABSCISSES CROISSANTES (AU SENS LARGE)
         IRET=2
         CALL FOVERF(ZR(JRPV),NBCOUP,IRET)
         IRET = 0
         E1 = ZR(JRPV+NBCOUP) / ZR(JRPV)
         PRECMA = 1.D-10

        DO 200 I = 1 , NBCOUP-1
          EI = ( ZR(JRPV+NBCOUP+I) - ZR(JRPV+NBCOUP+I-1) ) /
     &         ( ZR(JRPV+I)        - ZR(JRPV+I-1)        )
          IF ( EI .GT. E1 ) THEN
           IRET = IRET + 1
           VALKK (1) = NOMCLE(II)
           VALRR (1) = E1
           VALRR (2) = EI
           VALRR (3) = ZR(JRPV+I)
           CALL U2MESG('E','MODELISA9_61',1,VALKK,0,0,3,VALRR)
          ELSEIF ( (E1-EI)/E1 .LE. PRECMA ) THEN
           VALKK (1) = NOMCLE(II)
           VALRR (1) = E1
           VALRR (2) = EI
           VALRR (3) = PRECMA
           VALRR (4) = ZR(JRPV+I)
           CALL U2MESG('A','MODELISA9_62',1,VALKK,0,0,4,VALRR)
          ENDIF
 200     CONTINUE
         IF ( IRET .NE. 0 ) THEN
          CALL U2MESS('F','MODELISA6_73')
         ENDIF
         CPROL = ZK16(JFCT+1)
        ELSE IF ( ZK16(JFCT)(1:1) .EQ. 'N' ) THEN
         CALL JELIRA(NOMFCT//'.VALE','NUTIOC',NBFCT,K8BID)
         NBPTM = 0
         DO 160 K=1,NBFCT
          CALL JELIRA(JEXNUM(NOMFCT//'.VALE',K),'LONMAX',NBPTS,K8BID)
          NBCOUP = NBPTS / 2
         IF (NBPTS . GE . NBMAX ) NBMAX = NBPTS
         IF ( NOMRC(1:8)  .EQ. 'TRACTION') THEN
           IF ( NBPTS .LT. 4 ) THEN
            CALL U2MESS('F','MODELISA6_74')
           ENDIF
         ENDIF
         IF ( NOMRC(1:13) .EQ. 'META_TRACTION') THEN
           IF ( NBPTS .LT. 2 ) THEN
            CALL U2MESK('F','MODELISA6_75',1,NOMCLE(II))
           ENDIF
         ENDIF
         CALL JEVEUO(JEXNUM(NOMFCT//'.VALE',K),'L',JRPV)
          IF ( ZR(JRPV) .LE . 0.D0 ) THEN
           VALI = K
           VALKK (1) = NOMCLE(II)
           VALKK (2) = NOMFCT
           VALRR (1) = ZR(JRPV)
           CALL U2MESG('F','MODELISA9_63',2,VALKK,1,VALI,1,VALRR)
          ENDIF
          IF ( ZR(JRPV+NBPTS/2) .LE . 0.D0 ) THEN
           VALI = K
           VALKK (1) = NOMCLE(II)
           VALKK (2) = NOMFCT
           VALRR (1) = ZR(JRPV+NBPTS/2)
           CALL U2MESG('F','MODELISA9_64',2,VALKK,1,VALI,1,VALRR)
          ENDIF
C         VERIF ABSCISSES CROISSANTES (AU SENS LARGE)
          IRET=2
          CALL FOVERF(ZR(JRPV),NBCOUP,IRET)
          IRET = 0
          E1 = ZR(JRPV+NBCOUP) / ZR(JRPV)
          DO 210 I = 1 , NBCOUP-1
           EI = ( ZR(JRPV+NBCOUP+I) - ZR(JRPV+NBCOUP+I-1) ) /
     &          ( ZR(JRPV+I)        - ZR(JRPV+I-1)        )
           IF ( EI .GT. E1 ) THEN
            IRET = IRET + 1
            VALKK (1) = NOMCLE(II)
            VALRR (1) = E1
            VALRR (2) = EI
            VALRR (3) = ZR(JRPV+I)
            CALL U2MESG('E','MODELISA9_65',1, VALKK,0,0,3,VALRR)
           ENDIF
 210      CONTINUE
          IF ( IRET .NE. 0 ) THEN
           CALL U2MESS('F','MODELISA6_73')
          ENDIF
 160     CONTINUE
         CPROL = ZK16(JFCT+7)
        ELSE
         CALL U2MESS('F','MODELISA6_76')
        ENDIF
 149   CONTINUE

       RDEP = NOMMAT//'.&&RDEP'
       CALL WKVECT (RDEP//'.PROL','G V K16',6*NPES,JPROL)
       DO 300 ICOUNT=1,NPES
        ZK16(JPROL  +(ICOUNT-1)*6 ) = 'FONCTION'
        ZK16(JPROL+1+(ICOUNT-1)*6 ) = 'LIN LIN '
       ZK16(JPROL+2+(ICOUNT-1)*6 ) = 'EPSI    '
        ZK16(JPROL+3+(ICOUNT-1)*6 ) = ZK16(JFCT+3)
 300   CONTINUE
       NBPTMS=NBPTM
       CALL WKVECT (RDEP//'.VALE','G V R',2*NBMAX*NPES,JVALE)
      ENDIF
C
C --- 5- CREATION D'UNE FONCTION POUR STOCKER MZ(P)
C
      IEXIST = 0
      IF ( NOMRC(1:8) .EQ. 'DIS_CONT'  ) THEN
        IEXIST = 1
        DO 400 I=1,NBK
          IF ( VALK(NBR+NBC+I)(1:7) .EQ. 'RELA_MZ' ) THEN
            IEXIST = 2
            NOMFCT = VALK(NBR+NBC+NBK+I)
            GOTO 451
          ENDIF
 400    CONTINUE
 451    CONTINUE
        IF (IEXIST .EQ. 2) THEN
        CALL JEVEUO(NOMFCT//'.PROL','L',JFCT)
        IF ( ZK16(JFCT)(1:1) .EQ. 'F' ) THEN
          CALL JELIRA(NOMFCT//'.VALE','LONMAX',NBPTM,K8BID)
          IF ( NBPTM .LT. 4 ) THEN
            CALL U2MESS('F','MODELISA6_77')
          ENDIF
          NBCOUP = NBPTM / 2
          CALL JEVEUO(NOMFCT//'.VALE','L',JRPV)
          IF ( ZR(JRPV) .LE . 0.D0 ) THEN
            VALKK (1) = NOMFCT
            VALRR (1) = ZR(JRPV)
            CALL U2MESG('F','MODELISA9_66',1,VALKK,0,0,1,VALRR)
          ENDIF
          IF ( ZR(JRPV+NBPTM/2) .LE. 0.D0 ) THEN
            VALKK (1) = NOMFCT
            VALRR (1) = ZR(JRPV+NBPTM/2)
            CALL U2MESG('F','MODELISA9_67',1,VALKK,0,0,1,VALRR)
          ENDIF
C         VERIF ABSCISSES CROISSANTES (AU SENS LARGE)
          IRET=2
          CALL FOVERF(ZR(JRPV),NBCOUP,IRET)
          IRET = 0
          E1 = ZR(JRPV+NBCOUP) / ZR(JRPV)
          PRECMA = 1.D-10
          DO 460 I = 1 , NBCOUP-1
             EI = ( ZR(JRPV+NBCOUP+I) - ZR(JRPV+NBCOUP+I-1) ) /
     &            ( ZR(JRPV+I)        - ZR(JRPV+I-1)        )
             IF ( EI .GT. E1 ) THEN
               IRET = IRET + 1
               VALRR (1) = E1
               VALRR (2) = EI
               VALRR (3) = ZR(JRPV+I)
               CALL U2MESG('E','MODELISA9_68',0,' ',0,0,3,VALRR)
             ELSEIF ( (E1-EI)/E1 .LE. PRECMA ) THEN
               VALRR (1) = E1
               VALRR (2) = EI
               VALRR (3) = PRECMA
               VALR (4) = ZR(JRPV+I)
               CALL U2MESG('A','MODELISA9_69',0,' ',0,0,4,VALRR)
             ENDIF
 460      CONTINUE
          IF ( IRET .NE. 0 ) THEN
            CALL U2MESS('F','MODELISA6_73')
          ENDIF
          CPROL = ZK16(JFCT+1)
        ELSE IF ( ZK16(JFCT)(1:1) .EQ. 'N' ) THEN
          CALL JELIRA(NOMFCT//'.VALE','NUTIOC',NBFCT,K8BID)
          NBPTM = 0
          DO 470 K=1,NBFCT
            CALL JELIRA(JEXNUM(NOMFCT//'.VALE',K),'LONMAX',NBPTS,K8BID)
            NBCOUP = NBPTS / 2
            IF ( NBPTS .LT. 4 ) THEN
             CALL U2MESS('F','MODELISA6_74')
            ENDIF
            CALL JEVEUO(JEXNUM(NOMFCT//'.VALE',K),'L',JRPV)
            IF ( ZR(JRPV) .LE . 0.D0 ) THEN
              VALI = K
              VALKK (1) = NOMFCT
              VALRR (1) = ZR(JRPV)
              CALL U2MESG('F','MODELISA9_70',1,VALKK,1,VALI,1,VALRR)
            ENDIF
            IF ( ZR(JRPV+NBPTS/2) .LE . 0.D0 ) THEN
              VALI = K
              VALKK (1) = NOMFCT
              VALRR (1) = ZR(JRPV+NBPTS/2)
              CALL U2MESG('F','MODELISA9_71',1,VALKK,1,VALI,1,VALRR)
            ENDIF
C           VERIF ABSCISSES CROISSANTES (AU SENS LARGE)
            IRET=2
            CALL FOVERF(ZR(JRPV),NBCOUP,IRET)
            IF ( IRET .NE. 0 ) THEN
              CALL U2MESS('F','MODELISA6_78')
            ENDIF
            IRET = 0
            E1 = ZR(JRPV+NBCOUP) / ZR(JRPV)
            DO 480 I = 1 , NBCOUP-1
              EI = ( ZR(JRPV+NBCOUP+I) - ZR(JRPV+NBCOUP+I-1) ) /
     &             ( ZR(JRPV+I)        - ZR(JRPV+I-1)        )
              IF ( EI .GT. E1 ) THEN
                IRET = IRET + 1
                VALRR (1) = E1
                VALRR (2) = EI
                VALRR (3) = ZR(JRPV+I)
                CALL U2MESG('E','MODELISA9_72',0,' ',0,0,3,VALRR)
              ENDIF
 480        CONTINUE
            IF ( IRET .NE. 0 ) THEN
              CALL U2MESS('F','MODELISA6_73')
            ENDIF
            NBPTM = MAX(NBPTM,NBPTS)
 470      CONTINUE
          CPROL = ZK16(JFCT+7)
        ELSE
          CALL U2MESS('F','MODELISA6_79')
        ENDIF
        MZP = NOMMAT//'.&&MZP'
C
        CALL WKVECT (MZP//'.PROL','G V K16',6*NPES,JPROL)
        DO 490 ICOUNT=1,NPES
           ZK16(JPROL  +(ICOUNT-1)*6 ) = 'FONCTION'
           ZK16(JPROL+1+(ICOUNT-1)*6 ) = CPROL
           ZK16(JPROL+2+(ICOUNT-1)*6 ) = ZK16(JFCT+2)
           ZK16(JPROL+3+(ICOUNT-1)*6 ) = ZK16(JFCT+3)
 490    CONTINUE
        NBPTMS=NBPTM
        CALL WKVECT (MZP//'.VALE','G V R',NBPTM*NPES,JVALE)
      ENDIF
      ENDIF
C
C --- 6 CREATION SI NECESSAIRE D'UNE FONCTION POUR STOCKER BETA
C       (ENTHALPIE VOLUMIQUE) CALCULEE A PARTIR DE RHO_CP
C
      IF ( NOMRC(1:8) .EQ. 'THER_NL'  ) THEN
        DO 650 I=1,NBK
          IF (( VALK(NBR+NBC+I)(1:4) .EQ. 'BETA' ) ) THEN
            NOMFCT = VALK(NBR+NBC+NBK+I)
C
C IL N'Y A RIEN A FAIRE, ON TRAVAILLE DIRECTEMENT AVEC BETA
C
            GOTO 651
          ENDIF
 650    CONTINUE
        DO 660 I=1,NBK
          IF (( VALK(NBR+NBC+I)(1:6) .EQ. 'RHO_CP' ) ) THEN
            NOMFCT = VALK(NBR+NBC+NBK+I)
            GOTO 661
          ENDIF
 660    CONTINUE
        GOTO 651
 661    CONTINUE
        CALL GCNCON ( '_' , NOMINT )
        CALL FOCAIN ('TRAPEZE',NOMFCT,0.D0,NOMINT,'G')
C
C SI PROLONGEMENT CONSTANT POUR RHO_CP : ON AFFECTE PROL LINEAIRE A BETA
C
        PROL1 = NOMFCT//'.PROL'
        CALL JEVEUO(PROL1,'L',LPRO1)
        PROL2 = NOMINT//'.PROL'
        CALL JEVEUO(PROL2,'E',LPRO2)
        IF (ZK16(LPRO1+4)(1:1).EQ.'C') ZK16(LPRO2+4)(1:1)='L'
        IF (ZK16(LPRO1+4)(2:2).EQ.'C') ZK16(LPRO2+4)(2:2)='L'
C
        DO 670 I=NBK,1,-1
          VALK(NBR+NBC+NBK+I+1) = VALK(NBR+NBC+NBK+I)
 670    CONTINUE
        NBK = NBK + 1
        VALK(NBR+NBC+  NBK) = 'BETA    '
        VALK(NBR+NBC+2*NBK) = NOMINT
 651    CONTINUE
      ENDIF
C
C --- 7 VERIFICATION DES NOMS DES PARAMETRES DES TABLES
      IF ( NOMRC(1:10) .EQ. 'META_ACIER')THEN
       DO 720 I=1,NBK
          IF (VALK(NBR+NBC+I)(1:3) .EQ. 'TRC') THEN
             CALL GETVID(NOMRC,'TRC',1,1,1,TABLE,N)
             CALL TBEXP2(TABLE,'VITESSE')
             CALL TBEXP2(TABLE,'PARA_EQ')
             CALL TBEXP2(TABLE,'COEF_0')
             CALL TBEXP2(TABLE,'COEF_1')
             CALL TBEXP2(TABLE,'COEF_2')
             CALL TBEXP2(TABLE,'COEF_3')
             CALL TBEXP2(TABLE,'COEF_4')
             CALL TBEXP2(TABLE,'COEF_5')
             CALL TBEXP2(TABLE,'NB_POINT')
             CALL TBEXP2(TABLE,'Z1')
             CALL TBEXP2(TABLE,'Z2')
             CALL TBEXP2(TABLE,'Z3')
             CALL TBEXP2(TABLE,'TEMP')
             CALL TBEXP2(TABLE,'SEUIL')
             CALL TBEXP2(TABLE,'AKM')
             CALL TBEXP2(TABLE,'BKM')
             CALL TBEXP2(TABLE,'TPLM')
             CALL TBEXP2(TABLE,'DREF')
             CALL TBEXP2(TABLE,'A')
          ENDIF
 720   CONTINUE
      ENDIF
C
      CALL JEDETR('&&RCSTOC.TYPOBJ')
      CALL JEDETR('&&RCSTOC.NOMOBJ')
      CALL JEDETR('&&RCSTOC.PARAME')
C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
