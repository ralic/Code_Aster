      SUBROUTINE TE0580(OPTION,NOMTE)
C MODIF ELEMENTS  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C              SEE THE FILE "LICENSE.TERMS" FOR INFORMATION ON USAGE AND
C              REDISTRIBUTION OF THIS FILE.
C ======================================================================
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C     OPTION: SIGM_ELNO_COQU ET VARI_ELNO_COQU
C     ------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER JNUMC,JSIGM,ICOU,JNBSPI
      INTEGER NNO,NBCOU,NPG,NBSP,JCONT,NUSP,IPG,ICMP,JGEOM
      INTEGER NORDO,JTAB(7),LZR,LT2EV
      INTEGER JVARN,JVARI,ICM,NCMP,INO
      REAL*8 VPG(24),VNO(24),PGL(3,3)
      CHARACTER*16 OPTION,NOMTE
      CHARACTER*8  ELREFE
      CHARACTER*24 DESR
      LOGICAL GRILLE


      CALL ELREF1(ELREFE)

      IF (OPTION(1:14).EQ.'SIGM_ELNO_COQU') THEN
        CALL JEVECH('PSIGNOD','E',JSIGM)
        CALL JEVECH('PGEOMER','L',JGEOM)
      ELSE IF (OPTION(1:14).EQ.'VARI_ELNO_COQU') THEN
        CALL JEVECH('PVARINR','E',JVARN)
      END IF

      CALL JEVECH('PNUMCOR','L',JNUMC)

      IF (NOMTE.EQ.'MEDKTR3' .OR. NOMTE.EQ.'MEGRDKT') THEN
        NNO=3
        NPG=3
        IF (OPTION(1:14).EQ.'SIGM_ELNO_COQU') THEN
          CALL DXTPGL(ZR(JGEOM),PGL)
          LT2EV = 51
        END IF
      ELSE IF (NOMTE.EQ.'MEDKQU4 ') THEN
        NNO=4
        NPG=4
        IF (OPTION(1:14).EQ.'SIGM_ELNO_COQU') THEN
          CALL DXQPGL(ZR(JGEOM),PGL)
          LT2EV = 81
        END IF
      END IF
C
C
      IF (OPTION(1:14).EQ.'SIGM_ELNO_COQU') THEN
        DESR = '&INEL.'//ELREFE//'.DESR'
        CALL JEVETE(DESR,'L',LZR)
        CALL DXREPE(NNO,PGL,ZR(LZR))
      END IF

      IF (NOMTE(1:8).EQ.'MEGRDKT ') THEN
        GRILLE = .TRUE.
      ELSE
        GRILLE = .FALSE.
      END IF


      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU=ZI(JNBSPI-1+1)


      IF (OPTION(1:14).EQ.'SIGM_ELNO_COQU') THEN
        CALL TECACH(.TRUE.,.TRUE.,'PCONTRR',7,JTAB)
        JCONT = JTAB(1)
        NBSP  = JTAB(7)
      ELSE IF (OPTION(1:14).EQ.'VARI_ELNO_COQU') THEN
        CALL TECACH(.TRUE.,.TRUE.,'PVARIGR',7,JTAB)
        JVARI = JTAB(1)
        NCMP  = JTAB(6)
        NBSP  = JTAB(7)
      END IF


      ICOU = ZI(JNUMC)
      IF (ICOU.LE.0 .OR. ICOU.GT.NBCOU) CALL UTMESS('F','TE0580',
     +    ' NUME_COUCHE INCORRECT')

      NORDO = ZI(JNUMC+1)

      IF (GRILLE) THEN
        IF (NORDO.NE.0) CALL UTMESS('F','TE0580',
     +                           ' NIVE_COUCHE NE PEUT ETRE QUE "MOY"'
     +                              )
        NUSP=ICOU
      ELSE
        NUSP=3*(ICOU-1) + NORDO +2
      END IF



      IF (OPTION(1:14).EQ.'SIGM_ELNO_COQU') THEN
 
        DO 1, IPG=1,NPG
          DO 2,ICMP=1,6
            VPG(6*(IPG-1)+ICMP)= ZR( JCONT-1+(IPG-1)*6*NBSP + 
     +                              (NUSP-1)*6+ICMP )
 2        CONTINUE
          DO 3,ICMP=4,6
            VPG(6*(IPG-1)+ICMP)=VPG(6*(IPG-1)+ICMP)/SQRT(2.D0)
 3        CONTINUE
 1      CONTINUE
C       -- PASSAGE GAUSS -> NOEUDS :
        CALL PPGANO ( NNO , NPG , 6 , VPG ,  VNO )
C       -- PASSAGE DANS LE REPERE DE L'UTILISATEUR :
        CALL DXSIRO(NNO,ZR(LZR-1+LT2EV),VNO,ZR(JSIGM))

      ELSE IF (OPTION(1:14).EQ.'VARI_ELNO_COQU') THEN
        DO 4, ICMP=1,NCMP
          DO 5, IPG=1,NPG
            VPG(IPG) = ZR(JVARI-1+(IPG-1)*NCMP*NBSP+(NUSP-1)*NCMP+ICMP)
 5        CONTINUE
C         -- PASSAGE GAUSS -> NOEUDS :
          CALL PPGANO ( NNO , NPG , 1 , VPG ,  VNO )
          DO 6, INO=1,NNO
            ZR(JVARN-1 + (INO-1)*NCMP + ICMP) = VNO(INO)
 6        CONTINUE
 4      CONTINUE

      END IF


      END
