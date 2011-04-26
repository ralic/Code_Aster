      SUBROUTINE ASEXC2( MOTFAC,NBOCC,NBMODE,PARMOD,AMORT,CORFRE,NOMA,
     &                   NDIR,NOMSUP,NOMSPE,DIRSPE,ECHSPE,NATURE,
     &                   NBSUPM,NSUPP,KNOEU,KVSPE,KASPE )
      IMPLICIT  NONE
      INTEGER          NBOCC,NBMODE,NDIR(*),NATURE(3,*),NSUPP(*)
      REAL*8           PARMOD(NBMODE,*),AMORT(*),DIRSPE(3,*),ECHSPE(3,*)
      CHARACTER*8      NOMSUP(3,*),NOMSPE(3,*),NOMA
      CHARACTER*(*)    MOTFAC,KVSPE,KASPE,KNOEU
      LOGICAL          CORFRE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     COMMANDE : COMB_SISM_MODAL
C                TRAITEMENT DU MOT-CLE "EXCIT" POUR LE MONO-APPUI
C
C
C IN  : MOTFAC : MOT CLE FACTEUR
C IN  : NBOCC  : NOMBRE D'OCCURENCE DU MOT CLE FACTEUR
C IN  : NBMODE : NOMBRE DE MODES
C IN  : AMORT  : AMORTISSEMENTS MODAUX
C IN  : PARMOD : PARAMETRES MODAUX
C IN  : CORFRE : CORRECTION FREQUENCE SI .TRUE.
C OUT : NDIR   : DIRECTION DU SEISME A ETUDIER
C OUT : VALSPE : VALEURS DU SPECTRE
C OUT : ASYSPE : VALEURS ASYMPTOTIQUES DU SPECTRE
C
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*16               ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                 ZK32
      CHARACTER*80                                          ZK80
      COMMON  / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32       JEXNOM, JEXNUM
C
C ---------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      I, ID, IER, IFM, IGR, II, III, IM, INAT, INO, IOC
      INTEGER      IRET, IS, IUNIFI, J, JASPE, JDGN, JGRN, JKNO, JNOE
      INTEGER      JVAR1, JVSPE, N1, NBPT1, NBPT2, NBSUPM, NGR, NIMPR
      INTEGER      NNO

      REAL*8       AMOR, COEF, DEUXPI, ECHEL, EPSI, FREQ, DIRSP0(3)
      REAL*8       ECHSP0(3), VALPU(2), OMEGA, OMEGA2, R8B, R8DEPI
      REAL*8       RESU, UN, UNS2PI, XNORM, ZERO

      CHARACTER*1  K1B, DIR(3)
      CHARACTER*4  KNAT
      CHARACTER*8  K8B, SPECT, NOEU, GRNOEU, NOMSP0(3), NOMPU(2)
      CHARACTER*9  NIVEAU
      CHARACTER*24 VALE, OBJ1, OBJ2, VALK(2)
C
C     ------------------------------------------------------------------
C
      DATA   VALE / '                   .VALE' /
      DATA  NOMPU / 'AMOR' , 'FREQ'    /
      DATA   DIR  / 'X' , 'Y' , 'Z' /
C
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER    = 0
      IFM    = IUNIFI('RESULTAT')
      EPSI   = 1.D-03
      ZERO   = 0.D0
      UN     = 1.D0
      DEUXPI = R8DEPI()
      UNS2PI = UN / DEUXPI
      NSUPP(1) = 0
      NSUPP(2) = 0
      NSUPP(3) = 0
      OBJ1 = NOMA//'.GROUPENO'
      OBJ2 = NOMA//'.NOMNOE'
C
C     --- LECTURE MOT-CLE FACTEUR IMPRESSION ---
C
      CALL GETVTX('IMPRESSION','NIVEAU',1,1,1,NIVEAU,NIMPR)
      IF (NIMPR.EQ.0) NIVEAU='TOUT     '
C
C     --- NOMBRE DE SUPPORTS PAR DIRECTION ---
      DO 10 IOC = 1,NBOCC
C
         ECHSP0(1) = UN
         ECHSP0(2) = UN
         ECHSP0(3) = UN
         DIRSP0(1) = UN
         DIRSP0(2) = UN
         DIRSP0(3) = UN
         XNORM = UN
C
C        --- UN SPECTRE SUIVANT UN AXE ---
         CALL GETVR8(MOTFAC,'AXE',IOC,1,0,R8B,N1)
         IF (N1.NE.0) THEN
            CALL GETVR8(MOTFAC,'AXE' ,IOC,1,3,DIRSP0,N1)
            XNORM = ZERO
            DO 12 ID = 1,3
               XNORM = XNORM + DIRSP0(ID) * DIRSP0(ID)
 12         CONTINUE
            IF (XNORM.LT.EPSI) THEN
               IER = IER + 1
               CALL U2MESS('E','SEISME_4')
               GOTO 10
            ENDIF
            XNORM = UN / SQRT(XNORM)
            CALL GETVID(MOTFAC,'SPEC_OSCI',IOC,1,1,SPECT,N1)
            NOMSP0(1) = SPECT
            NOMSP0(2) = SPECT
            NOMSP0(3) = SPECT
            CALL GETVR8(MOTFAC,'ECHELLE',IOC,1,1,ECHEL,N1)
            IF (N1.NE.0) THEN
               ECHSP0(1) = ECHEL
               ECHSP0(2) = ECHEL
               ECHSP0(3) = ECHEL
            ENDIF
C
C        --- UN SPECTRE DANS LES 3 DIRECTIONS ---
         ELSE
         CALL GETVR8(MOTFAC,'TRI_AXE'  ,IOC,1,0,R8B,N1)
         IF (N1.NE.0) THEN
            CALL GETVR8(MOTFAC,'TRI_AXE'  ,IOC,1,3,DIRSP0,N1)
            CALL GETVID(MOTFAC,'SPEC_OSCI',IOC,1,1,SPECT ,N1)
            NOMSP0(1) = SPECT
            NOMSP0(2) = SPECT
            NOMSP0(3) = SPECT
            CALL GETVR8(MOTFAC,'ECHELLE',IOC,1,1,ECHEL,N1)
            IF (N1.NE.0) THEN
               ECHSP0(1) = ECHEL
               ECHSP0(2) = ECHEL
               ECHSP0(3) = ECHEL
            ENDIF
C
C        --- 3 SPECTRES DANS LES 3 DIRECTIONS ---
         ELSE

            CALL GETVID(MOTFAC,'SPEC_OSCI',IOC,1,3,NOMSP0,N1)
            CALL GETVR8(MOTFAC,'ECHELLE'  ,IOC,1,3,ECHSP0,N1)
C
         ENDIF
         ENDIF
C
         CALL GETVTX(MOTFAC,'NATURE',IOC,1,1,KNAT,N1)
         IF (KNAT.EQ.'ACCE') INAT = 1
         IF (KNAT.EQ.'VITE') INAT = 2
         IF (KNAT.EQ.'DEPL') INAT = 3
C
         DO 14 ID = 1,3
            DIRSP0(ID) = XNORM * DIRSP0(ID)
            IF (ABS(DIRSP0(ID)).GT.EPSI) THEN
               NDIR(ID) = 1
C
            CALL GETVEM(NOMA,'NOEUD',MOTFAC,'NOEUD',IOC,1,0,NOEU,N1)
            IF (N1.NE.0) THEN
               NNO = -N1
               CALL WKVECT('&&ASEXC2.NOEUD','V V K8',NNO,JNOE)
               CALL GETVEM(NOMA,'NOEUD',MOTFAC,'NOEUD',
     &                                  IOC,1,NNO,ZK8(JNOE),N1)
               DO 20 INO = 1, NNO
                  NOEU = ZK8(JNOE+INO-1)
                  CALL JENONU(JEXNOM(OBJ2,NOEU),IRET)
                  IF (IRET.EQ.0) THEN
                     IER = IER + 1
                     VALK(1) = NOEU
                     VALK(2) = NOMA
                     CALL U2MESK('E','SEISME_1', 2 ,VALK)
                     GOTO 20
                  ENDIF
                  DO 22 IS = 1,NSUPP(ID)
                     IF (NOMSUP(ID,IS).EQ.NOEU) THEN
                        IER = IER + 1
                        CALL U2MESK('E','SEISME_7',1,NOEU)
                        GOTO 20
                     ENDIF
 22               CONTINUE
                  NSUPP(ID) = NSUPP(ID) + 1
                  NOMSUP(ID,NSUPP(ID)) = NOEU
                  NOMSPE(ID,NSUPP(ID)) = NOMSP0(ID)
                  DIRSPE(ID,NSUPP(ID)) = DIRSP0(ID)
                  ECHSPE(ID,NSUPP(ID)) = ECHSP0(ID)
                  NATURE(ID,NSUPP(ID)) = INAT
 20            CONTINUE
               CALL JEDETR('&&ASEXC2.NOEUD')
C
            ELSE
               CALL GETVEM(NOMA,'GROUP_NO',MOTFAC,'GROUP_NO',
     &                                     IOC,1,0,K8B,N1)
               NGR = -N1
               CALL WKVECT('&&ASEXC2.GROUP_NO','V V K8',NGR,JGRN)
               CALL GETVEM(NOMA,'GROUP_NO',MOTFAC,'GROUP_NO',
     &                                     IOC,1,NGR,ZK8(JGRN),N1)
               DO 30 IGR = 1, NGR
                  GRNOEU = ZK8(JGRN+IGR-1)
                  CALL JEEXIN(JEXNOM(OBJ1,GRNOEU),IRET)
                  IF (IRET .EQ. 0) THEN
                     IER = IER + 1
                     VALK(1) = GRNOEU
                     VALK(2) = NOMA
                     CALL U2MESK('E','SEISME_2', 2 ,VALK)
                     GOTO 30
                  ENDIF
                  CALL JELIRA(JEXNOM(OBJ1,GRNOEU),'LONUTI',NNO,K1B)
                  CALL JEVEUO(JEXNOM(OBJ1,GRNOEU),'L',JDGN)
                  DO 32 INO = 1, NNO
                     CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN+INO-1)),NOEU)
                     DO 34 IS = 1,NSUPP(ID)
                        IF (NOMSUP(ID,IS).EQ.NOEU) THEN
                           IER = IER + 1
                           CALL U2MESK('E','SEISME_7',1,NOEU)
                           GOTO 32
                        ENDIF
 34                  CONTINUE
                     NSUPP(ID) = NSUPP(ID) + 1
                     NOMSUP(ID,NSUPP(ID)) = NOEU
                     NOMSPE(ID,NSUPP(ID)) = NOMSP0(ID)
                     DIRSPE(ID,NSUPP(ID)) = DIRSP0(ID)
                     ECHSPE(ID,NSUPP(ID)) = ECHSP0(ID)
                     NATURE(ID,NSUPP(ID)) = INAT
 32               CONTINUE
 30            CONTINUE
               CALL JEDETR('&&ASEXC2.GROUP_NO')
            ENDIF
            ENDIF
 14      CONTINUE
C
 10   CONTINUE
C
      IF (IER.NE.0) CALL U2MESS('F','SEISME_6')
C
C     --- NOM DES SUPPORTS PAR DIRECTION ---
      NBSUPM = MAX(NSUPP(1),NSUPP(2),NSUPP(3))
      CALL WKVECT( KNOEU ,'V V K8',3*NBSUPM,JKNO)
      DO 54 IS = 1,3*NBSUPM
         ZK8(JKNO+IS-1) = '        '
 54   CONTINUE
      DO 50 ID = 1,3
         I = NBSUPM*(ID-1)
         DO 52 IS = 1,NSUPP(ID)
            I = I + 1
            ZK8(JKNO+I-1) = NOMSUP(ID,IS)
 52      CONTINUE
 50   CONTINUE
C
C     --- INTERPOLATION DES SPECTRES ---
      IF (NIVEAU.EQ.'TOUT     ' .OR. NIVEAU.EQ.'SPEC_OSCI') THEN
         WRITE(IFM,1000)
         WRITE(IFM,1020)
      ENDIF
      CALL WKVECT( KVSPE ,'V V R',3*NBSUPM*NBMODE,JVSPE)
      DO 40 IM = 1,NBMODE
         II = 0
         AMOR     = AMORT(IM)
         OMEGA2   = PARMOD(IM,2)
         OMEGA    = SQRT( OMEGA2 )
         FREQ     = UNS2PI * OMEGA
         VALPU(1) = AMOR
         VALPU(2) = FREQ
         IF ( CORFRE ) VALPU(2) = VALPU(2) * SQRT( UN - AMOR*AMOR )
         DO 42 ID = 1,3
            III = 0
            IF (NDIR(ID).EQ.1) THEN
               DO 44 IS = 1,NSUPP(ID)
                  CALL FOINTE('F ',NOMSPE(ID,IS),2,NOMPU,VALPU,RESU,IER)
                  COEF  = DIRSPE(ID,IS)*ECHSPE(ID,IS)
                  RESU = RESU * COEF
                  IF (NATURE(ID,IS).EQ.2)  RESU = RESU * OMEGA
                  IF (NATURE(ID,IS).EQ.3)  RESU = RESU * OMEGA2
                  J = ID + 3*(IM-1) + 3*NBMODE*(IS-1)
                  ZR(JVSPE+J-1) = RESU
                  IF (NIVEAU.EQ.'TOUT     '
     &                               .OR. NIVEAU.EQ.'SPEC_OSCI') THEN
                    IF (II.EQ.0) THEN
                       II = 1
                       III = 1
                       WRITE(IFM,1200)IM,FREQ,AMOR,DIR(ID),
     &                                         NOMSUP(ID,IS),RESU
                    ELSE
                       IF (III.EQ.0) THEN
                          III = 1
                          WRITE(IFM,1210)DIR(ID),NOMSUP(ID,IS),RESU
                       ELSE
                          WRITE(IFM,1220)NOMSUP(ID,IS),RESU
                       ENDIF
                    ENDIF
                  ENDIF
 44            CONTINUE
            ENDIF
 42      CONTINUE
 40   CONTINUE
C
C     --- VALEURS ASYMPTOTIQUES DES SPECTRES ---
      IF (NIVEAU.EQ.'TOUT     ' .OR. NIVEAU.EQ.'SPEC_OSCI') THEN
         WRITE(IFM,1300)
         WRITE(IFM,1320)
      ENDIF
      CALL WKVECT( KASPE ,'V V R',3*NBSUPM,JASPE)
      DO 60 ID = 1,3
         J = NBSUPM*(ID-1)
         IF (NDIR(ID).EQ.1) THEN
            III = 0
            DO 62 IS = 1,NSUPP(ID)
               VALE(1:8) = NOMSPE(ID,IS)
               CALL JEVEUO(JEXNUM(VALE,1),'L',JVAR1)
               CALL JELIRA(JEXNUM(VALE,1),'LONMAX',NBPT1,K1B)
               NBPT2 = NBPT1 / 2
               OMEGA = DEUXPI * ZR(JVAR1+NBPT2-1)
               COEF  = DIRSPE(ID,IS)*ECHSPE(ID,IS)
               RESU = ZR(JVAR1+NBPT1-1) * COEF
               IF (NATURE(ID,IS).EQ.2)  RESU = RESU * OMEGA
               IF (NATURE(ID,IS).EQ.3)  RESU = RESU * OMEGA * OMEGA
               J = J + 1
               ZR(JASPE+J-1) = RESU
               IF (NIVEAU.EQ.'TOUT     '.OR.NIVEAU.EQ.'SPEC_OSCI') THEN
                  IF (III.EQ.0) THEN
                     III = 1
                     WRITE(IFM,1420)DIR(ID),NOMSUP(ID,IS),RESU
                  ELSE
                     WRITE(IFM,1430)NOMSUP(ID,IS),RESU
                  ENDIF
               ENDIF
 62         CONTINUE
         ENDIF
 60   CONTINUE
C
 1000 FORMAT(/,1X,'--- VALEURS DU SPECTRE ---')
 1020 FORMAT(1X,'MODE      FREQUENCE   AMORTISSEMENT   ',
     &          'DIR   SUPPORT         SPECTRE')
 1200 FORMAT(1P,1X,I4,3X,D12.5,4X,D12.5,4X,A1,4X,A8,3X,D12.5)
 1210 FORMAT(1P,40X,A1,4X,A8,3X,D12.5)
 1220 FORMAT(1P,45X,A8,3X,D12.5)
 1300 FORMAT(/,1X,'--- VALEURS ASYMPTOTIQUES DU SPECTRE ---')
 1320 FORMAT(1X,' DIRECTION   SUPPORT         SPECTRE')
 1420 FORMAT(1P,10X,A1,3X,A8,3X,D12.5)
 1430 FORMAT(1P,14X,A8,3X,D12.5)
C
      CALL JEDEMA()
      END
