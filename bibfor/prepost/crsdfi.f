      SUBROUTINE CRSDFI(LINOCH,NBNOCH,NOIDEZ)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) LINOCH(1),NOIDEZ
      INTEGER NBNOCH

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C----------------------------------------------------------------------

C     CREATION DE LA SD "FORMAT_IDEAS"

C IN  : LINOCH : L_K16 : LISTE DES NOMS DE CHAMP ('DEPL','SIEF_ELNO')
C IN  : NBNOCH : I     : NOMBRE DE CHAMPS A LIRE
C IN  : NOIDEZ : K16   : NOM DE LA SD FORMAT_IDEAS
C IN  : NBVARI : I     : NOMBRE DE VARIABLES INTERNES A LIRE POUR LE
C                        CHAMP DE VARIABLES INTERNES (VARI_R)

C----------------------------------------------------------------------

      CHARACTER*16 PARAM


      INTEGER POSI(2),NPOSI
      INTEGER LFINOM,LFINUM,LFIPAR,LFILOC,LFINBC,LFICMP
      INTEGER IOCC,NBOCC,NVAL,NCH,REC(20)
      INTEGER I,ICH,IVAL,NB,NBCMP
      CHARACTER*8 CMP(1000)
      CHARACTER*16 BLANC,NOCHAM,NOIDEA
      INTEGER      IARG
      DATA BLANC/'                '/

      CALL JEMARQ()

C- CREATION DE LA SRUCTURE DE DONNEES "FORMAT_IDEAS"

      NOIDEA = NOIDEZ
      CALL WKVECT(NOIDEA//'.FID_NOM','V V K16',NBNOCH,LFINOM)
      CALL WKVECT(NOIDEA//'.FID_NUM','V V I',NBNOCH,LFINUM)
      CALL WKVECT(NOIDEA//'.FID_PAR','V V I',NBNOCH*800,LFIPAR)
      CALL WKVECT(NOIDEA//'.FID_LOC','V V I',NBNOCH*12,LFILOC)
      CALL WKVECT(NOIDEA//'.FID_CMP','V V K8',NBNOCH*1000,LFICMP)
      CALL WKVECT(NOIDEA//'.FID_NBC','V V I',NBNOCH,LFINBC)

C- INITIALISATION DES OBJETS :
C           '.FID_NOM' , '.FID_NUM'
C           '.FID_PAR' , '.FID_CMP'

      DO 10 I = 1,NBNOCH
        ZK16(LFINOM-1+I) = BLANC
   10 CONTINUE
      DO 20 I = 1,NBNOCH
        ZI(LFINUM-1+I) = 9999
   20 CONTINUE
      DO 30 I = 1,800*NBNOCH
        ZI(LFIPAR-1+I) = 9999
   30 CONTINUE

      CALL GETFAC('FORMAT_IDEAS',NBOCC)
C--------------------------------------------------------------------

C- REMPLISSAGE DES OBJETS A PARTIR DES DONNEES UTILISATEUR

C--------------------------------------------------------------------
      IF (NBOCC.NE.0) THEN

        DO 90 ICH = 1,NBNOCH

          DO 80 IOCC = 1,NBOCC
            CALL GETVTX('FORMAT_IDEAS','NOM_CHAM',IOCC,IARG,1,
     &                  NOCHAM,NCH)
            IF (NOCHAM.EQ.LINOCH(ICH)) THEN
C- NOM_CHAM
              ZK16(LFINOM-1+ICH) = LINOCH(ICH)
C- NUME_DATASET
              CALL GETVIS('FORMAT_IDEAS','NUME_DATASET',IOCC,IARG,1,
     &                    IVAL,
     &                    NVAL)
              IF (NVAL.NE.0) THEN
                ZI(LFINUM-1+ICH) = IVAL
              END IF
C- RECORD 3
              CALL GETVIS('FORMAT_IDEAS','RECORD_3',IOCC,IARG,0,REC,NB)
              NVAL = -NB
              CALL GETVIS('FORMAT_IDEAS','RECORD_3',IOCC,IARG,NVAL,
     &                    REC,NB)
              IF (NVAL.NE.0) THEN
                DO 40 I = 1,NVAL
                  ZI(LFIPAR-1+ (ICH-1)*800+80+I) = REC(I)
   40           CONTINUE
              END IF
C- RECORD 6
              CALL GETVIS('FORMAT_IDEAS','RECORD_6',IOCC,IARG,0,REC,NB)
              NVAL = -NB
              CALL GETVIS('FORMAT_IDEAS','RECORD_6',IOCC,IARG,NVAL,
     &                    REC,NB)
              IF (NVAL.NE.0) THEN
                DO 50 I = 1,NVAL
                  ZI(LFIPAR-1+ (ICH-1)*800+200+I) = REC(I)
   50           CONTINUE
              END IF
C- RECORD 9
              CALL GETVIS('FORMAT_IDEAS','RECORD_9',IOCC,IARG,0,REC,NB)
              NVAL = -NB
              CALL GETVIS('FORMAT_IDEAS','RECORD_9',IOCC,IARG,NVAL,
     &                    REC,NB)
              IF (NVAL.NE.0) THEN
                DO 60 I = 1,NVAL
                  ZI(LFIPAR-1+ (ICH-1)*800+320+I) = REC(I)
   60           CONTINUE
              END IF
C- POSI_ORDRE
              PARAM='POSI_ORDRE'
              CALL GETVIS('FORMAT_IDEAS',PARAM,IOCC,IARG,2,POSI,NPOSI)
              ZI(LFILOC-1+ (ICH-1)*12+1) = POSI(1)
              ZI(LFILOC-1+ (ICH-1)*12+2) = POSI(2)
C- POSI_INST
              PARAM='POSI_INST'
              CALL GETVIS('FORMAT_IDEAS',PARAM,IOCC,IARG,2,POSI,NPOSI)
              IF (NPOSI.EQ.2) THEN
                ZI(LFILOC-1+ (ICH-1)*12+3) = POSI(1)
                ZI(LFILOC-1+ (ICH-1)*12+4) = POSI(2)
              END IF
C- POSI_FREQ
              PARAM='POSI_FREQ'
              CALL GETVIS('FORMAT_IDEAS',PARAM,IOCC,IARG,2,POSI,NPOSI)
              IF (NPOSI.EQ.2) THEN
                ZI(LFILOC-1+ (ICH-1)*12+5) = POSI(1)
                ZI(LFILOC-1+ (ICH-1)*12+6) = POSI(2)
              END IF
C- POSI_NUME_MODE
              PARAM='POSI_NUME_MODE'
              CALL GETVIS('FORMAT_IDEAS',PARAM,IOCC,IARG,2,POSI,NPOSI)
              IF (NPOSI.EQ.2) THEN
                ZI(LFILOC-1+ (ICH-1)*12+7) = POSI(1)
                ZI(LFILOC-1+ (ICH-1)*12+8) = POSI(2)
              END IF
C- POSI_MASS_GENE
              PARAM='POSI_MASS_GENE'
              CALL GETVIS('FORMAT_IDEAS',PARAM,IOCC,IARG,2,POSI,NPOSI)
              IF (NPOSI.EQ.2) THEN
                ZI(LFILOC-1+ (ICH-1)*12+9) = POSI(1)
                ZI(LFILOC-1+ (ICH-1)*12+10) = POSI(2)
              END IF
C- POSI_AMOR_GENE
              PARAM='POSI_AMOR_GENE'
              CALL GETVIS('FORMAT_IDEAS',PARAM,IOCC,IARG,2,POSI,NPOSI)
              IF (NPOSI.EQ.2) THEN
                ZI(LFILOC-1+ (ICH-1)*12+11) = POSI(1)
                ZI(LFILOC-1+ (ICH-1)*12+12) = POSI(2)
              END IF


C- CMP ET NOMBRE DE COMPOSANTES
              CALL GETVTX('FORMAT_IDEAS','NOM_CMP',IOCC,IARG,0,CMP,NB)
              NBCMP = -NB
              CALL GETVTX('FORMAT_IDEAS','NOM_CMP',IOCC,IARG,NBCMP,
     &                    CMP,NB)
              IF (NB.NE.0) THEN
                DO 70 I = 1,NBCMP
                  ZK8(LFICMP-1+ (ICH-1)*1000+I) = CMP(I)
   70           CONTINUE
                ZI(LFINBC-1+ICH) = NBCMP
              END IF
            END IF
   80     CONTINUE
   90   CONTINUE
      END IF

C---------------------------------------------------------------------

C- REMPLISSAGE DES OBJETS A PARTIR DES VALEURS PAR DEFAUT

C---------------------------------------------------------------------
      DO 100 ICH = 1,NBNOCH
        IF (ZK16(LFINOM-1+ICH).EQ.BLANC) THEN
          NOCHAM = LINOCH(ICH)

C------------------------------- 'DEPL' --------------------------------

          IF (NOCHAM(1:4).EQ.'DEPL') THEN
C- NOM_CHAM
            ZK16(LFINOM-1+ICH) = NOCHAM
C- NUME_DATASET
            ZI(LFINUM-1+ICH) = 55
C- RECORD 6
            ZI(LFIPAR-1+ (ICH-1)*800+200+1) = 1
            ZI(LFIPAR-1+ (ICH-1)*800+200+3) = 3
            ZI(LFIPAR-1+ (ICH-1)*800+200+4) = 8
            ZI(LFIPAR-1+ (ICH-1)*800+200+5) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+6) = 6
C- POSI_ORDRE
            ZI(LFILOC-1+ (ICH-1)*12+1) = 7
            ZI(LFILOC-1+ (ICH-1)*12+2) = 4
C- POSI_INST
            ZI(LFILOC-1+ (ICH-1)*12+3) = 8
            ZI(LFILOC-1+ (ICH-1)*12+4) = 1
C- POSI_FREQ
            ZI(LFILOC-1+ (ICH-1)*12+5) = 8
            ZI(LFILOC-1+ (ICH-1)*12+6) = 1
C- NOM_CMP
            ZK8(LFICMP-1+ (ICH-1)*1000+1) = 'DX'
            ZK8(LFICMP-1+ (ICH-1)*1000+2) = 'DY'
            ZK8(LFICMP-1+ (ICH-1)*1000+3) = 'DZ'
            ZK8(LFICMP-1+ (ICH-1)*1000+4) = 'DRX'
            ZK8(LFICMP-1+ (ICH-1)*1000+5) = 'DRY'
            ZK8(LFICMP-1+ (ICH-1)*1000+6) = 'DRZ'
            ZI(LFINBC-1+ICH) = 6

C------------------------------- 'VITE' --------------------------------

          ELSE IF (NOCHAM(1:4).EQ.'VITE') THEN
C- NOM_CHAM
            ZK16(LFINOM-1+ICH) = NOCHAM
C- NUME_DATASET
            ZI(LFINUM-1+ICH) = 55
C- RECORD 6
            ZI(LFIPAR-1+ (ICH-1)*800+200+1) = 1
            ZI(LFIPAR-1+ (ICH-1)*800+200+3) = 3
            ZI(LFIPAR-1+ (ICH-1)*800+200+4) = 11
            ZI(LFIPAR-1+ (ICH-1)*800+200+5) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+6) = 6
C- POSI_ORDRE
            ZI(LFILOC-1+ (ICH-1)*12+1) = 7
            ZI(LFILOC-1+ (ICH-1)*12+2) = 4
C- POSI_INST
            ZI(LFILOC-1+ (ICH-1)*12+3) = 8
            ZI(LFILOC-1+ (ICH-1)*12+4) = 1
C- POSI_FREQ
            ZI(LFILOC-1+ (ICH-1)*12+5) = 8
            ZI(LFILOC-1+ (ICH-1)*12+6) = 1
C- NOM_CMP
            ZK8(LFICMP-1+ (ICH-1)*1000+1) = 'DX'
            ZK8(LFICMP-1+ (ICH-1)*1000+2) = 'DY'
            ZK8(LFICMP-1+ (ICH-1)*1000+3) = 'DZ'
            ZK8(LFICMP-1+ (ICH-1)*1000+4) = 'DRX'
            ZK8(LFICMP-1+ (ICH-1)*1000+5) = 'DRY'
            ZK8(LFICMP-1+ (ICH-1)*1000+6) = 'DRZ'

            ZI(LFINBC-1+ICH) = 6

C------------------------------- 'ACCE' --------------------------------

          ELSE IF (NOCHAM(1:4).EQ.'ACCE') THEN
C- NOM_CHAM
            ZK16(LFINOM-1+ICH) = NOCHAM
C- NUME_DATASET
            ZI(LFINUM-1+ICH) = 55
C- RECORD 6
            ZI(LFIPAR-1+ (ICH-1)*800+200+1) = 1
            ZI(LFIPAR-1+ (ICH-1)*800+200+3) = 3
            ZI(LFIPAR-1+ (ICH-1)*800+200+4) = 12
            ZI(LFIPAR-1+ (ICH-1)*800+200+5) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+6) = 6
C- POSI_ORDRE
            ZI(LFILOC-1+ (ICH-1)*12+1) = 7
            ZI(LFILOC-1+ (ICH-1)*12+2) = 4
C- POSI_INST
            ZI(LFILOC-1+ (ICH-1)*12+3) = 8
            ZI(LFILOC-1+ (ICH-1)*12+4) = 1
C- POSI_FREQ
            ZI(LFILOC-1+ (ICH-1)*12+5) = 8
            ZI(LFILOC-1+ (ICH-1)*12+6) = 1
C- NOM_CMP
            ZK8(LFICMP-1+ (ICH-1)*1000+1) = 'DX'
            ZK8(LFICMP-1+ (ICH-1)*1000+2) = 'DY'
            ZK8(LFICMP-1+ (ICH-1)*1000+3) = 'DZ'
            ZK8(LFICMP-1+ (ICH-1)*1000+4) = 'DRX'
            ZK8(LFICMP-1+ (ICH-1)*1000+5) = 'DRY'
            ZK8(LFICMP-1+ (ICH-1)*1000+6) = 'DRZ'

            ZI(LFINBC-1+ICH) = 6

C------------------------------- 'TEMP' --------------------------------

          ELSE IF (NOCHAM(1:4).EQ.'TEMP') THEN
C- NOM_CHAM
            ZK16(LFINOM-1+ICH) = NOCHAM
C- NUME_DATASET
            ZI(LFINUM-1+ICH) = 55
C- RECORD 6
            ZI(LFIPAR-1+ (ICH-1)*800+200+1) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+2) = 4
            ZI(LFIPAR-1+ (ICH-1)*800+200+3) = 1
            ZI(LFIPAR-1+ (ICH-1)*800+200+4) = 5
            ZI(LFIPAR-1+ (ICH-1)*800+200+5) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+6) = 1
C- POSI_ORDRE
            ZI(LFILOC-1+ (ICH-1)*12+1) = 7
            ZI(LFILOC-1+ (ICH-1)*12+2) = 4
C- POSI_INST
            ZI(LFILOC-1+ (ICH-1)*12+3) = 8
            ZI(LFILOC-1+ (ICH-1)*12+4) = 1
C- POSI_FREQ
            ZI(LFILOC-1+ (ICH-1)*12+5) = 8
            ZI(LFILOC-1+ (ICH-1)*12+6) = 1
C- NOM_CMP
            ZK8(LFICMP-1+ (ICH-1)*1000+1) = 'TEMP'

            ZI(LFINBC-1+ICH) = 1

C------------------------------- 'VARI_ELNO' ------------------------

          ELSE IF (NOCHAM(1:4).EQ.'VARI') THEN
C- NOM_CHAM
            ZK16(LFINOM-1+ICH) = NOCHAM
C- NUME_DATASET
            ZI(LFINUM-1+ICH) = 57
C- RECORD 6
            ZI(LFIPAR-1+ (ICH-1)*800+200+1) = 1
            ZI(LFIPAR-1+ (ICH-1)*800+200+2) = 4
            ZI(LFIPAR-1+ (ICH-1)*800+200+3) = 3
            ZI(LFIPAR-1+ (ICH-1)*800+200+4) = 0
            ZI(LFIPAR-1+ (ICH-1)*800+200+5) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+6) = 6
C- POSI_ORDRE
            ZI(LFILOC-1+ (ICH-1)*12+1) = 7
            ZI(LFILOC-1+ (ICH-1)*12+2) = 4
C- POSI_INST
            ZI(LFILOC-1+ (ICH-1)*12+3) = 8
            ZI(LFILOC-1+ (ICH-1)*12+4) = 1
C- POSI_FREQ
            ZI(LFILOC-1+ (ICH-1)*12+5) = 8
            ZI(LFILOC-1+ (ICH-1)*12+6) = 1
C- NOM_CMP
            ZK8(LFICMP-1+ (ICH-1)*1000+1) = 'V1'
            ZK8(LFICMP-1+ (ICH-1)*1000+2) = 'V2'
            ZK8(LFICMP-1+ (ICH-1)*1000+3) = 'V3'
            ZK8(LFICMP-1+ (ICH-1)*1000+4) = 'V4'
            ZK8(LFICMP-1+ (ICH-1)*1000+5) = 'V5'
            ZK8(LFICMP-1+ (ICH-1)*1000+6) = 'V6'
            ZK8(LFICMP-1+ (ICH-1)*1000+7) = 'V7'
            ZK8(LFICMP-1+ (ICH-1)*1000+8) = 'V8'
            ZK8(LFICMP-1+ (ICH-1)*1000+9) = 'V9'
            ZK8(LFICMP-1+ (ICH-1)*1000+10) = 'V10'
            ZK8(LFICMP-1+ (ICH-1)*1000+11) = 'V11'
            ZK8(LFICMP-1+ (ICH-1)*1000+12) = 'V12'
            ZK8(LFICMP-1+ (ICH-1)*1000+13) = 'V13'
            ZK8(LFICMP-1+ (ICH-1)*1000+14) = 'V14'
            ZK8(LFICMP-1+ (ICH-1)*1000+15) = 'V15'
            ZK8(LFICMP-1+ (ICH-1)*1000+16) = 'V16'
            ZK8(LFICMP-1+ (ICH-1)*1000+17) = 'V17'
            ZK8(LFICMP-1+ (ICH-1)*1000+18) = 'V18'
            ZK8(LFICMP-1+ (ICH-1)*1000+19) = 'V19'
            ZK8(LFICMP-1+ (ICH-1)*1000+20) = 'V20'
            ZK8(LFICMP-1+ (ICH-1)*1000+21) = 'V21'
            ZK8(LFICMP-1+ (ICH-1)*1000+22) = 'V22'
            ZK8(LFICMP-1+ (ICH-1)*1000+23) = 'V23'
            ZK8(LFICMP-1+ (ICH-1)*1000+24) = 'V24'
            ZK8(LFICMP-1+ (ICH-1)*1000+25) = 'V25'
            ZK8(LFICMP-1+ (ICH-1)*1000+26) = 'V26'
            ZK8(LFICMP-1+ (ICH-1)*1000+27) = 'V27'
            ZK8(LFICMP-1+ (ICH-1)*1000+28) = 'V28'
            ZK8(LFICMP-1+ (ICH-1)*1000+29) = 'V29'
            ZK8(LFICMP-1+ (ICH-1)*1000+30) = 'V30'

            ZI(LFINBC-1+ICH) = 30

C------------------------------- 'EPSA_ELNO' ------------------------

          ELSE IF (NOCHAM(1:4).EQ.'EPSA') THEN
C- NOM_CHAM
            ZK16(LFINOM-1+ICH) = NOCHAM
C- NUME_DATASET
            ZI(LFINUM-1+ICH) = 57
C- RECORD 6
            ZI(LFIPAR-1+ (ICH-1)*800+200+1) = 1
            ZI(LFIPAR-1+ (ICH-1)*800+200+2) = 4
            ZI(LFIPAR-1+ (ICH-1)*800+200+3) = 4
            ZI(LFIPAR-1+ (ICH-1)*800+200+4) = 3
            ZI(LFIPAR-1+ (ICH-1)*800+200+5) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+6) = 6
C- POSI_ORDRE
            ZI(LFILOC-1+ (ICH-1)*12+1) = 7
            ZI(LFILOC-1+ (ICH-1)*12+2) = 4
C- POSI_INST
            ZI(LFILOC-1+ (ICH-1)*12+3) = 8
            ZI(LFILOC-1+ (ICH-1)*12+4) = 1
C- POSI_FREQ
            ZI(LFILOC-1+ (ICH-1)*12+5) = 8
            ZI(LFILOC-1+ (ICH-1)*12+6) = 1
C- NOM_CMP
            ZK8(LFICMP-1+ (ICH-1)*1000+1) = 'EPXX'
            ZK8(LFICMP-1+ (ICH-1)*1000+2) = 'EPXY'
            ZK8(LFICMP-1+ (ICH-1)*1000+3) = 'EPYY'
            ZK8(LFICMP-1+ (ICH-1)*1000+4) = 'EPXZ'
            ZK8(LFICMP-1+ (ICH-1)*1000+5) = 'EPYZ'
            ZK8(LFICMP-1+ (ICH-1)*1000+6) = 'EPZZ'

            ZI(LFINBC-1+ICH) = 6

C------------------------------- 'SIEF_ELNO' ------------------------

          ELSE IF (NOCHAM(1:4).EQ.'SIEF') THEN
C- NOM_CHAM
            ZK16(LFINOM-1+ICH) = NOCHAM
C- NUME_DATASET
            ZI(LFINUM-1+ICH) = 57
C- RECORD 6
            ZI(LFIPAR-1+ (ICH-1)*800+200+1) = 1
            ZI(LFIPAR-1+ (ICH-1)*800+200+2) = 4
            ZI(LFIPAR-1+ (ICH-1)*800+200+3) = 4
            ZI(LFIPAR-1+ (ICH-1)*800+200+4) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+5) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+6) = 6
C- POSI_ORDRE
            ZI(LFILOC-1+ (ICH-1)*12+1) = 7
            ZI(LFILOC-1+ (ICH-1)*12+2) = 4
C- POSI_INST
            ZI(LFILOC-1+ (ICH-1)*12+3) = 8
            ZI(LFILOC-1+ (ICH-1)*12+4) = 1
C- POSI_FREQ
            ZI(LFILOC-1+ (ICH-1)*12+5) = 8
            ZI(LFILOC-1+ (ICH-1)*12+6) = 1
C- NOM_CMP
            ZK8(LFICMP-1+ (ICH-1)*1000+1) = 'SIXX'
            ZK8(LFICMP-1+ (ICH-1)*1000+2) = 'SIXY'
            ZK8(LFICMP-1+ (ICH-1)*1000+3) = 'SIYY'
            ZK8(LFICMP-1+ (ICH-1)*1000+4) = 'SIXZ'
            ZK8(LFICMP-1+ (ICH-1)*1000+5) = 'SIYZ'
            ZK8(LFICMP-1+ (ICH-1)*1000+6) = 'SIZZ'

            ZI(LFINBC-1+ICH) = 6


C------------------------------- 'PRES'-----------------------------

          ELSE IF (NOCHAM(1:4).EQ.'PRES') THEN
C- NOM_CHAM
            ZK16(LFINOM-1+ICH) = NOCHAM
C- NUME_DATASET
            ZI(LFINUM-1+ICH) = 57
C- RECORD 6
            ZI(LFIPAR-1+ (ICH-1)*800+200+1) = 1
            ZI(LFIPAR-1+ (ICH-1)*800+200+2) = 4
            ZI(LFIPAR-1+ (ICH-1)*800+200+3) = 1
            ZI(LFIPAR-1+ (ICH-1)*800+200+4) = 15
            ZI(LFIPAR-1+ (ICH-1)*800+200+5) = 2
            ZI(LFIPAR-1+ (ICH-1)*800+200+6) = 1
C- POSI_ORDRE
            ZI(LFILOC-1+ (ICH-1)*12+1) = 7
            ZI(LFILOC-1+ (ICH-1)*12+2) = 4
C- POSI_INST
            ZI(LFILOC-1+ (ICH-1)*12+3) = 8
            ZI(LFILOC-1+ (ICH-1)*12+4) = 1
C- POSI_FREQ
            ZI(LFILOC-1+ (ICH-1)*12+5) = 8
            ZI(LFILOC-1+ (ICH-1)*12+6) = 1
C- NOM_CMP
            ZK8(LFICMP-1+ (ICH-1)*1000+1) = 'PRES'

            ZI(LFINBC-1+ICH) = 1
          END IF
        END IF

  100 CONTINUE

      CALL JEDEMA()

      END
