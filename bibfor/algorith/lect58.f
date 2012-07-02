      SUBROUTINE LECT58 (IDEAS,NOMRES,MAIL,TYPRES,ACCES,LISTR8,
     &                   LISTIS,PRECIS,CRIT,EPSI,LINOCH,NBNOCH)
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     LECT58 : LECTURE FICHIER FORMAT UNIVERSEL DATASET 58
C
C     IN : IDEAS : NUMERO LOGIQUE DU FICHIER UNV
C     IN : NOMRES : NOM DE LA SD RESULTATS
C     IN : MAIL : NOM DU MAILLAGE
C     IN : TYPRES : TYPE DE RESULTAT ('EVOL_ELAS','DYNA_TRANS')
C     IN : ACCES : TYPE D'ACCES ('TOUT_ORDRE','NUME_ORDRE','INST',...)
C     IN : LISTR8 : NOM DE L'OBJET CONTENANT LA LISTE DES INSTANTS
C                        OU DES FREQUENCES A LIRE
C     IN : LISTIS : NOM DE L'OBJET CONTENANT LA LISTE DES
C                        NUMEROS D'ORDRE A LIRE
C     IN : PRECIS : INDICATEUR DE VERIFICATION DE LA PRECISION
C     IN : CRIT : PRECISION : CRITERE RELATIF OU ABSOLU
C     IN : EPSI : PRECISION DEMANDEE
C     IN : LINOCH : L_K16 : LISTE DES NOMS DE CHAMP ('DEPL',...)
C     IN : NBNOCH : I     : NOMBRE DE CHAMPS A LIRE
C     -----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      LOGICAL ASTOCK
      INTEGER IDEAS,PRECIS,NBNOCH
      CHARACTER*(*) NOMRES,MAIL,TYPRES,ACCES,LISTR8,LISTIS,CRIT
      CHARACTER*(*) LINOCH(*)
      REAL*8 EPSI
C
C
C
C
C
      CHARACTER*1   PRFNOE
      CHARACTER*6   KAR
      CHARACTER*8   K8BID, LABK8, NOMGD, LICMP(30), MAILLA
      CHARACTER*16  NOMSYM,MOTCLE(1),TYMOCL(1)
      CHARACTER*19  CNS,NOMCH,PRFCHN
      CHARACTER*24  VABS,VORI,VCOR,VALMES,NOOJB
      CHARACTER*80  LIGNE, REPEM1, REC(20)
      INTEGER NBABS, ITYPE, IDIR, NBNMES, ICHAMP, NBMESU
      INTEGER VALI,INOCH,ICHAM0
      INTEGER LABEL, LCORR, IBID, LORI
      INTEGER NBREC,IFIELD,NBABS1,INATUR,INATU1
      INTEGER LVALC,LVALR,LABS
      INTEGER IREC,IRET,IFRES,IUNIFI
      INTEGER NBOCC,IOCC,NBNO2,IAGNO2,I,ICMPM
      INTEGER NUMORD,JCNSV,JCNSL,IMES,ICMP,INO,IVAL,JABS,NCMP
      REAL*8 AMIN,APAS,RBID,RVAL,DIR(3)
      COMPLEX*16 CVAL,CZERO,CUN
      LOGICAL TROUVE,ZCMPLX,FICAB,FICVA,VUCONT,VUDEF
      INTEGER      IARG
C
C----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      ICHAM0 = 0
      INATU1 = 0
      NBABS1 = 0
      PRFNOE='N'

      REPEM1 (  1 : 50 ) =
     & '    -1                                            '
      REPEM1 ( 51 : 80 ) =
     & '                              '
      MAILLA = MAIL
      CVAL = DCMPLX(0.D0,0.D0)
      CZERO = DCMPLX(0.D0,0.D0)
      CUN = DCMPLX(1.D0,0.D0)
      FICAB = .FALSE.
      FICVA = .FALSE.
      VABS = '&&ABSCISSES'
      VORI = '&&ORIENTATIONS'
      VCOR = '&&CORRESPONDANCE'
      VALMES = '&&VALEURSMESUREES'
      CNS = '&&CNS'
C
C RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE : NBNMES
      CALL DISMOI('F','NB_NO_MAILLA',MAILLA,'MAILLAGE',
     &                   NBNMES,K8BID,IBID)
C
C VECTEUR DES NUMEROS DES NOEUDS MESURE SELON L ORDRE FICHIER UNV
      CALL WKVECT ( VCOR , 'V V I' , NBNMES*6 , LCORR )
C
C VECTEUR DES ORIENTATIONS DE MESURE SELON L ORDRE DU FICHIER UNV
      CALL WKVECT ( VORI , 'V V I' , NBNMES*6 , LORI  )
C
C  BOUCLE SUR LES CHAMPS
      DO 500 INOCH=1,NBNOCH
        NBMESU = 0
        NOMSYM = LINOCH(INOCH)
        IF (NOMSYM .EQ. 'SIEF_NOEU') ICHAM0 = 2
        IF (NOMSYM .EQ. 'EPSI_NOEU') ICHAM0 = 3
        IF (NOMSYM .EQ. 'DEPL') ICHAM0 = 8
        IF (NOMSYM .EQ. 'VITE') ICHAM0 = 11
        IF (NOMSYM .EQ. 'ACCE') ICHAM0 = 12

      REWIND IDEAS

   10 CONTINUE
      READ (IDEAS, 1000, END = 170) LIGNE
      IF (LIGNE .NE. REPEM1) GO TO 10

      READ (IDEAS, '(A6)', END = 170, ERR = 160) KAR
      IF (KAR.EQ.'    58') THEN
        NBREC = 11
      ELSE
C POSITIONNEMENT A LA FIN DU DATASET
        GO TO 11
      END IF

C LECTURE DE L'ENTETE DU DATASET
      DO 20 IREC = 1,NBREC
        READ (IDEAS,'(A80)',ERR=160) REC(IREC)
   20 CONTINUE

C RECHERCHE DU NOMBRE DE VALEURS CONTENUES DANS LE DATASET
      IREC = 7
      IFIELD = 2
      CALL DECOD2(REC,IREC,IFIELD,0,NBABS,RBID,TROUVE)
      IF (.NOT. FICAB) THEN
        CALL WKVECT(VABS,'V V R',NBABS,LABS)
        NBABS1 = NBABS
        FICAB = .TRUE.
      ELSE
        IF (NBABS .NE. NBABS1) THEN
          CALL U2MESS('F','ALGORITH4_98')
        END IF
      END IF
C
C- RECHERCHE DE LA NATURE DU CHAMP
C   REEL     --> INATUR = 2,4
C   COMPLEXE --> INATUR = 5,6
      IFIELD = 1
      CALL DECOD2(REC,IREC,IFIELD,0,INATUR,RBID,TROUVE)
      IF (NBMESU .EQ. 0) THEN
        INATU1 = INATUR
      ELSE
        IF (INATUR .NE. INATU1) THEN
          CALL U2MESS('F','ALGORITH4_99')
        END IF
      END IF
      IF (INATUR.EQ.5 .OR. INATUR.EQ.6) THEN
        IF (TYPRES(1:6) .EQ. 'DYNA_T')  CALL U2MESS('F','ALGORITH5_1')
        ZCMPLX = .TRUE.
        IF (.NOT. FICVA) THEN
          CALL WKVECT(VALMES,'V V C',NBABS*NBNMES*3,LVALC)
          FICVA = .TRUE.
        END IF
      ELSE
        IF (TYPRES(1:6) .EQ. 'DYNA_H') CALL U2MESS('F','ALGORITH5_2')
        ZCMPLX = .FALSE.
        IF (.NOT. FICVA) THEN
          CALL WKVECT(VALMES,'V V R',NBABS*NBNMES*3,LVALR)
          FICVA = .TRUE.
        END IF
      END IF

C RECUPERATION RANGEMENT DES VALEURS : EVEN / UNEVEN : ITYPE
      IFIELD = 3
      CALL DECOD2(REC,IREC,IFIELD,0,ITYPE,RBID,TROUVE)
      IF (ITYPE .EQ. 1) THEN
C RECUPERATION ABSCISSE MIN ET PAS : AMIN APAS
        IFIELD = 4
        CALL DECOD2(REC,IREC,IFIELD,1,IBID,AMIN,TROUVE)
        IFIELD = 5
        CALL DECOD2(REC,IREC,IFIELD,1,IBID,APAS,TROUVE)
      END IF

C LECTURE DU TYPE DU CHAMP
      IREC = 9
      IFIELD = 1
      CALL DECOD2(REC,IREC,IFIELD,0,ICHAMP,RBID,TROUVE)

      IF (ICHAMP .NE. ICHAM0) GO TO 11

      IF (ICHAMP .EQ. 2) THEN
        IF (NBMESU .EQ. 0) THEN
          NCMP = 6
          LICMP(1) = 'SIXX'
          LICMP(2) = 'SIYY'
          LICMP(3) = 'SIZZ'
          LICMP(4) = 'SIXY'
          LICMP(5) = 'SIXZ'
          LICMP(6) = 'SIYZ'
          IF (ZCMPLX) THEN
            NOMGD = 'SIEF_C'
          ELSE
            NOMGD = 'SIEF_R'
          ENDIF
        ENDIF
      ENDIF
      IF (ICHAMP .EQ. 3) THEN
        IF (NBMESU .EQ. 0) THEN
          NCMP = 6
          LICMP(1) = 'EPXX'
          LICMP(2) = 'EPYY'
          LICMP(3) = 'EPZZ'
          LICMP(4) = 'EPXY'
          LICMP(5) = 'EPXZ'
          LICMP(6) = 'EPYZ'
          IF (ZCMPLX) THEN
            CALL U2MESS('F','ALGORITH5_3')
          ELSE
            NOMGD = 'EPSI_R'
          ENDIF
        ENDIF
      ENDIF
      IF (ICHAMP .EQ. 8 .OR. ICHAMP .EQ. 11 .OR. ICHAMP .EQ. 12) THEN
        IF (NBMESU .EQ. 0) THEN
          NCMP = 12
          LICMP(1) = 'D1'
          LICMP(2) = 'D2'
          LICMP(3) = 'D3'
          LICMP(4) = 'D1X'
          LICMP(5) = 'D1Y'
          LICMP(6) = 'D1Z'
          LICMP(7) = 'D2X'
          LICMP(8) = 'D2Y'
          LICMP(9) = 'D2Z'
          LICMP(10) = 'D3X'
          LICMP(11) = 'D3Y'
          LICMP(12) = 'D3Z'
          IF (ZCMPLX) THEN
            NOMGD = 'DEPL_C'
          ELSE
            NOMGD = 'DEPL_R'
          ENDIF
        ENDIF
      ENDIF

      NBMESU = NBMESU + 1

      IF (NBMESU .GT. NBNMES*6) THEN
        CALL U2MESS('F','ALGORITH5_4')
      END IF

C LECTURE DU NUMERO DU NOEUD
      IREC = 6
      IFIELD = 6
      CALL DECOD2(REC,IREC,IFIELD,0,LABEL,RBID,TROUVE)
      IF (LABEL .EQ. 0) THEN
        LIGNE = REC(IREC)
        LABK8 = LIGNE(32:41)
        CALL JENONU (JEXNOM (MAILLA//'.NOMNOE', LABK8), LABEL)
      ELSE
C PRE_IDEAS RAJOUTE UN 'N' DEVANT LE NUMERO DU NOEUD (VOIR ECRNEU)
        CALL CODNOP(LABK8,PRFNOE,1,1)
        CALL CODENT(LABEL,'G',LABK8(2:8))
        CALL JENONU (JEXNOM (MAILLA//'.NOMNOE', LABK8), LABEL)
      END IF
      ZI(LCORR-1 + NBMESU) = LABEL

C LECTURE DU CODE DE LA DIRECTION DE MESURE
      IREC = 6
      IFIELD = 7
      CALL DECOD2(REC,IREC,IFIELD,0,IDIR,RBID,TROUVE)
      ZI(LORI-1 +NBMESU) = IDIR

C LECTURE DES VALEURS
      CALL LECTVL(ZCMPLX,ITYPE,NBABS,INATUR,IDEAS,NBMESU,
     &                   LABS,AMIN,APAS,LVALC,LVALR)

      READ (IDEAS, 1000, END = 170) LIGNE
      IF ( LIGNE .NE. REPEM1) THEN
        VALI = NBMESU
        CALL U2MESG('F','ALGORITH15_98',0,' ',1,VALI,0,0.D0)
      ENDIF

      GO TO 10

  160 CONTINUE
C EN CAS D ERREUR DE LECTURE DU FICHIER UNV
      CALL U2MESS('F','ALGORITH5_5')

   11 CONTINUE
C POSITIONNEMENT A LA FIN DU DATASET
      READ ( IDEAS , 1000 , END = 170 ) LIGNE
      IF ( LIGNE .NE. REPEM1 ) GO TO 11
      GO TO 10

  170 CONTINUE
C FIN LECTURE FICHIER UNV
C
      IFRES = IUNIFI ('MESSAGE')
      WRITE(IFRES,1001) NOMSYM,NBMESU
 1001 FORMAT('NOM_CHAM : ',A16,'NOMBRE DE MESURES : ',I6)
      IF (NBMESU .EQ. 0) THEN
        WRITE(IFRES,1002) NOMSYM
 1002   FORMAT('AUCUN CHAMP ',A16,' TROUVE')
        CALL U2MESS('A','ALGORITH5_6')
        GO TO 9999
      ENDIF

C CREATION DE SD_RESULTAT DYNA_TRANS / DYNA_HARMO / HARM_GENE : TYPRES
      IF((ZCMPLX) .AND. (TYPRES(1:6) .EQ. 'DYNA_T'))
     &  CALL U2MESS('F','ALGORITH5_1')

      IF((.NOT.ZCMPLX) .AND. (TYPRES(1:6) .NE. 'DYNA_T'))
     &  CALL U2MESS('F','ALGORITH5_2')

      IF (INOCH .EQ. 1) CALL RSAGSD(NOMRES,NBABS)
      NOOJB='12345678.00000.NUME.PRNO'
      CALL GNOMSD ( NOOJB,10,14)
      PRFCHN=NOOJB(1:19)

      VUDEF = .FALSE.
      VUCONT = .FALSE.
      DO 200 NUMORD = 1,NBABS
        RVAL = ZR(LABS-1 +NUMORD)
        CALL NUMEOK(ACCES,NUMORD,RVAL,LISTR8,LISTIS,PRECIS,
     &                CRIT,EPSI,ASTOCK)
        IF(ASTOCK) THEN
          CALL CNSCRE(MAILLA,NOMGD,NCMP,LICMP,'V',CNS)
          CALL JEVEUO(CNS//'.CNSV','E',JCNSV)
          CALL JEVEUO(CNS//'.CNSL','E',JCNSL)
          DO 303 IMES = 1,NBMESU
            ICMP = ZI(LORI-1 + IMES)
            IVAL = NBABS*(IMES-1) + NUMORD
            INO = ZI(LCORR-1 + IMES)
            IF(ZCMPLX) THEN
              CVAL = ZC(LVALC-1 +IVAL)
            ELSE
              RVAL = ZR(LVALR-1 +IVAL)
            ENDIF
            IF (ICMP .LT. 0) THEN
              ICMP = -ICMP
              IF(ZCMPLX) THEN
                CVAL = -CVAL
              ELSE
                RVAL = -RVAL
              ENDIF
            ENDIF
            IF (NOMGD(1:4) .EQ. 'DEPL') THEN
C ON SUPPOSE QUE ICMP EST COMPRIS ENTRE -3 ET 3
              IDIR = (INO-1)*NCMP + (ICMP-1)*3 + 3
              IF(ZCMPLX) THEN
                ZC(JCNSV-1 + (INO-1)*NCMP+ICMP) = CVAL
                ZC(JCNSV-1 + IDIR+1) = CZERO
                ZC(JCNSV-1 + IDIR+2) = CZERO
                ZC(JCNSV-1 + IDIR+3) = CZERO
                ZC(JCNSV-1 + IDIR+ICMP) = CUN
              ELSE
                ZR(JCNSV-1 + (INO-1)*NCMP+ICMP) = RVAL
                ZR(JCNSV-1 + IDIR+1) = 0.D0
                ZR(JCNSV-1 + IDIR+2) = 0.D0
                ZR(JCNSV-1 + IDIR+3) = 0.D0
                ZR(JCNSV-1 + IDIR+ICMP) = 1.D0
              ENDIF
              ZL(JCNSL-1 + (INO-1)*NCMP+ICMP) = .TRUE.
              ZL(JCNSL-1 + IDIR+1) = .TRUE.
              ZL(JCNSL-1 + IDIR+2) = .TRUE.
              ZL(JCNSL-1 + IDIR+3) = .TRUE.

C TRAITEMENT DES ORIENTATIONS POUR DEPL
      CALL GETFAC('REDEFI_ORIENT',NBOCC)
      IF (NBOCC.GT.0) THEN
        DO 304 IOCC = 1,NBOCC
          MOTCLE(1) = 'NOEUD'
          TYMOCL(1) = 'NOEUD'
          CALL RELIEM(' ',MAILLA,'NU_NOEUD','REDEFI_ORIENT',IOCC,1,
     &                 MOTCLE, TYMOCL,'&&DEFDIR',NBNO2)
          CALL JEVEUO('&&DEFDIR','L',IAGNO2)
          DO 305 I = 1,NBNO2
            IF(ZI(IAGNO2-1 +I) .EQ. INO) THEN
          CALL GETVIS('REDEFI_ORIENT','CODE_DIR',IOCC,IARG,1,ICMPM,IBID)
              IF(ICMP .EQ. ICMPM) THEN
          CALL GETVR8('REDEFI_ORIENT','DIRECTION',IOCC,IARG,3,DIR,IBID)
                IF(ZCMPLX) THEN
                  ZC(JCNSV-1 + IDIR+1) = DCMPLX(DIR(1),0.D0)
                  ZC(JCNSV-1 + IDIR+2) = DCMPLX(DIR(2),0.D0)
                  ZC(JCNSV-1 + IDIR+3) = DCMPLX(DIR(3),0.D0)
                ELSE
                  ZR(JCNSV-1 + IDIR+1) = DIR(1)
                  ZR(JCNSV-1 + IDIR+2) = DIR(2)
                  ZR(JCNSV-1 + IDIR+3) = DIR(3)
                ENDIF
              ENDIF
            ENDIF
 305      CONTINUE
          CALL JEDETR('&&DEFDIR')
 304    CONTINUE
      ENDIF
C FIN TRAITEMENT DES ORIENTATIONS POUR DEPL
            ENDIF

            IF (NOMGD(1:4) .EQ. 'SIEF') THEN
              CALL GETFAC('REDEFI_ORIENT',NBOCC)
              IF ((NBOCC.GT.0) .AND. (.NOT. VUCONT)) THEN
                CALL U2MESS('A','ALGORITH5_9')
                VUCONT = .TRUE.
              ENDIF
              IF(ZCMPLX) THEN
                ZC(JCNSV-1 + (INO-1)*NCMP+ICMP) = CVAL
              ELSE
                ZR(JCNSV-1 + (INO-1)*NCMP+ICMP) = RVAL
              ENDIF
              ZL(JCNSL-1 + (INO-1)*NCMP+ICMP) = .TRUE.
            ENDIF

            IF (NOMGD(1:4) .EQ. 'EPSI') THEN
              CALL GETFAC('REDEFI_ORIENT',NBOCC)
              IF ((NBOCC.GT.0) .AND. (.NOT. VUDEF)) THEN
                CALL U2MESS('A','ALGORITH5_10')
                VUDEF = .TRUE.
              ENDIF
              ZR(JCNSV-1 + (INO-1)*NCMP+ICMP) = RVAL
              ZL(JCNSL-1 + (INO-1)*NCMP+ICMP) = .TRUE.
            ENDIF
 303      CONTINUE

C RECUPERATION DU NOM DU CHAMP POUR NUMORD : NOMCH
          CALL RSEXCH(NOMRES,NOMSYM,NUMORD,NOMCH,IRET)
          CALL CNSCNO(CNS,PRFCHN,'NON','G',NOMCH,'F',IBID)
C
          CALL RSNOCH(NOMRES,NOMSYM,NUMORD)
          IF(ZCMPLX) THEN
            CALL RSADPA(NOMRES,'E',1,'FREQ',NUMORD,0,JABS,K8BID)
          ELSE
            CALL RSADPA(NOMRES,'E',1,'INST',NUMORD,0,JABS,K8BID)
          ENDIF
          ZR(JABS) = ZR(LABS-1 + NUMORD)
C
          CALL DETRSD('CHAM_NO_S',CNS)
        ENDIF
C FIN BOUCLE SUR NUMERO ORDRE
 200  CONTINUE

 9999 CONTINUE

C FIN BOUCLE SUR LES CHAMPS DEMANDES
 500  CONTINUE

      CALL JEDETR(VABS)
      CALL JEDETR(VORI)
      CALL JEDETR(VCOR)
      CALL JEDETR(VALMES)

      CALL JEDEMA()
C
 1000 FORMAT ( A80 )
C
      END
