      SUBROUTINE RESUCO(NUMORD,INSTAP,DEFICO,RESOCO,DEPTOT,DEPDEL,
     &                  DDEPLA,NOMA,CNSINR,ITERAT)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/02/2003   AUTEUR PABHHHH N.TARDIEU 
C TOLE CRP_20
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
      IMPLICIT     NONE
      INTEGER NUMORD,ITERAT
      REAL*8 INSTAP
      CHARACTER*8 NOMA
      CHARACTER*19 CNSINR
      CHARACTER*24 DEFICO,RESOCO,DEPTOT,DEPDEL,DDEPLA
C ======================================================================
C COMMANDE STAT_NON_LINE / CONTACT UNILATERAL
C ECRITURE DANS LE FICHIER MESSAGE DES COUPLES NOEUD / MAILLE (OU NOEUD)
C EFFECTIVEMENT EN CONTACT A CONVERGENCE DE NEWTON (FIN DU PAS DE TEMPS)
C ======================================================================
C     IN   NUMORD : NUMERO DU PAS DE CHARGE
C     IN   INSTAP : INSTANT DE CALCUL
C     IN   DEFICO : SD DE DEFINITION DU CONTACT
C     IN   RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C     IN   NOMA   : NOM DU MAILLAGE
C     OUT  CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
C ======================================================================
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
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
C --------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------
C ======================================================================
      INTEGER IFM,NIV,ICONTA,POS1,POS2,NUM1,NUM2,NDIM,II,JJ,KK
      INTEGER JAPPAR,JCOCO,JLIAC,NBLIAC,JNOCO,JMACO,LLIAC,NBLIAI,NESCL
      INTEGER JAPJEU,JDEPDE,JDDEPL,IPENA
      INTEGER IBID,JCNSVR,JCNSLR,JDIM
      INTEGER JATMU,JAFMU,JMU,JDECAL,JAPPTR,JAPCOE,JAPDDL,NBDDL,NEQ,LMAT
      INTEGER JAPJFY,JAPJFX,JAPCOF,JMETH,JJIAC,LLF,LLF1,LLF2,NESMAX
      REAL*8 AJEUFX,AJEUFY,AJEUFT,RN,RNN,RT1,RT2,RT,COE,TESTMU,TESTCF
      REAL*8 VAL1,VAL2,VARC,R8BID,RT11,RT12,RT21,RT22,R8PREM,R8MIEM
      LOGICAL LBID
      CHARACTER*7 CHAIN
      CHARACTER*8 NOM1,NOM2,LICMPR(11)
      CHARACTER*16 K16BID
      CHARACTER*19 COCO,LIAC,ATMU,AFMU,MU,MATASS
      CHARACTER*19 K19BID,MATRIX(2)
      CHARACTER*24 APPARI,NDIMCO,METHCO,CONTNO,CONTMA,APPOIN,APCOEF
      CHARACTER*24 APDDL,APJEU,APCOFR,APJEFX,APJEFY,K24BID,PENAL
C ======================================================================
      CNSINR = '&&RESUCO.CNSINR'
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C --- RECUPERATION STRUCTURE DE DONNEES DE CONTACT

C --- STRUCTURE DE DONNEES DE CONTACT
C --- TRAITEMENT DU CONTACT : NOUVELLE VERSION (NMCONT)

      APPARI = RESOCO(1:14)//'.APPARI'
      CALL JEEXIN(APPARI,ICONTA)
      IF (ICONTA.NE.0) THEN

C ------ RECUPERATION DU DESCRIPTEUR DE LA MATRICE MECANIQUE
C        (ADHERENCE A LA METHODE DE NEWTON)
C --- LA VARIABLE ITERAT DE NMMATR EST MISE A 1 POUR
C --- FORCER LA RECHERCHE DU NOM DE LA MATRASS

        CALL NMMATR('CONTACT_FR',K24BID,K24BID,K24BID,K24BID,K24BID,
     &              K24BID,K19BID,K24BID,RESOCO,K16BID,K19BID,0.D0,
     &              K24BID,K19BID,0,ITERAT,K24BID,K24BID,K24BID,K24BID,
     &              MATRIX,K16BID,DEFICO,K24BID,LBID,K16BID,K24BID,
     &              K24BID,LBID,K24BID,K24BID,K24BID,R8BID,R8BID,IBID)

        MATASS = MATRIX(1)
        CALL JEVEUO(MATASS//'.&INT','L',LMAT)
        NEQ = ZI(LMAT+2)
        CALL JEVEUO(APPARI,'L',JAPPAR)
        NESCL = ZI(JAPPAR)
        NBLIAI = NESCL
        COCO = RESOCO(1:14)//'.COCO'
        CALL JEVEUO(COCO,'L',JCOCO)
        LIAC = RESOCO(1:14)//'.LIAC'
        CALL JEVEUO(LIAC,'L',JLIAC)
        NDIM = ZI(JCOCO)
        NBLIAC = ZI(JCOCO+1)
        LLF = ZI(JCOCO+6)
        LLF1 = ZI(JCOCO+7)
        LLF2 = ZI(JCOCO+8)

        CONTNO = DEFICO(1:16)//'.NOEUCO'
        CONTMA = DEFICO(1:16)//'.MAILCO'
        METHCO = DEFICO(1:16)//'.METHCO'
        NDIMCO = DEFICO(1:16)//'.NDIMCO'
        PENAL = DEFICO(1:16)//'.PENAL'
        APPOIN = RESOCO(1:14)//'.APPOIN'
        APCOEF = RESOCO(1:14)//'.APCOEF'
        APCOFR = RESOCO(1:14)//'.APCOFR'
        APDDL = RESOCO(1:14)//'.APDDL'
        APJEU = RESOCO(1:14)//'.APJEU'
        APJEFX = RESOCO(1:14)//'.APJEFX'
        APJEFY = RESOCO(1:14)//'.APJEFY'
        ATMU = RESOCO(1:14)//'.ATMU'
        AFMU = RESOCO(1:14)//'.AFMU'
        MU = RESOCO(1:14)//'.MU'
        CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
        CALL JEVEUO(DDEPLA(1:19)//'.VALE','L',JDDEPL)
        CALL JEVEUO(CONTNO,'L',JNOCO)
        CALL JEVEUO(CONTMA,'L',JMACO)
        CALL JEVEUO(METHCO,'L',JMETH)
        CALL JEVEUO(APPOIN,'L',JAPPTR)
        CALL JEVEUO(APCOEF,'L',JAPCOE)
        CALL JEVEUO(APDDL,'L',JAPDDL)
        CALL JEVEUO(APJEU,'L',JAPJEU)
        CALL JEVEUO(APJEFX,'L',JAPJFX)
        CALL JEVEUO(APJEFY,'L',JAPJFY)
        CALL JEVEUO(APCOFR,'L',JAPCOF)
        CALL JEVEUO(ATMU,'L',JATMU)
        CALL JEVEUO(AFMU,'L',JAFMU)
        CALL JEVEUO(MU,'L',JMU)
        CALL JEVEUO(NDIMCO,'L',JDIM)
        CALL JEVEUO(PENAL,'L',IPENA)

        NESMAX = ZI(JDIM+8)

C --- ECRITURE DES RELATIONS DE CONTACT A LA FIN DU PAS DE TEMPS

        IF (NIV.EQ.2) THEN
          WRITE (IFM,*)
          WRITE (IFM,'(''<CONTACT_2> '',10X,169(''*''))')
          WRITE (IFM,'(''<CONTACT_2> '',10X,''*'',11X,A28,128X,''*'')')
     &      'LISTE DES COUPLES EN CONTACT'
          WRITE (IFM,'(''<CONTACT_2> '',10X,''*'',15X,A14,I6,132X,
     &    ''*'')')  'NUMERO D''ORDRE',NUMORD
          WRITE (IFM,
     &    '(''<CONTACT_2> '',10X,''*'',15X,A7,3X,1PE12.5,
     &      130X,''*'')') 'INSTANT',INSTAP
          WRITE (IFM,'(''<CONTACT_2> '',10X,169(''*''))')

        IF (NBLIAC.EQ.0) WRITE (IFM,
     &    '(''<CONTACT_2> '',10X,''*'',
     &                                   18X,
     &    A14,135X,''*'')') 'PAS DE CONTACT'
        END IF

C ---- RECUPERATION DES DONNEES POUR CHAQUE LIAISON
C ---- PREPARATION POUR L'ARCHIVAGE
C --- CREATION DU CHAM_NO_S POUR LE CONTACT
        LICMPR(1) = 'CONTACT'
        LICMPR(2) = 'JEU'
        LICMPR(3) = 'RNN'
        LICMPR(4) = 'DUGT1'
        LICMPR(5) = 'DUGT2'
        LICMPR(6) = 'DUGT'
        LICMPR(7) = 'RN'
        LICMPR(8) = 'RT1'
        LICMPR(9) = 'RT2'
        LICMPR(10) = 'RT'
        LICMPR(11) = 'MU'

C --- CREATION DU CHAM_NO_S POUR LE CONTACT
        CALL CNSCRE(NOMA,'INFC_R',11,LICMPR,'V',CNSINR)
        CALL JEVEUO(CNSINR//'.CNSV','E',JCNSVR)
        CALL JEVEUO(CNSINR//'.CNSL','E',JCNSLR)

        DO 60 II = 1,NBLIAI
          VARC = 0.D0
          RN = 0.D0
          RNN = 0.D0
          COE = 0.D0
          RT1 = 0.D0
          RT2 = 0.D0
          AJEUFX = 0.D0
          AJEUFY = 0.D0
          AJEUFT = 0.D0
          JDECAL = ZI(JAPPTR+II-1)
          NBDDL = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
          IF (ZI(JMETH+6).EQ.-1) THEN
C  ---    METHODE PENALISEE EN CONTACT UNILATERAL
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                  ZR(JAFMU),RN)
          ELSE
C  ---    RECUPERATION DE AT.MU
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                  ZR(JATMU),RN)
C  ---    RECUPERATION DE (ASG)T.MUSG
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),
     &                  ZR(JATMU),RT11)
C  ---    RECUPERATION DE (AG)T.MUG
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),
     &                  ZR(JAFMU),RT12)
            RT1 = RT11 + RT12
            IF (NDIM.EQ.3) THEN
C  ---      RECUPERATION DE (ASG)T.MUSG
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                    ZI(JAPDDL+JDECAL),ZR(JATMU),RT21)
C  ---      RECUPERATION DE (AG)T.MUG
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                    ZI(JAPDDL+JDECAL),ZR(JAFMU),RT22)
              RT2 = RT21 + RT22
            END IF
          END IF
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),
     &                ZR(JDEPDE),VAL1)
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),
     &                ZR(JDDEPL),VAL2)
          AJEUFX = VAL1 + VAL2
          IF (NDIM.EQ.3) THEN
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                  ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL1)
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                  ZI(JAPDDL+JDECAL),ZR(JDDEPL),VAL2)
            AJEUFY = VAL1 + VAL2
          END IF
C ======================================================================
C --- METHODE PENALISATION --------------------------------------------
C ======================================================================
          IF (ZI(JMETH+6).EQ.3 .OR. ZI(JMETH+6).EQ.5) THEN
             DO 10 KK = 1,NBLIAC
                LLIAC = ZI(JLIAC+KK-1)
                IF (LLIAC.EQ.II) THEN
                   TESTMU = ZR(JMU-1+3*NBLIAI+LLIAC)
                   TESTCF = SQRT(ZR(IPENA-1+2*LLIAC))
                   IF (TESTCF.GT.R8MIEM()) THEN
                      IF (ABS((TESTMU-TESTCF)/TESTCF).GT.R8PREM()) THEN
                         VARC = 1.0D0
                      ELSE
                         VARC = 2.0D0
                      END IF
                   ELSE
                      VARC = 2.0D0
                   END IF
                   RNN = ZR(JMU+KK-1)
                   GO TO 20
                END IF
   10        CONTINUE
   20        CONTINUE
C ======================================================================
C --- AUTRES METHODES -------------------------------------------------
C ======================================================================
          ELSE
            DO 40 KK = 1,NBLIAC
              LLIAC = ZI(JLIAC+KK-1)
              IF (LLIAC.EQ.II) THEN
                DO 30 JJ = 1,LLF + LLF1 + LLF2
                  JJIAC = ZI(JLIAC+NBLIAC+JJ-1)
                  IF (JJIAC.EQ.II) THEN
                    VARC = 1.0D0
                    RNN = ZR(JMU+KK-1)
                    IF ((JJ.GT.LLF) .AND. (JJ.LE. (LLF+LLF1))) THEN
                      RT2 = 0.D0
                    ELSE IF (JJ.GT. (LLF+LLF1) .AND.
     &                       (JJ.LE. (LLF+LLF1+LLF2))) THEN
                      RT1 = 0.D0
                    END IF
                    GO TO 50
                  END IF
   30           CONTINUE
                VARC = 2.0D0
                RNN = ZR(JMU+KK-1)
                GO TO 50
              END IF
   40       CONTINUE
   50       CONTINUE
          END IF

          RN = RN/2.D0
          RT1 = RT1/2.D0
          RT2 = RT2/2.D0
          AJEUFT = SQRT(AJEUFX**2+AJEUFY**2)
          RT = SQRT(RT1**2+RT2**2)
          IF (RN.NE.0.D0) COE = RT/RN


C ----- RECHERCHE CONNECTIVITE

          POS1 = ZI(JAPPAR+3* (II-1)+1)
          NUM1 = ZI(JNOCO+POS1-1)
          CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
          POS2 = ZI(JAPPAR+3* (II-1)+2)
          IF (POS2.GT.0) THEN
            CHAIN = 'MAILLE '
            NUM2 = ZI(JMACO+POS2-1)
            CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUM2),NOM2)
          ELSE IF (POS2.LT.0) THEN
            CHAIN = 'NOEUD '
            NUM2 = ZI(JNOCO+ABS(POS2)-1)
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM2),NOM2)
          ELSE IF (POS2.EQ.0) THEN
            CHAIN = ' '
            NOM2 = ' '
          END IF

C ----- FIN RECHERCHE CONNECTIVITE

          IF (NIV.EQ.2) THEN
            IF (VARC.GE.1.0D0) THEN
              WRITE (IFM,1001) '* ','LIAISON ACTIVE   ',II,' * NOEUD ',
     &          NOM1,' * ',CHAIN,NOM2,' *','JEU ',ZR(JAPJEU+II-1),' *',
     &          'RNN ',RNN,' *','GLI ',AJEUFT,' *','RN  ',RN,' *',
     &          'RT  ',RT,' *','MU  ',COE,' *'
            ELSE
              WRITE (IFM,1001) '* ','LIAISON INACTIVE ',II,' * NOEUD ',
     &          NOM1,' * ',CHAIN,NOM2,' *','JEU ',ZR(JAPJEU+II-1),' *',
     &          'RNN ',RNN,' *','GLI ',AJEUFT,' *','RN  ',RN,' *',
     &          'RT  ',RT,' *','MU  ',COE,' *'
            END IF
          END IF

C --- RECUPERATION DES VALEURS DU CONTACT

          ZR(JCNSVR-1+ (NUM1-1)*11+1) = VARC
          ZR(JCNSVR-1+ (NUM1-1)*11+2) = ZR(JAPJEU+II-1)
          ZR(JCNSVR-1+ (NUM1-1)*11+3) = RNN
          ZR(JCNSVR-1+ (NUM1-1)*11+4) = AJEUFX
          ZR(JCNSVR-1+ (NUM1-1)*11+5) = AJEUFY
          ZR(JCNSVR-1+ (NUM1-1)*11+6) = AJEUFT
          ZR(JCNSVR-1+ (NUM1-1)*11+7) = RN
          ZR(JCNSVR-1+ (NUM1-1)*11+8) = RT1
          ZR(JCNSVR-1+ (NUM1-1)*11+9) = RT2
          ZR(JCNSVR-1+ (NUM1-1)*11+10) = RT
          ZR(JCNSVR-1+ (NUM1-1)*11+11) = COE
          ZL(JCNSLR-1+ (NUM1-1)*11+1) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+2) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+3) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+4) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+5) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+6) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+7) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+8) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+9) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+10) = .TRUE.
          ZL(JCNSLR-1+ (NUM1-1)*11+11) = .TRUE.
   60   CONTINUE

        IF (NIV.EQ.2) THEN
          WRITE (IFM,'(''<CONTACT_2> '',10X,169(''*''))')
          WRITE (IFM,*)
        END IF
      END IF

 1001 FORMAT ('<CONTACT_2> ',10X,A2,A17,I5,A9,A8,A3,A7,A8,A2,A4,1PE12.5,
     &       A2,A4,1PE12.5,A2,A4,1PE12.5,A2,A4,1PE12.5,A2,A4,1PE12.5,A2,
     &       A4,1PE12.5,A2)

      CALL JEDEMA()
C-----------------------------------------------------------------------
      END
