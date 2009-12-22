      SUBROUTINE SSCGMA(MA,NBGMP,NBGMIN)
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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
C TOLE CRP_20
C ----------------------------------------------------------------------
C     BUT: TRAITER LE MOT CLEF CREA_GROUP_MA
C          DE L'OPERATEUR: DEFI_GROUP

C     IN:
C          MA    : NOM DU MAILLAGE
C          NBGMP : NOMBRE DE GROUP_MA A CREER
C     ------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      CHARACTER*8 MA,NOMA,NOGMA,KBID,NOGMA2,KPOS,NOM1,TOUT
      CHARACTER*8 ALARM,TYMA
      CHARACTER*16 CONCEP,CMD,OPTION
      CHARACTER*24 LISMA
      CHARACTER*24 VALK(2)
      CHARACTER*132 CARD
C     ------------------------------------------------------------------

      CALL JEMARQ()

C     RECUPERATION DU NIVEAU D'IMPRESSION
C     -----------------------------------
      CALL INFNIV(IFM,NIV)

      CALL GETRES(KBID,CONCEP,CMD)
      LISMA = '&&SSCGMA.LISTE_MAILLES'
      CALL JELIRA(MA//'.GROUPEMA','NMAXOC',NBGRMN,KBID)
      NBIS = NBGRMN
      NBK8 = NBGRMN
      CALL WKVECT('&&SSCGMA.LIK8','V V K8',NBK8,IALIK8)
      CALL WKVECT('&&SSCGMA.LII1','V V I',NBIS,IALII1)
      CALL WKVECT('&&SSCGMA.LII2','V V I',NBIS,IALII2)

      CALL GETVTX(' ','ALARME',1,1,1,ALARM,NALAR)

      NBGNAJ = 0
      DO 210,IOCC = 1,NBGMP

        CALL GETVTX('CREA_GROUP_MA','NOM',IOCC,1,1,NOGMA,N1)

        CALL JENONU(JEXNOM(MA//'.GROUPEMA',NOGMA),IBID)
        IF (IBID.GT.0) CALL U2MESK('A','ALGELINE3_7',1,NOGMA)

        CALL GETVEM(MA,'MAILLE','CREA_GROUP_MA','MAILLE',IOCC,1,0,KBID,
     &              N2)
        CALL GETVTX('CREA_GROUP_MA','INTERSEC',IOCC,1,0,KBID,N3)
        CALL GETVTX('CREA_GROUP_MA','UNION',IOCC,1,0,KBID,N4)
        CALL GETVTX('CREA_GROUP_MA','DIFFE   ',IOCC,1,0,KBID,N5)
        CALL GETVEM(MA,'GROUP_MA','CREA_GROUP_MA','GROUP_MA',IOCC,1,0,
     &              KBID,N6)
        CALL GETVTX('CREA_GROUP_MA','OPTION',IOCC,1,0,OPTION,N7)
        CALL GETVTX('CREA_GROUP_MA','TOUT',IOCC,1,0,TOUT,N8)
        N2 = -N2
        N3 = -N3
        N4 = -N4
        N5 = -N5
        N6 = -N6
        N7 = -N7
        N8 = -N8

C       -- MOT CLEF TOUT:
C       -------------------
        IF (N8.GT.0) THEN
          CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMAT,KBID,IERD)
          CALL JECROC(JEXNOM(MA//'.GROUPEMA',NOGMA))
          CALL JEECRA(JEXNOM(MA//'.GROUPEMA',NOGMA),'LONMAX',NBMAT,KBID)
          CALL JEVEUO(JEXNOM(MA//'.GROUPEMA',NOGMA),'E',IAGMA)
          DO 10,IMA = 1,NBMAT
            ZI(IAGMA+IMA-1) = IMA
   10     CONTINUE
          NBGNAJ = NBGNAJ + 1
          GO TO 210
        END IF

C       -- MOT CLEF MAILLE:
C       -------------------
        IF (N2.GT.0) THEN
          CALL WKVECT('&&SSCGMA.L_MAILLE','V V K8',N2,ILMAK8)
          CALL GETVEM(MA,'MAILLE','CREA_GROUP_MA','MAILLE',IOCC,1,N2,
     &                ZK8(ILMAK8),N1)
          CALL WKVECT('&&SSCGMA.MAILLE','V V I',N2,JMAIL)
          CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMAT,KBID,IERD)
          CALL WKVECT('&&SSCGMA.MAILLE2','V V I',NBMAT,JMAIL2)
          NBMA = 0
          IER = 0
          DO 20 IM1 = 1,N2
            NOM1 = ZK8(ILMAK8+IM1-1)
            CALL JENONU(JEXNOM(MA//'.NOMMAI',NOM1),NUM)
            IF (NUM.EQ.0) THEN
              IER = IER + 1
              CALL U2MESK('E','SOUSTRUC_31',1,NOM1)
              GO TO 20
            END IF
            ZI(JMAIL2-1+NUM) = ZI(JMAIL2-1+NUM) + 1
            IF (ZI(JMAIL2-1+NUM).EQ.2) THEN
               VALK(1) = NOM1
               VALK(2) = NOGMA
               CALL U2MESK('A','SOUSTRUC_32', 2 ,VALK)
              GO TO 20
            END IF
            NBMA = NBMA + 1
            ZI(JMAIL+NBMA-1) = NUM
   20     CONTINUE
          IF (IER.NE.0) CALL ASSERT(.FALSE.)
          CALL JECROC(JEXNOM(MA//'.GROUPEMA',NOGMA))
          CALL JEECRA(JEXNOM(MA//'.GROUPEMA',NOGMA),'LONMAX',NBMA,KBID)
          CALL JEVEUO(JEXNOM(MA//'.GROUPEMA',NOGMA),'E',IAGMA)
          DO 30,IMA = 0,NBMA - 1
            ZI(IAGMA+IMA) = ZI(JMAIL+IMA)
   30     CONTINUE
          NBGNAJ = NBGNAJ + 1
          CALL JEDETR('&&SSCGMA.MAILLE')
          CALL JEDETR('&&SSCGMA.MAILLE2')
          CALL JEDETR('&&SSCGMA.L_MAILLE')
          GO TO 210
        END IF

C       -- MOT CLEF GROUP_MA:
C       ---------------------
        IF (N6.GT.0) THEN
          CALL GETVEM(MA,'GROUP_MA','CREA_GROUP_MA','GROUP_MA',IOCC,1,1,
     &                NOGMA2,NBID)
          CALL GETVTX('CREA_GROUP_MA','POSITION',IOCC,1,0,KPOS,N6B)
          CALL JENONU(JEXNOM(MA//'.GROUPEMA',NOGMA2),IGM2)
          CALL JELIRA(JEXNUM(MA//'.GROUPEMA',IGM2),'LONMAX',ILI2,KBID)
          CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',IGM2),'L',IAGM2)
          IND1 = 0
          IND2 = 0
          IF (N6B.EQ.0) THEN
            CALL GETVIS('CREA_GROUP_MA','NUME_INIT',IOCC,1,1,IND1,N6A)
            IF (N6A.EQ.0) IND1 = 1
            CALL GETVIS('CREA_GROUP_MA','NUME_FIN',IOCC,1,1,IND2,N6A)
            IF (N6A.EQ.0) IND2 = ILI2
            IF (IND2.LT.IND1) CALL U2MESS('F','SOUSTRUC_33')
            IF (ILI2.LT.IND2) CALL U2MESS('F','SOUSTRUC_34')
            N6A = IND2 - IND1 + 1
          ELSE
            N6A = 1
          END IF
          CALL JECROC(JEXNOM(MA//'.GROUPEMA',NOGMA))
          CALL JEECRA(JEXNOM(MA//'.GROUPEMA',NOGMA),'LONMAX',N6A,KBID)
          CALL JEVEUO(JEXNOM(MA//'.GROUPEMA',NOGMA),'E',IAGMA)
          NBGNAJ = NBGNAJ + 1
          IF (N6B.EQ.0) THEN
            N = IND2 - IND1 + 1
            DO 40 II = 1,N
              ZI(IAGMA-1+II) = ZI(IAGM2-2+IND1+II)
   40       CONTINUE
            GO TO 210
          END IF
          CALL GETVTX('CREA_GROUP_MA','POSITION',IOCC,1,1,KPOS,N6B)
          IF (KPOS.EQ.'INIT') THEN
            ZI(IAGMA) = ZI(IAGM2)
          ELSE IF (KPOS.EQ.'FIN') THEN
            II = ILI2
            ZI(IAGMA) = ZI(IAGM2+II-1)
          ELSE IF (KPOS.EQ.'MILIEU') THEN
            II = (ILI2+1)/2
            ZI(IAGMA) = ZI(IAGM2+II-1)
          END IF
          GO TO 210
        END IF


C       -- MOT CLEF INTER:
C       -------------------
        IF (N3.GT.0) THEN
          CALL GETVTX('CREA_GROUP_MA','INTERSEC',IOCC,1,N3,ZK8(IALIK8),
     &                NBID)
          DO 50,IGM = 1,N3
            CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IALIK8-1+IGM)),IGM2)
            IF (IGM2.EQ.0) CALL U2MESK('F','SOUSTRUC_35',1,ZK8(IALIK8-1+
     &                                   IGM))
   50     CONTINUE

          CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IALIK8)),IGM1)
          CALL JELIRA(JEXNUM(MA//'.GROUPEMA',IGM1),'LONMAX',ILI1,KBID)
          CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',IGM1),'L',IAGM1)
          IF (ILI1.GT.NBIS) THEN
            NBIS = 2*ILI1
            CALL JEDETR('&&SSCGMA.LII1')
            CALL JEDETR('&&SSCGMA.LII2')
            CALL WKVECT('&&SSCGMA.LII1','V V I',NBIS,IALII1)
            CALL WKVECT('&&SSCGMA.LII2','V V I',NBIS,IALII2)
          END IF
          N = ILI1
          DO 60 II = 1,N
            ZI(IALII1-1+II) = ZI(IAGM1-1+II)
   60     CONTINUE

          DO 80,IGM = 2,N3
            CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IALIK8-1+IGM)),IGM2)
            CALL JELIRA(JEXNUM(MA//'.GROUPEMA',IGM2),'LONMAX',ILI2,KBID)
            CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',IGM2),'L',IAGM2)
            CALL UTLISI('INTER',ZI(IALII1),N,ZI(IAGM2),ILI2,ZI(IALII2),
     &                  NBIS,NTROU)
            N = NTROU
            DO 70 II = 1,N
              ZI(IALII1-1+II) = ZI(IALII2-1+II)
   70       CONTINUE
   80     CONTINUE

          IF (N.EQ.0) THEN
            IF (ALARM.EQ.'OUI') THEN
              CALL U2MESK('A','SOUSTRUC_36',1,NOGMA)
            END IF
          ELSE
            CALL JECROC(JEXNOM(MA//'.GROUPEMA',NOGMA))
            CALL JEECRA(JEXNOM(MA//'.GROUPEMA',NOGMA),'LONMAX',N,KBID)
            CALL JEVEUO(JEXNOM(MA//'.GROUPEMA',NOGMA),'E',IAGMA)
            DO 90 II = 1,N
              ZI(IAGMA-1+II) = ZI(IALII1-1+II)
   90       CONTINUE
            NBGNAJ = NBGNAJ + 1
          END IF
          GO TO 210
        END IF


C       -- MOT CLEF UNION:
C       -------------------
        IF (N4.GT.0) THEN
          CALL GETVTX('CREA_GROUP_MA','UNION',IOCC,1,N4,ZK8(IALIK8),
     &                NBID)
          DO 100,IGM = 1,N4
            CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IALIK8-1+IGM)),IGM2)
            IF (IGM2.EQ.0) CALL U2MESK('F','SOUSTRUC_35',1,ZK8(IALIK8-1+
     &                                   IGM))
  100     CONTINUE

          CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IALIK8)),IGM1)
          CALL JELIRA(JEXNUM(MA//'.GROUPEMA',IGM1),'LONMAX',ILI1,KBID)
          CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',IGM1),'L',IAGM1)
          IF (ILI1.GT.NBIS) THEN
            NBIS = 2*ILI1
            CALL JEDETR('&&SSCGMA.LII1')
            CALL JEDETR('&&SSCGMA.LII2')
            CALL WKVECT('&&SSCGMA.LII1','V V I',NBIS,IALII1)
            CALL WKVECT('&&SSCGMA.LII2','V V I',NBIS,IALII2)
          END IF
          N = ILI1
          DO 110 II = 1,N
            ZI(IALII1-1+II) = ZI(IAGM1-1+II)
  110     CONTINUE

          DO 130,IGM = 2,N4
            CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IALIK8-1+IGM)),IGM2)
            CALL JELIRA(JEXNUM(MA//'.GROUPEMA',IGM2),'LONMAX',ILI2,KBID)
            CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',IGM2),'L',IAGM2)
            CALL UTLISI('UNION',ZI(IALII1),N,ZI(IAGM2),ILI2,ZI(IALII2),
     &                  NBIS,NTROU)

            IF (NTROU.LT.0) THEN
              NBIS = -2*NTROU
              CALL JEDETR('&&SSCGMA.LII2')
              CALL WKVECT('&&SSCGMA.LII2','V V I',NBIS,IALII2)
              CALL UTLISI('UNION',ZI(IALII1),N,ZI(IAGM2),ILI2,
     &                    ZI(IALII2),NBIS,NTROU)
              CALL JEDETR('&&SSCGMA.LII1')
              CALL WKVECT('&&SSCGMA.LII1','V V I',NBIS,IALII1)
            END IF
            N = NTROU
            DO 120 II = 1,N
              ZI(IALII1-1+II) = ZI(IALII2-1+II)
  120       CONTINUE
  130     CONTINUE

          IF (N.EQ.0) THEN
            IF (ALARM.EQ.'OUI') THEN
              CALL U2MESK('A','SOUSTRUC_36',1,NOGMA)
            END IF
          ELSE
            CALL JECROC(JEXNOM(MA//'.GROUPEMA',NOGMA))
            CALL JEECRA(JEXNOM(MA//'.GROUPEMA',NOGMA),'LONMAX',N,KBID)
            CALL JEVEUO(JEXNOM(MA//'.GROUPEMA',NOGMA),'E',IAGMA)
            DO 140 II = 1,N
              ZI(IAGMA-1+II) = ZI(IALII1-1+II)
  140       CONTINUE
            NBGNAJ = NBGNAJ + 1
          END IF
          GO TO 210
        END IF


C       -- MOT CLEF DIFFE:
C       -------------------
        IF (N5.GT.0) THEN
          CALL GETVTX('CREA_GROUP_MA','DIFFE',IOCC,1,N5,ZK8(IALIK8),
     &                NBID)
          DO 150,IGM = 1,N5
            CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IALIK8-1+IGM)),IGM2)
            IF (IGM2.EQ.0) CALL U2MESK('F','SOUSTRUC_35',1,ZK8(IALIK8-1+
     &                                   IGM))
  150     CONTINUE

          CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IALIK8)),IGM1)
          CALL JELIRA(JEXNUM(MA//'.GROUPEMA',IGM1),'LONMAX',ILI1,KBID)
          CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',IGM1),'L',IAGM1)
          IF (ILI1.GT.NBIS) THEN
            NBIS = 2*ILI1
            CALL JEDETR('&&SSCGMA.LII1')
            CALL JEDETR('&&SSCGMA.LII2')
            CALL WKVECT('&&SSCGMA.LII1','V V I',NBIS,IALII1)
            CALL WKVECT('&&SSCGMA.LII2','V V I',NBIS,IALII2)
          END IF
          N = ILI1
          DO 160 II = 1,N
            ZI(IALII1-1+II) = ZI(IAGM1-1+II)
  160     CONTINUE

          DO 180,IGM = 2,N5
            CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IALIK8-1+IGM)),IGM2)
            CALL JELIRA(JEXNUM(MA//'.GROUPEMA',IGM2),'LONMAX',ILI2,KBID)
            CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',IGM2),'L',IAGM2)
            CALL UTLISI('DIFFE',ZI(IALII1),N,ZI(IAGM2),ILI2,ZI(IALII2),
     &                  NBIS,NTROU)
            N = NTROU
            DO 170 II = 1,N
              ZI(IALII1-1+II) = ZI(IALII2-1+II)
  170       CONTINUE
  180     CONTINUE

          IF (N.EQ.0) THEN
            IF (ALARM.EQ.'OUI') THEN
              CALL U2MESK('A','SOUSTRUC_36',1,NOGMA)
            END IF
          ELSE
            CALL JECROC(JEXNOM(MA//'.GROUPEMA',NOGMA))
            CALL JEECRA(JEXNOM(MA//'.GROUPEMA',NOGMA),'LONMAX',N,KBID)
            CALL JEVEUO(JEXNOM(MA//'.GROUPEMA',NOGMA),'E',IAGMA)
            DO 190 II = 1,N
              ZI(IAGMA-1+II) = ZI(IALII1-1+II)
  190       CONTINUE
            NBGNAJ = NBGNAJ + 1
          END IF
          GO TO 210
        END IF


C       -- MOT CLEF OPTION:
C       -------------------
        IF (N7.GT.0) THEN

          CALL GETVTX('CREA_GROUP_MA','OPTION',IOCC,1,1,OPTION,NB)

C            -- TRAITEMENT DE L'OPTION FACE_NORMALE :
C               -----------------------------------
          IF (OPTION(1:12).EQ.'FACE_NORMALE') THEN
            CALL CGMAFN('CREA_GROUP_MA',IOCC,MA,LISMA,NBMA)

C            -- TRAITEMENT DE L'OPTION SPHERE :
C               -----------------------------
          ELSE IF (OPTION(1:6).EQ.'SPHERE') THEN
            CALL CGMASP('CREA_GROUP_MA',IOCC,MA,LISMA,NBMA)

C            -- TRAITEMENT DE L'OPTION CYLINDRE :
C               -------------------------------
          ELSE IF (OPTION(1:8).EQ.'CYLINDRE') THEN
            CALL CGMACY('CREA_GROUP_MA',IOCC,MA,LISMA,NBMA)

C            -- TRAITEMENT DE L'OPTION BANDE :
C               ----------------------------
          ELSE IF (OPTION(1:5).EQ.'BANDE') THEN
            CALL CGMABA('CREA_GROUP_MA',IOCC,MA,LISMA,NBMA)

C            -- TRAITEMENT DE L'OPTION APPUI_LACHE :
C               ----------------------------------
          ELSE IF (OPTION(1:11).EQ.'APPUI_LACHE') THEN
            CALL U2MESS('A','SOUSTRUC2_6')
            CALL CGMAAL('CREA_GROUP_MA',IOCC,MA,LISMA,NBMA)

C            -- TRAITEMENT DE L'OPTION APPUI_STRICT :
C               ----------------------------------
          ELSE IF (OPTION(1:5).EQ.'APPUI') THEN
            CALL CGMAAP('CREA_GROUP_MA',IOCC,MA,LISMA,NBMA)
          END IF
C
C        -- ON FILTRE LES TYPES DE MAILLES :
C           --------------------------------
          IF (NBMA.GT.0) THEN
            CALL GETVTX('CREA_GROUP_MA','TYPE_MAILLE',IOCC,1,1,
     &                  TYMA,NTYP)
            IF(TYMA(1:4).NE.'TOUT')THEN
               CALL CGMFTM(TYMA,MA,LISMA,NBMA,IERR)
               IF(IERR.NE.0)THEN
                  CALL U2MESK('F','SOUSTRUC2_7',1,NOGMA)
               ENDIF
            ENDIF
          ENDIF  
C
C        -- CREATION ET AFFECTATION DU GROUP_MA :
C            ----------------------------------
          IF (NBMA.EQ.0) THEN
            IF (ALARM.EQ.'OUI') THEN
              CALL U2MESK('A','SOUSTRUC_36',1,NOGMA)
            END IF
          ELSE
            CALL JEVEUO(LISMA,'L',IDLIMA)

            CALL JECROC(JEXNOM(MA//'.GROUPEMA',NOGMA))
            CALL JEECRA(JEXNOM(MA//'.GROUPEMA',NOGMA),'LONMAX',NBMA,
     &                  KBID)
            CALL JEVEUO(JEXNOM(MA//'.GROUPEMA',NOGMA),'E',IAGMA)

            DO 200 II = 1,NBMA
              ZI(IAGMA-1+II) = ZI(IDLIMA-1+II)
  200       CONTINUE

            NBGNAJ = NBGNAJ + 1

          END IF

          CALL JEDETR(LISMA)

        END IF

  210 CONTINUE

C     IMPRESSIONS NIVEAUX 1 ET 2
C     --------------------------
      IF (NIV.GE.1 .AND. NBGNAJ.NE.0) THEN
        WRITE (IFM,'(/,/,A,I6,/,45(''=''))')
     &    'NOMBRE  DE GROUPES DE MAILLES CREES : ',NBGNAJ

        WRITE (IFM,'(/,15X,38(''-''),2(/,15X,A),/,15X,38(''-''))')
     &    '! NOM DU GROUPE ! NBRE DE MAILLES DU !',
     &    '!    MAILLES    !     GROUPE_MA      !'

        DO 220 I = 1,NBGNAJ
          II = NBGMIN + I
          CALL JENUNO(JEXNUM(MA//'.GROUPEMA',II),NOGMA)
          CALL JELIRA(JEXNUM(MA//'.GROUPEMA',II),'LONMAX',NBMA,KBID)
          WRITE (IFM,'(15X,A,2X,A8,5X,A,2X,I8,10X,A)') '!',NOGMA,'!',
     &      NBMA,'!'
  220   CONTINUE
        WRITE (IFM,'(15X,38(''-''),/)')
      END IF

C     IMPRESSIONS NIVEAU 2
C     --------------------
      IF (NIV.EQ.2 .AND. NBGNAJ.NE.0) THEN
        MAXCOL = 8
        DO 250 I = 1,NBGNAJ
          II = NBGMIN + I
          CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',II),'L',IAGMA)
          CALL JENUNO(JEXNUM(MA//'.GROUPEMA',II),NOGMA)
          CALL JELIRA(JEXNUM(MA//'.GROUPEMA',II),'LONMAX',NBMA,KBID)
          WRITE (IFM,'(/,3A,/,28(''-''))') 'MAILLES DU GROUPE ',NOGMA,
     &      ' :'
          NBLINE = NBMA/MAXCOL
          IRESTE = MOD(NBMA,MAXCOL)
          IF (IRESTE.NE.0) NBLINE = NBLINE + 1
          NBCOL = MAXCOL
          KKK = 0
          DO 240 JJJ = 1,NBLINE
            IF (IRESTE.NE.0 .AND. JJJ.EQ.NBLINE) NBCOL = IRESTE
            DO 230 III = 1,NBCOL
              KKK = KKK + 1
              CALL JENUNO(JEXNUM(MA//'.NOMMAI',ZI(IAGMA-1+KKK)),NOMA)
              CARD((III-1)*10+1:) = ' '//NOMA//' '
  230       CONTINUE
            WRITE (IFM,'(A)') CARD(:10*NBCOL)
  240     CONTINUE
  250   CONTINUE
        WRITE (IFM,'(/,/)')
      END IF

      CALL JEDETC('V','&&SSCGMA',1)
      CALL JEDEMA()

      END
