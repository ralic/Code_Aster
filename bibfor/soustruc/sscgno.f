      SUBROUTINE SSCGNO(MA,NBGNIN)
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 14/10/2002   AUTEUR VABHHTS J.PELLET 
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
C TOLE  CRP_20
C ----------------------------------------------------------------------
C     BUT: TRAITER LE MOT CLEF CREA_GROUP_NO
C          DE L'OPERATEUR: DEFI_GROUP

C     IN:
C          MA    : NOM DU MAILLAGE
C          NBGNP : NOMBRE DE GROUP_NO A CREER
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

      CHARACTER*8 MA,NONO,NOGNO,NOGNO2,K8B,KPOS,NOM1,CRIT,PREFIX,NOM2
      CHARACTER*16 CONCEP,CMD,OPTION
      CHARACTER*24 NOMNOE,GRPNOE,COOVAL,LISNO,LISNOM
      CHARACTER*80 CARD
C     ------------------------------------------------------------------

      CALL JEMARQ()

C     RECUPERATION DU NIVEAU D'IMPRESSION
C     -----------------------------------
      CALL INFNIV(IFM,NIV)

      CALL GETRES(K8B,CONCEP,CMD)
      LISNO = '&&SSCGNO.LISTE_NOEUDS'
      NOMNOE = MA//'.NOMNOE         '
      GRPNOE = MA//'.GROUPENO       '
      COOVAL = MA//'.COORDO    .VALE'
      CALL JEVEUO(COOVAL,'L',JVALE)
      CALL JELIRA(GRPNOE,'NMAXOC',NBGRMN,K8B)
      NBIS = NBGRMN
      NBK8 = NBGRMN
      CALL WKVECT('&&SSCGNO.LIK8','V V K8',NBK8,IALIK8)
      CALL WKVECT('&&SSCGNO.LII1','V V I',NBIS,IALII1)
      CALL WKVECT('&&SSCGNO.LII2','V V I',NBIS,IALII2)

      CALL GETFAC('CREA_GROUP_NO',NBOCC)
      NBGNAJ = 0

      DO 240,IOCC = 1,NBOCC
        CALL GETVEM(MA,'NOEUD','CREA_GROUP_NO','NOEUD',IOCC,1,0,K8B,N2)
        CALL GETVID('CREA_GROUP_NO','INTERSEC',IOCC,1,0,K8B,N3)
        CALL GETVID('CREA_GROUP_NO','UNION',IOCC,1,0,K8B,N4)
        CALL GETVID('CREA_GROUP_NO','DIFFE',IOCC,1,0,K8B,N5)
        CALL GETVEM(MA,'GROUP_MA','CREA_GROUP_NO','GROUP_MA',IOCC,1,0,
     &              K8B,N6)
        CALL GETVTX('CREA_GROUP_NO','TOUT_GROUP_MA',IOCC,1,0,K8B,N7)
        CALL GETVEM(MA,'GROUP_NO','CREA_GROUP_NO','GROUP_NO',IOCC,1,0,
     &              K8B,N8)
        CALL GETVTX('CREA_GROUP_NO','OPTION',IOCC,1,0,OPTION,N9)

CJMP
        N10 = 0
C       VERFICATIONS SUPPLEMENTAIRES
        IF (N9.NE.0) THEN
          CALL GETVTX('CREA_GROUP_NO','OPTION',IOCC,1,1,OPTION,NB)
          IF (OPTION(1:10).EQ.'NOEUD_ORDO') THEN
            N10 = 1
          ELSE
            N10 = 0
          END IF
          IF (OPTION(1:13).EQ.'SEGM_DROI_ORDO') THEN
            NOM1 = '--------'
            CALL GETVTX('CREA_GROUP_NO','POSITION',IOCC,1,0,K8B,N11)
            CALL GETVIS('CREA_GROUP_NO','NUME_INIT',IOCC,1,0,IBID,N12)
            CALL GETVIS('CREA_GROUP_NO','NUME_FIN',IOCC,1,0,IBID,N13)
            NTEST = ABS(N3) + ABS(N4) + ABS(N5) + ABS(N6) + ABS(N7) +
     &              ABS(N11) + ABS(N12) + ABS(N13)
            CALL UTRENO('CREA_GROUP_NO','ORIG',IOCC,MA,NOM1)
            CALL UTRENO('CREA_GROUP_NO','EXTR',IOCC,MA,NOM2)
            IF ((NOM1.NE.'        ') .OR. (NOM2.NE.'        ')) THEN
              IF (NTEST.NE.0) THEN
                CALL UTMESS('F',CMD,'NOEUD ORIGINE INCOMPATIBLE '//
     &                'AVEC INTERSEC,UNION,DIFFE,TOUT_GROUP_MA,GROUP_MA'
     &                      //'POSITION,NUME_INIT,NUME_FIN')
              END IF
            END IF
            CALL GETVR8('CREA_GROUP_NO','PRECISION',IOCC,1,1,TOLE,N14)
            IF (N14.EQ.0) CALL UTMESS('F',CMD,'IL FAUT UNE PRECISION')
            CALL GETVTX('CREA_GROUP_NO','CRITERE',IOCC,1,1,CRIT,N15)
            IF (N15.EQ.0) CALL UTMESS('F',CMD,'IL FAUT UN CRITERE')
          END IF
        END IF


C       -- MOTS CLEFS GROUP_MA ET TOUT_GROUP_MA:
C       ----------------------------------------

CJMP

        IF (N10.EQ.0) THEN

          IF ((N6+N7).LT.0) THEN
            CALL SSGNGM(MA,IOCC,NBGNA2)
            NBGNAJ = NBGNAJ + NBGNA2
            GO TO 240
          END IF

        END IF

C       -- AUTRES MOTS CLEFS : NOEUD, INTER, UNION, DIFFE, GROUP_NO:
C       ------------------------------------------------------------
        CALL GETVID('CREA_GROUP_NO','NOM',IOCC,1,1,NOGNO,N1)

        CALL JENONU(JEXNOM(GRPNOE,NOGNO),IRET)
        IF (IRET.GT.0) THEN
          CALL UTMESS('A','SSCGNO',' LE GROUP_NO : '//NOGNO//
     &                ' EXISTE DEJA : ON NE LE CREE DONC PAS.')
          GO TO 240
        END IF

C       -- MOTS CLEFS : NOEUD_ORIG, NOEUD_EXTR:
C       ---------------------------------------
        CALL UTRENO('CREA_GROUP_NO','ORIG',IOCC,MA,NOM1)
        CALL JENONU(JEXNOM(NOMNOE,NOM1),NUMORI)
        CALL UTRENO('CREA_GROUP_NO','EXTR',IOCC,MA,NOM1)
        CALL JENONU(JEXNOM(NOMNOE,NOM1),NUMEXT)

        IF (NUMORI.EQ.0 .AND. NUMEXT.NE.0) THEN
          CALL UTMESS('F',CMD,'IL FAUT DEFINIR LE NOEUD ORIGINE')
        END IF
        IF (NUMORI.NE.0 .AND. NUMEXT.EQ.0) THEN
          CALL UTMESS('F',CMD,'IL FAUT DEFINIR LE NOEUD EXTREMITE')
        END IF

        N2 = -N2
        N3 = -N3
        N4 = -N4
        N5 = -N5
        N8 = -N8
        N9 = -N9

C       -- MOT CLEF NOEUD:
C       -------------------
        IF (N2.GT.0) THEN
          IF (N9.GT.0) THEN
            CALL GETVTX('CREA_GROUP_NO','OPTION',IOCC,1,1,OPTION,NB)
            IF (OPTION(1:14).NE.'SEGM_DROI_ORDO') GO TO 210
          END IF
          CALL WKVECT('&&SSCGNO.L_NOEUD','V V K8',N2,ILNOK8)
          CALL GETVEM(MA,'NOEUD','CREA_GROUP_NO','NOEUD',IOCC,1,N2,
     &                ZK8(ILNOK8),NB)
          CALL WKVECT('&&SSCGNO.NOEUD','V V I',N2,JNOEU)
          CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNOT,K8B,IERD)
          CALL WKVECT('&&SSCGNO.NOEUD2','V V I',NBNOT,JNOEU2)
C         --- ON VERIFIE QUE TOUS LES NOEUDS SONT DISTINCTS ---
          NBNO = 0
          IER = 0
          DO 10 IM1 = 1,N2
            NOM1 = ZK8(ILNOK8+IM1-1)
            CALL JENONU(JEXNOM(NOMNOE,NOM1),NUM)
            IF (NUM.EQ.0) THEN
              IER = IER + 1
              CALL UTMESS('E','SSCGNO','LE NOEUD : '//NOM1//
     &                    ' N''APPARTIENT PAS AU MAILLAGE')
              GO TO 10
            END IF
            ZI(JNOEU2-1+NUM) = ZI(JNOEU2-1+NUM) + 1
            IF (ZI(JNOEU2-1+NUM).EQ.2) THEN
              CALL UTMESS('A',CMD,'NOEUD EN DOUBLE : '//NOM1//
     &                    ' DANS LE GROUP_NO: '//NOGNO)
              GO TO 10
            END IF
            NBNO = NBNO + 1
            ZI(JNOEU+NBNO-1) = NUM
   10     CONTINUE
          IF (IER.NE.0) CALL UTMESS('F',CMD,'ARRET SUR ERREUR')

          IF (NUMORI.NE.0) THEN
            CALL GETVR8('CREA_GROUP_NO','PRECISION',IOCC,1,1,TOLE,N10)
            IF (N10.EQ.0) CALL UTMESS('F',CMD,'IL FAUT UNE PRECISION')
            CALL GETVTX('CREA_GROUP_NO','CRITERE',IOCC,1,1,CRIT,N10)
            IF (N10.EQ.0) CALL UTMESS('F',CMD,'IL FAUT UN CRITERE')
            CALL OREINO(MA,ZI(JNOEU),NBNO,NUMORI,NUMEXT,ZR(JVALE),CRIT,
     &                  TOLE,IRET)
            IF (IRET.NE.0) CALL UTMESS('F','SSCGNO','ARRET SUR ERREURS')
          END IF

          CALL JECROC(JEXNOM(GRPNOE,NOGNO))
          CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',NBNO,K8B)
          CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGMA)
          DO 20 INO = 0,NBNO - 1
            ZI(IAGMA+INO) = ZI(JNOEU+INO)
   20     CONTINUE
          NBGNAJ = NBGNAJ + 1
          CALL JEDETR('&&SSCGNO.NOEUD')
          CALL JEDETR('&&SSCGNO.NOEUD2')
          CALL JEDETR('&&SSCGNO.L_NOEUD')
          GO TO 240
        END IF


C       -- MOT CLEF GROUP_NO:
C       ---------------------
        IF (N8.GT.0) THEN
          CALL GETVEM(MA,'GROUP_NO','CREA_GROUP_NO','GROUP_NO',IOCC,1,1,
     &                NOGNO2,NBID)
          CALL JENONU(JEXNOM(GRPNOE,NOGNO2),IGN2)
          IF (IGN2.EQ.0) CALL UTMESS('F','SSCGNO',
     &                               'LE GROUP_NO : '//NOGNO2//
     &                               ' N''APPARTIENT PAS AU MAILLAGE')
          CALL JELIRA(JEXNUM(GRPNOE,IGN2),'LONMAX',ILI2,K8B)
          CALL JEVEUO(JEXNUM(GRPNOE,IGN2),'L',IAGN2)

          IF (NUMORI.NE.0) THEN
            CALL GETVTX('CREA_GROUP_NO','OPTION',IOCC,1,1,OPTION,NB)
            IF (OPTION(1:14).NE.'SEGM_DROI_ORDO') THEN
              CALL UTDEBM('F',CMD,'ERREUR DANS LES DONNEES')
              CALL UTIMPK('L','OPTION ',1,OPTION)
              CALL UTIMPK('S',' NON VALIDE POUR ',1,'ORIG ET EXTR')
              CALL UTFINM()
            END IF
            CALL GETVR8('CREA_GROUP_NO','PRECISION',IOCC,1,1,TOLE,N10)
            IF (N10.EQ.0) CALL UTMESS('F',CMD,'IL FAUT UNE PRECISION')
            CALL GETVTX('CREA_GROUP_NO','CRITERE',IOCC,1,1,CRIT,N10)
            IF (N10.EQ.0) CALL UTMESS('F',CMD,'IL FAUT UN CRITERE')
            CALL JECROC(JEXNOM(GRPNOE,NOGNO))
            CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',ILI2,K8B)
            CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGNO)
            DO 30 INO = 0,ILI2 - 1
              ZI(IAGNO+INO) = ZI(IAGN2+INO)
   30       CONTINUE
            CALL OREINO(MA,ZI(IAGNO),ILI2,NUMORI,NUMEXT,ZR(JVALE),CRIT,
     &                  TOLE,IRET)
            IF (IRET.NE.0) CALL UTMESS('F','SSCGNO','ARRET SUR ERREURS')
            GO TO 240
          END IF

          CALL GETVTX('CREA_GROUP_NO','POSITION',IOCC,1,0,KPOS,N6B)
          IND1 = 0
          IND2 = 0
          IF (N6B.EQ.0) THEN
            CALL GETVIS('CREA_GROUP_NO','NUME_INIT',IOCC,1,1,IND1,N6A)
            IF (N6A.EQ.0) IND1 = 1
            CALL GETVIS('CREA_GROUP_NO','NUME_FIN',IOCC,1,1,IND2,N6A)
            IF (N6A.EQ.0) IND2 = ILI2
            IF (IND2.LT.IND1) CALL UTMESS('F','SSCGNO',
     &               'L''INDICE FINAL EST INFERIEUR A L''INDICE INITIAL'
     &                             )
            IF (ILI2.LT.IND2) CALL UTMESS('F','SSCGNO',
     &             'L''INDICE FINAL EST SUPERIEUR A LA TAILLE DU GROUPE'
     &                             )
            N6A = IND2 - IND1 + 1
          ELSE
            N6A = 1
          END IF

          CALL JECROC(JEXNOM(GRPNOE,NOGNO))
          CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',N6A,K8B)
          CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGNO)
          NBGNAJ = NBGNAJ + 1
          IF (N6B.NE.0) GO TO 50
          N = IND2 - IND1 + 1
          DO 40 II = 1,N
            ZI(IAGNO-1+II) = ZI(IAGN2-2+IND1+II)
   40     CONTINUE
          GO TO 240
   50     CONTINUE
          CALL GETVTX('CREA_GROUP_NO','POSITION',IOCC,1,1,KPOS,N6B)
          IF (KPOS.EQ.'INIT') THEN
            ZI(IAGNO) = ZI(IAGN2)
          ELSE IF (KPOS.EQ.'FIN') THEN
            II = ILI2
            ZI(IAGNO) = ZI(IAGN2+II-1)
          ELSE IF (KPOS.EQ.'MILIEU') THEN
            II = (ILI2+1)/2
            ZI(IAGNO) = ZI(IAGN2+II-1)
          END IF
          GO TO 240
        END IF


C       -- MOT CLEF INTER:
C       -------------------
        IF (N3.GT.0) THEN
          CALL GETVID('CREA_GROUP_NO','INTERSEC',IOCC,1,N3,ZK8(IALIK8),
     &                NBID)
          IER = 0
          DO 60,IGN = 1,N3
            CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8-1+IGN)),IGN2)
            IF (IGN2.EQ.0) THEN
              CALL UTMESS('E','SSCGNO','LE GROUP_NO : '//
     &                    ZK8(IALIK8-1+IGN)//
     &                    ' N''APPARTIENT PAS AU MAILLAGE')
              IER = IER + 1
            END IF
   60     CONTINUE
          IF (IER.NE.0) CALL UTMESS('F',CMD,'ARRET SUR ERREUR')

          CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8)),IGN1)
          CALL JELIRA(JEXNUM(GRPNOE,IGN1),'LONMAX',ILI1,K8B)
          CALL JEVEUO(JEXNUM(GRPNOE,IGN1),'L',IAGM1)
          IF (ILI1.GT.NBIS) THEN
            NBIS = 2*ILI1
            CALL JEDETR('&&SSCGNO.LII1')
            CALL JEDETR('&&SSCGNO.LII2')
            CALL WKVECT('&&SSCGNO.LII1','V V I',NBIS,IALII1)
            CALL WKVECT('&&SSCGNO.LII2','V V I',NBIS,IALII2)
          END IF
          N = ILI1
          DO 70 II = 1,N
            ZI(IALII1-1+II) = ZI(IAGM1-1+II)
   70     CONTINUE

          DO 90,IGN = 2,N3
            CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8-1+IGN)),IGN2)
            CALL JELIRA(JEXNUM(GRPNOE,IGN2),'LONMAX',ILI2,K8B)
            CALL JEVEUO(JEXNUM(GRPNOE,IGN2),'L',IAGM2)
            CALL UTLISI('INTER',ZI(IALII1),N,ZI(IAGM2),ILI2,ZI(IALII2),
     &                  NBIS,NTROU)
            N = NTROU
            DO 80 II = 1,N
              ZI(IALII1-1+II) = ZI(IALII2-1+II)
   80       CONTINUE
   90     CONTINUE

          IF (N.EQ.0) THEN
            CALL UTMESS('A','SSCGNO','LE GROUP_NO :'//NOGNO//
     &                  ' EST VIDE, ON NE LE CREE PAS.')
            GO TO 240
          END IF
          CALL JECROC(JEXNOM(GRPNOE,NOGNO))
          CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',N,K8B)
          CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGMA)
          DO 100 II = 1,N
            ZI(IAGMA-1+II) = ZI(IALII1-1+II)
  100     CONTINUE
          NBGNAJ = NBGNAJ + 1
          GO TO 240
        END IF


C       -- MOT CLEF UNION:
C       -------------------
        IF (N4.GT.0) THEN
          CALL GETVID('CREA_GROUP_NO','UNION',IOCC,1,N4,ZK8(IALIK8),
     &                NBID)
          IER = 0
          DO 110,IGN = 1,N4
            CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8-1+IGN)),IGN2)
            IF (IGN2.EQ.0) THEN
              CALL UTMESS('E','SSCGNO','LE GROUP_NO : '//
     &                    ZK8(IALIK8-1+IGN)//
     &                    ' N''APPARTIENT PAS AU MAILLAGE')
              IER = IER + 1
            END IF
  110     CONTINUE
          IF (IER.NE.0) CALL UTMESS('F',CMD,'ARRET SUR ERREUR')

          CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8)),IGN1)
          CALL JELIRA(JEXNUM(GRPNOE,IGN1),'LONMAX',ILI1,K8B)
          CALL JEVEUO(JEXNUM(GRPNOE,IGN1),'L',IAGM1)
          IF (ILI1.GT.NBIS) THEN
            NBIS = 2*ILI1
            CALL JEDETR('&&SSCGNO.LII1')
            CALL JEDETR('&&SSCGNO.LII2')
            CALL WKVECT('&&SSCGNO.LII1','V V I',NBIS,IALII1)
            CALL WKVECT('&&SSCGNO.LII2','V V I',NBIS,IALII2)
          END IF
          N = ILI1
          DO 120 II = 1,N
            ZI(IALII1-1+II) = ZI(IAGM1-1+II)
  120     CONTINUE

          DO 140,IGN = 2,N4
            CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8-1+IGN)),IGN2)
            CALL JELIRA(JEXNUM(GRPNOE,IGN2),'LONMAX',ILI2,K8B)
            CALL JEVEUO(JEXNUM(GRPNOE,IGN2),'L',IAGM2)
            CALL UTLISI('UNION',ZI(IALII1),N,ZI(IAGM2),ILI2,ZI(IALII2),
     &                  NBIS,NTROU)

            IF (NTROU.LT.0) THEN
              NBIS = -2*NTROU
              CALL JEDETR('&&SSCGNO.LII2')
              CALL WKVECT('&&SSCGNO.LII2','V V I',NBIS,IALII2)
              CALL UTLISI('UNION',ZI(IALII1),N,ZI(IAGM2),ILI2,
     &                    ZI(IALII2),NBIS,NTROU)
              CALL JEDETR('&&SSCGNO.LII1')
              CALL WKVECT('&&SSCGNO.LII1','V V I',NBIS,IALII1)
            END IF
            N = NTROU
            DO 130 II = 1,N
              ZI(IALII1-1+II) = ZI(IALII2-1+II)
  130       CONTINUE
  140     CONTINUE

          IF (N.EQ.0) THEN
            CALL UTMESS('A','SSCGNO','LE GROUP_NO :'//NOGNO//
     &                  'EST VIDE. ON NE LE CREE PAS.')
          ELSE
            CALL JECROC(JEXNOM(GRPNOE,NOGNO))
            CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',N,K8B)
            CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGMA)
            DO 150 II = 1,N
              ZI(IAGMA-1+II) = ZI(IALII1-1+II)
  150       CONTINUE
            NBGNAJ = NBGNAJ + 1
          END IF
          GO TO 240
        END IF


C       -- MOT CLEF DIFFE:
C       -------------------
        IF (N5.GT.0) THEN
          CALL GETVID('CREA_GROUP_NO','DIFFE',IOCC,1,N5,ZK8(IALIK8),
     &                NBID)
          IER = 0
          DO 160,IGN = 1,N5
            CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8-1+IGN)),IGN2)
            IF (IGN2.EQ.0) THEN
              CALL UTMESS('E','SSCGNO','LE GROUP_NO : '//
     &                    ZK8(IALIK8-1+IGN)//
     &                    ' N''APPARTIENT PAS AU MAILLAGE')
              IER = IER + 1
            END IF
  160     CONTINUE
          IF (IER.NE.0) CALL UTMESS('F',CMD,'ARRET SUR ERREUR')

          CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8)),IGN1)
          CALL JELIRA(JEXNUM(GRPNOE,IGN1),'LONMAX',ILI1,K8B)
          CALL JEVEUO(JEXNUM(GRPNOE,IGN1),'L',IAGM1)
          IF (ILI1.GT.NBIS) THEN
            NBIS = 2*ILI1
            CALL JEDETR('&&SSCGNO.LII1')
            CALL JEDETR('&&SSCGNO.LII2')
            CALL WKVECT('&&SSCGNO.LII1','V V I',NBIS,IALII1)
            CALL WKVECT('&&SSCGNO.LII2','V V I',NBIS,IALII2)
          END IF
          N = ILI1
          DO 170 II = 1,N
            ZI(IALII1-1+II) = ZI(IAGM1-1+II)
  170     CONTINUE

          DO 190,IGN = 2,N5
            CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8-1+IGN)),IGN2)
            CALL JELIRA(JEXNUM(GRPNOE,IGN2),'LONMAX',ILI2,K8B)
            CALL JEVEUO(JEXNUM(GRPNOE,IGN2),'L',IAGM2)
            CALL UTLISI('DIFFE',ZI(IALII1),N,ZI(IAGM2),ILI2,ZI(IALII2),
     &                  NBIS,NTROU)
            N = NTROU
            DO 180 II = 1,N
              ZI(IALII1-1+II) = ZI(IALII2-1+II)
  180       CONTINUE
  190     CONTINUE

          IF (N.EQ.0) THEN
            CALL UTMESS('A','SSCGNO','LE GROUP_NO :'//NOGNO//
     &                  'EST VIDE. ON NE LE CREE PAS.')
          ELSE
            CALL JECROC(JEXNOM(GRPNOE,NOGNO))
            CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',N,K8B)
            CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGMA)
            DO 200 II = 1,N
              ZI(IAGMA-1+II) = ZI(IALII1-1+II)
  200       CONTINUE
            NBGNAJ = NBGNAJ + 1
          END IF
          GO TO 240
        END IF

C       -- MOT CLEF OPTION:
C       -------------------
  210   CONTINUE
        IF (N9.GT.0) THEN

          CALL GETVTX('CREA_GROUP_NO','OPTION',IOCC,1,1,OPTION,NB)

C            -- TRAITEMENT DE L'OPTION ENV_SPHERE :
C               ---------------------------------
          IF (OPTION(1:10).EQ.'ENV_SPHERE') THEN
            CALL CGNOES('CREA_GROUP_NO',IOCC,MA,LISNO,NBNO)

C            -- TRAITEMENT DE L'OPTION ENV_CYLINDRE :
C               -----------------------------------
          ELSE IF (OPTION(1:12).EQ.'ENV_CYLINDRE') THEN
            CALL CGNOEC('CREA_GROUP_NO',IOCC,MA,LISNO,NBNO)

C            -- TRAITEMENT DE L'OPTION PLAN :
C               ---------------------------
          ELSE IF (OPTION(1:4).EQ.'PLAN') THEN
            CALL CGNOPL('CREA_GROUP_NO',IOCC,MA,LISNO,NBNO)
CJMP
C            -- TRAITEMENT DE L'OPTION NOEUD_ORDO :
C               ---------------------------------
          ELSE IF (OPTION(1:10).EQ.'NOEUD_ORDO') THEN
            PREFIX = '&&SSCGNO'
            CALL FONFIS ( PREFIX, MA, 'CREA_GROUP_NO', IOCC, 'V')

            LISNOM = PREFIX//'.FOND      .NOEU'
            CALL JELIRA(LISNOM,'LONMAX',NBNO,K8B)
            CALL JEVEUO(LISNOM,'L',IDNONO)

            CALL WKVECT(LISNO,'V V I',NBNO,IDLINO)
            DO 220 I = 1,NBNO
              CALL JENONU(JEXNOM(NOMNOE,ZK8(IDNONO-1+I)),NUNO)
              ZI(IDLINO-1+I) = NUNO
  220       CONTINUE

            CALL JEDETR ( LISNOM )

          END IF

C        -- CREATION ET AFFECTATION DU GROUP_NO :
C            ----------------------------------
          IF (NBNO.EQ.0) THEN
            CALL UTMESS('A','SSCGNO','LE GROUP_NO :'//NOGNO//
     &                  'EST VIDE. ON NE LE CREE PAS.')
          ELSE
            CALL JEVEUO(LISNO,'L',IDLINO)

            CALL JECROC(JEXNOM(MA//'.GROUPENO',NOGNO))
            CALL JEECRA(JEXNOM(MA//'.GROUPENO',NOGNO),'LONMAX',NBNO,K8B)
            CALL JEVEUO(JEXNOM(MA//'.GROUPENO',NOGNO),'E',IAGMA)

            DO 230 II = 1,NBNO
              ZI(IAGMA-1+II) = ZI(IDLINO-1+II)
  230       CONTINUE
            NBGNAJ = NBGNAJ + 1

          END IF

          CALL JEDETR(LISNO)

        END IF

  240 CONTINUE

C     IMPRESSIONS NIVEAUX 1 ET 2
C     --------------------------
      IF (NIV.GE.1 .AND. NBGNAJ.NE.0) THEN
        WRITE (IFM,'(/,/,A,I6,/,39(''=''))')
     &    'NOMBRE  DE GROUPES DE NOEUDS CREES : ',NBGNAJ

        IF (NBOCC.GE.1) THEN
          WRITE (IFM,'(/,15X,38(''-''),2(/,15X,A),/,15X,38(''-''))')
     &      '! NOM DU GROUPE ! NBRE DE NOEUDS DU  !',
     &      '!    NOEUDS     !      GROUPE_NO     !'

          DO 250 I = 1,NBGNAJ
            II = NBGNIN + I
            CALL JENUNO(JEXNUM(GRPNOE,II),NOGNO)
            CALL JELIRA(JEXNUM(GRPNOE,II),'LONMAX',NBNO,K8B)
            WRITE (IFM,'(15X,A,2X,A8,5X,A,2X,I8,10X,A)') '!',NOGNO,'!',
     &        NBNO,'!'
  250     CONTINUE
          WRITE (IFM,'(15X,38(''-''),/)')
        END IF
      END IF

C     IMPRESSIONS NIVEAU 2
C     --------------------
      IF (NIV.EQ.2 .AND. NBGNAJ.NE.0) THEN
        MAXCOL = 8
        DO 280 I = 1,NBGNAJ
          II = NBGNIN + I
          CALL JEVEUO(JEXNUM(GRPNOE,II),'L',IAGNO)
          CALL JENUNO(JEXNUM(GRPNOE,II),NOGNO)
          CALL JELIRA(JEXNUM(GRPNOE,II),'LONMAX',NBNO,K8B)
          WRITE (IFM,'(/,3A,/,27(''-''))') 'NOEUDS DU GROUPE ',NOGNO,
     &      ' :'
          NBLINE = NBNO/MAXCOL
          IRESTE = MOD(NBNO,MAXCOL)
          IF (IRESTE.NE.0) NBLINE = NBLINE + 1
          NBCOL = MAXCOL
          KKK = 0
          DO 270 JJJ = 1,NBLINE
            IF (IRESTE.NE.0 .AND. JJJ.EQ.NBLINE) NBCOL = IRESTE
            DO 260 III = 1,NBCOL
              KKK = KKK + 1
              CALL JENUNO(JEXNUM(NOMNOE,ZI(IAGNO-1+KKK)),NONO)
              CARD((III-1)*10+1:) = ' '//NONO//' '
  260       CONTINUE
            WRITE (IFM,'(A))') CARD(:10*NBCOL)
  270     CONTINUE
  280   CONTINUE
        WRITE (IFM,'(/,/)')
      END IF

      CALL JEDETC('V','&&SSCGNO',1)
      CALL JEDEMA()

      END
