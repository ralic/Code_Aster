      SUBROUTINE SSCGNO ( MA, NBGNIN )
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C
C     IN:  MA    : NOM DU MAILLAGE
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

      INTEGER       NALAR
      CHARACTER*8  ALARM
      CHARACTER*8  MA,NONO,NOGNO,NOGNO2,K8B,KPOS,NOM1,PREFIX
      CHARACTER*16 CONCEP,CMD,OPTION, MOTCLE, TYPMCL, MOTFAC
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

      MOTCLE = 'GROUP_MA'
      TYPMCL = 'GROUP_MA'

      MOTFAC = 'CREA_GROUP_NO'
      CALL GETFAC ( MOTFAC, NBOCC )
      CALL GETVTX(' ','ALARME',1,1,1,ALARM,NALAR)
      NBGNAJ = 0

C ----------------------------------------------------------------------

      DO 100 , IOCC = 1 , NBOCC

        CALL GETVID ( MOTFAC, 'NOEUD'        , IOCC,1,0, K8B, N2 )
        CALL GETVID ( MOTFAC, 'INTERSEC'     , IOCC,1,0, K8B, N3 )
        CALL GETVID ( MOTFAC, 'UNION'        , IOCC,1,0, K8B, N4 )
        CALL GETVID ( MOTFAC, 'DIFFE'        , IOCC,1,0, K8B, N5 )
        CALL GETVID ( MOTFAC, 'GROUP_MA'     , IOCC,1,0, K8B, N6 )
        CALL GETVTX ( MOTFAC, 'TOUT_GROUP_MA', IOCC,1,0, K8B, N7 )
        CALL GETVID ( MOTFAC, 'GROUP_NO'     , IOCC,1,0, K8B, N8 )
        CALL GETVTX ( MOTFAC, 'OPTION'       , IOCC,1,0, K8B, N9 )

C ----------------------------------------------------------------------
C ----- MOT CLEF "TOUT_GROUP_MA" :
C       --------------------------
        IF ( N7.LT.0 ) THEN
           CALL SSGNGM ( MA, IOCC, NBGNA2 )
           NBGNAJ = NBGNAJ + NBGNA2
           GO TO 100
        END IF

C ----------------------------------------------------------------------
C ----- MOT CLEF "GROUP_MA" :
C       ---------------------
        IF ( N6.LT.0 .AND. N9.EQ.0 ) THEN
           CALL SSGNGM ( MA, IOCC, NBGNA2 )
           NBGNAJ = NBGNAJ + NBGNA2
           GO TO 100
        END IF

C ----------------------------------------------------------------------

        CALL GETVID ( MOTFAC, 'NOM', IOCC,1,1, NOGNO, N1 )
        CALL JENONU ( JEXNOM(GRPNOE,NOGNO), IRET )
        IF (IRET.GT.0) THEN
            IF (ALARM.EQ.'OUI') THEN
              CALL U2MESK('A','SOUSTRUC_37',1,NOGNO)
            END IF
          GO TO 100
        END IF

        N2 = -N2
        N3 = -N3
        N4 = -N4
        N5 = -N5
        N8 = -N8
        N9 = -N9

C ----------------------------------------------------------------------
C ----- MOT CLEF "INTERSEC" :
C       ---------------------
        IF (N3.GT.0) THEN
          CALL GETVEM(MA,'GROUP_NO',MOTFAC,'INTERSEC',IOCC,1,N3,
     &                ZK8(IALIK8),NBID)

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
          DO 30 II = 1,N
            ZI(IALII1-1+II) = ZI(IAGM1-1+II)
 30       CONTINUE

          DO 32,IGN = 2,N3
            CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8-1+IGN)),IGN2)
            CALL JELIRA(JEXNUM(GRPNOE,IGN2),'LONMAX',ILI2,K8B)
            CALL JEVEUO(JEXNUM(GRPNOE,IGN2),'L',IAGM2)
            CALL UTLISI('INTER',ZI(IALII1),N,ZI(IAGM2),ILI2,ZI(IALII2),
     &                  NBIS,NTROU)
            N = NTROU
            DO 34 II = 1,N
              ZI(IALII1-1+II) = ZI(IALII2-1+II)
 34         CONTINUE
 32       CONTINUE

          IF (N.EQ.0) THEN
            IF (ALARM.EQ.'OUI') THEN
              CALL U2MESK('A','SOUSTRUC_38',1,NOGNO)
            END IF
            GO TO 100
          END IF
          CALL JECROC(JEXNOM(GRPNOE,NOGNO))
          CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',N,K8B)
          CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGMA)
          DO 36 II = 1,N
            ZI(IAGMA-1+II) = ZI(IALII1-1+II)
 36       CONTINUE
          NBGNAJ = NBGNAJ + 1
          GO TO 100
        END IF

C ----------------------------------------------------------------------
C ----- MOT CLEF "UNION" :
C       ------------------
        IF (N4.GT.0) THEN
          CALL GETVEM(MA,'GROUP_NO',MOTFAC,'UNION',IOCC,1,N4,
     &                ZK8(IALIK8),NBID)

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
          DO 40 II = 1,N
            ZI(IALII1-1+II) = ZI(IAGM1-1+II)
 40       CONTINUE

          DO 42,IGN = 2,N4
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
            DO 44 II = 1,N
              ZI(IALII1-1+II) = ZI(IALII2-1+II)
 44         CONTINUE
 42       CONTINUE

          IF (N.EQ.0) THEN
            IF (ALARM.EQ.'OUI') THEN
              CALL U2MESK('A','SOUSTRUC_38',1,NOGNO)
            END IF
          ELSE
            CALL JECROC(JEXNOM(GRPNOE,NOGNO))
            CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',N,K8B)
            CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGMA)
            DO 46 II = 1,N
              ZI(IAGMA-1+II) = ZI(IALII1-1+II)
 46         CONTINUE
            NBGNAJ = NBGNAJ + 1
          END IF
          GO TO 100
        END IF

C ----------------------------------------------------------------------
C ----- MOT CLEF "DIFFE" :
C       ------------------
        IF (N5.GT.0) THEN
          CALL GETVEM(MA,'GROUP_NO',MOTFAC,'DIFFE',IOCC,1,N5,
     &                ZK8(IALIK8),NBID)

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
          DO 50 II = 1,N
            ZI(IALII1-1+II) = ZI(IAGM1-1+II)
 50       CONTINUE

          DO 52,IGN = 2,N5
            CALL JENONU(JEXNOM(GRPNOE,ZK8(IALIK8-1+IGN)),IGN2)
            CALL JELIRA(JEXNUM(GRPNOE,IGN2),'LONMAX',ILI2,K8B)
            CALL JEVEUO(JEXNUM(GRPNOE,IGN2),'L',IAGM2)
            CALL UTLISI('DIFFE',ZI(IALII1),N,ZI(IAGM2),ILI2,ZI(IALII2),
     &                  NBIS,NTROU)
            N = NTROU
            DO 54 II = 1,N
              ZI(IALII1-1+II) = ZI(IALII2-1+II)
 54         CONTINUE
 52       CONTINUE

          IF (N.EQ.0) THEN
            IF (ALARM.EQ.'OUI') THEN
              CALL U2MESK('A','SOUSTRUC_38',1,NOGNO)
            END IF
          ELSE
            CALL JECROC(JEXNOM(GRPNOE,NOGNO))
            CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',N,K8B)
            CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGMA)
            DO 56 II = 1,N
              ZI(IAGMA-1+II) = ZI(IALII1-1+II)
 56         CONTINUE
            NBGNAJ = NBGNAJ + 1
          END IF
          GO TO 100
        END IF

C ----------------------------------------------------------------------
C ----- MOT CLEF "OPTION" :
C       -------------------
        IF (N9.GT.0) THEN
          CALL GETVTX ( MOTFAC, 'OPTION', IOCC,1,1, OPTION, N9 )

C         -- TRAITEMENT DE L'OPTION "ENV_SPHERE" :
C         ----------------------------------------
          IF (OPTION(1:10).EQ.'ENV_SPHERE') THEN
            CALL CGNOES(MOTFAC,IOCC,MA,LISNO,NBNO)

C         -- TRAITEMENT DE L'OPTION "ENV_CYLINDRE" :
C         ------------------------------------------
          ELSE IF (OPTION(1:12).EQ.'ENV_CYLINDRE') THEN
            CALL CGNOEC(MOTFAC,IOCC,MA,LISNO,NBNO)

C         -- TRAITEMENT DE L'OPTION "PLAN" :
C         ----------------------------------
          ELSE IF (OPTION(1:4).EQ.'PLAN') THEN
            CALL CGNOPL(MOTFAC,IOCC,MA,LISNO,NBNO)

C         -- TRAITEMENT DE L'OPTION "SEGM_DROI_ORDO" :
C         --------------------------------------------
          ELSE IF (OPTION(1:14).EQ.'SEGM_DROI_ORDO') THEN
            CALL CGNOSO(MOTFAC,IOCC,MA,LISNO,NBNO)

C         -- TRAITEMENT DE L'OPTION "TUNNEL" :
C         ------------------------------------
          ELSE IF (OPTION(1:6).EQ.'TUNNEL') THEN
            CALL CGNOFU(MOTFAC,IOCC,MA,LISNO,NBNO)

C         -- TRAITEMENT DE L'OPTION "NOEUD_ORDO" :
C         ----------------------------------------
          ELSE IF (OPTION(1:10).EQ.'NOEUD_ORDO') THEN
            PREFIX = '&&SSCGNO'
            CALL FONFIS ( PREFIX, MA, MOTFAC, IOCC,
     &                                        1, MOTCLE, TYPMCL, 'V' )
            LISNOM = PREFIX//'.FOND      .NOEU'
            CALL JELIRA(LISNOM,'LONMAX',NBNO,K8B)
            CALL JEVEUO(LISNOM,'L',IDNONO)

            CALL WKVECT(LISNO,'V V I',NBNO,IDLINO)
            DO 90 I = 1,NBNO
              CALL JENONU(JEXNOM(NOMNOE,ZK8(IDNONO-1+I)),NUNO)
              ZI(IDLINO-1+I) = NUNO
 90         CONTINUE

            CALL JEDETR ( PREFIX//'.FOND      .NOEU' )
            CALL JEDETR ( PREFIX//'.FOND      .TYPE' )

          ELSE
            CALL U2MESK('F','PREPOST3_82',1,OPTION)
          END IF

C         -- CREATION ET AFFECTATION DU GROUP_NO :
C         ----------------------------------------
          IF (NBNO.EQ.0) THEN
            IF (ALARM.EQ.'OUI') THEN
              CALL U2MESK('A','SOUSTRUC_38',1,NOGNO)
            END IF
          ELSE
            CALL JEVEUO(LISNO,'L',IDLINO)

            CALL JECROC(JEXNOM(MA//'.GROUPENO',NOGNO))
            CALL JEECRA(JEXNOM(MA//'.GROUPENO',NOGNO),'LONMAX',NBNO,K8B)
            CALL JEVEUO(JEXNOM(MA//'.GROUPENO',NOGNO),'E',IAGMA)

            DO 92 II = 1,NBNO
              ZI(IAGMA-1+II) = ZI(IDLINO-1+II)
 92         CONTINUE
            NBGNAJ = NBGNAJ + 1

          END IF
          CALL JEDETR ( LISNO )
          GO TO 100
        END IF

C ----------------------------------------------------------------------
C ----- MOT CLEF "NOEUD" :
C       ------------------
        IF ( N2.GT.0 ) THEN
          CALL WKVECT('&&SSCGNO.L_NOEUD','V V K8',N2,ILNOK8)
          CALL GETVEM(MA,'NOEUD',MOTFAC,'NOEUD',IOCC,1,N2,
     &                ZK8(ILNOK8),NB)
          CALL WKVECT('&&SSCGNO.NOEUD','V V I',N2,JNOEU)
          CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNOT,K8B,IERD)
          CALL WKVECT('&&SSCGNO.NOEUD2','V V I',NBNOT,JNOEU2)
C         --- ON VERIFIE QUE TOUS LES NOEUDS SONT DISTINCTS ---
          NBNO = 0
          DO 20 IM1 = 1,N2
            NOM1 = ZK8(ILNOK8+IM1-1)
            CALL JENONU(JEXNOM(NOMNOE,NOM1),NUM)
            ZI(JNOEU2-1+NUM) = ZI(JNOEU2-1+NUM) + 1
            IF (ZI(JNOEU2-1+NUM).EQ.2) THEN
              CALL UTMESS('A',CMD,'NOEUD EN DOUBLE : '//NOM1//
     &                            ' DANS LE GROUP_NO: '//NOGNO)
C        CALL U2MESK('A','SOUSTRUC_39', 2 ,VALK)
              GOTO 20
            END IF
            NBNO = NBNO + 1
            ZI(JNOEU+NBNO-1) = NUM
 20       CONTINUE
C
          CALL JECROC(JEXNOM(GRPNOE,NOGNO))
          CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',NBNO,K8B)
          CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGMA)
          DO 22 INO = 0,NBNO - 1
            ZI(IAGMA+INO) = ZI(JNOEU+INO)
 22       CONTINUE
          NBGNAJ = NBGNAJ + 1
          CALL JEDETR('&&SSCGNO.NOEUD')
          CALL JEDETR('&&SSCGNO.NOEUD2')
          CALL JEDETR('&&SSCGNO.L_NOEUD')
          GO TO 100
        END IF

C ----------------------------------------------------------------------
C ----- MOT CLEF "GROUP_NO" :
C       ---------------------
        IF ( N8.GT.0 ) THEN
          CALL GETVEM(MA,'GROUP_NO',MOTFAC,'GROUP_NO',IOCC,1,1,
     &                NOGNO2,NBID)
          CALL JENONU(JEXNOM(GRPNOE,NOGNO2),IGN2)
          CALL JELIRA(JEXNUM(GRPNOE,IGN2),'LONMAX',ILI2,K8B)
          CALL JEVEUO(JEXNUM(GRPNOE,IGN2),'L',IAGN2)
C
          CALL GETVTX(MOTFAC,'POSITION',IOCC,1,0,KPOS,N6B)
          IND1 = 0
          IND2 = 0
          IF (N6B.EQ.0) THEN
            CALL GETVIS(MOTFAC,'NUME_INIT',IOCC,1,1,IND1,N6A)
            IF (N6A.EQ.0) IND1 = 1
            CALL GETVIS(MOTFAC,'NUME_FIN',IOCC,1,1,IND2,N6A)
            IF (N6A.EQ.0) IND2 = ILI2
            IF (IND2.LT.IND1) CALL U2MESS('F','SOUSTRUC_33')
            IF (ILI2.LT.IND2) CALL U2MESS('F','SOUSTRUC_34')
            N6A = IND2 - IND1 + 1
          ELSE
            N6A = 1
          END IF

          CALL JECROC(JEXNOM(GRPNOE,NOGNO))
          CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',N6A,K8B)
          CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGNO)
          NBGNAJ = NBGNAJ + 1
          IF (N6B.NE.0) GO TO 80
          N = IND2 - IND1 + 1
          DO 82 II = 1,N
            ZI(IAGNO-1+II) = ZI(IAGN2-2+IND1+II)
 82       CONTINUE
          GO TO 100
 80       CONTINUE
          CALL GETVTX(MOTFAC,'POSITION',IOCC,1,1,KPOS,N6B)
          IF (KPOS.EQ.'INIT') THEN
            ZI(IAGNO) = ZI(IAGN2)
          ELSE IF (KPOS.EQ.'FIN') THEN
            II = ILI2
            ZI(IAGNO) = ZI(IAGN2+II-1)
          ELSE IF (KPOS.EQ.'MILIEU') THEN
            II = (ILI2+1)/2
            ZI(IAGNO) = ZI(IAGN2+II-1)
          END IF
          GO TO 100
        END IF

C ----------------------------------------------------------------------

 100  CONTINUE

C ----------------------------------------------------------------------
C --- IMPRESSIONS NIVEAUX 1 ET 2 :
C     --------------------------
      IF (NIV.GE.1 .AND. NBGNAJ.NE.0) THEN
        WRITE (IFM,'(/,/,A,I6,/,39(''=''))')
     &    'NOMBRE  DE GROUPES DE NOEUDS CREES : ',NBGNAJ

        IF (NBOCC.GE.1) THEN
          WRITE (IFM,'(/,15X,38(''-''),2(/,15X,A),/,15X,38(''-''))')
     &      '! NOM DU GROUPE ! NBRE DE NOEUDS DU  !',
     &      '!    NOEUDS     !      GROUPE_NO     !'

          DO 200 I = 1,NBGNAJ
            II = NBGNIN + I
            CALL JENUNO(JEXNUM(GRPNOE,II),NOGNO)
            CALL JELIRA(JEXNUM(GRPNOE,II),'LONMAX',NBNO,K8B)
            WRITE (IFM,'(15X,A,2X,A8,5X,A,2X,I8,10X,A)') '!',NOGNO,'!',
     &        NBNO,'!'
 200      CONTINUE
          WRITE (IFM,'(15X,38(''-''),/)')
        END IF
      END IF

C ----------------------------------------------------------------------
C --- IMPRESSIONS NIVEAU 2 :
C     --------------------
      IF (NIV.EQ.2 .AND. NBGNAJ.NE.0) THEN
        MAXCOL = 8
        DO 300 I = 1,NBGNAJ
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
          DO 302 JJJ = 1,NBLINE
            IF (IRESTE.NE.0 .AND. JJJ.EQ.NBLINE) NBCOL = IRESTE
            DO 304 III = 1,NBCOL
              KKK = KKK + 1
              CALL JENUNO(JEXNUM(NOMNOE,ZI(IAGNO-1+KKK)),NONO)
              CARD((III-1)*10+1:) = ' '//NONO//' '
 304        CONTINUE
            WRITE (IFM,'(A))') CARD(:10*NBCOL)
 302      CONTINUE
 300    CONTINUE
        WRITE (IFM,'(/,/)')
      END IF

C ----------------------------------------------------------------------
      CALL JEDETC('V','&&SSCGNO',1)
      CALL JEDEMA()

      END
