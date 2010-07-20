      SUBROUTINE CALYRC(CHARGZ)
      IMPLICIT NONE
      CHARACTER*(*) CHARGZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/07/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C TOLE CRP_20

C     CREER LES CARTES CHAR.CHXX.CMULT ET CHAR.CHXX.CIMPO
C          ET REMPLIR LIGRCH, POUR LE MOT-CLE LIAISON_CYCL
C     (COMMANDES AFFE_CHAR_MECA ET AFFE_CHAR_THER)

C IN  : CHARGE : NOM UTILISATEUR DU RESULTAT DE CHARGE
C-----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNUM
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------

      INTEGER K,KK,NUNO1,NUNO2,INO1,INO2,NDIM,IER,NOCC,IOCC
      INTEGER IBID,JNOMA,NNOMX,IDMAX,IDNOMN,IDCOEF,JCMUC,IDNOMD
      INTEGER IDIREC,IDIMEN,IAGMA1,IAGMA2,NBMA1,NBMA2
      INTEGER NBNO2,IDCAL1,IDCAL2, NUL
      INTEGER ICONB1,ICONU1,ICOCF1,ICONB2,ICONU2,ICOCF2
      INTEGER NNO11,NNO12,I,INDIRE,LNO
      INTEGER NBTYP,NDDL2,JLISTK,JDIM,NDIM1,IRET,NBPAR
      INTEGER JNUNOE,JNORM,IDIM,IJ,NORIEN,NTRAIT
      INTEGER ICOEF1,ICOEF2,ICOEF3,IAGNO3,NBNO3,NBMA3,IDMAI3
      LOGICAL LROTA,DNOR,LREORI
      REAL*8       BETA,COEF1,MROTA(3,3),ZERO,NORMAL(3)
      REAL*8       R8B,PREC,ARMIN
      REAL*8       COEF11,COEF12,COEF3
      COMPLEX*16   CBID,BETAC
      CHARACTER*1  KB
      CHARACTER*2  TYPLAG
      CHARACTER*4  FONREE
      CHARACTER*4  TYPCOE,ZCST,TYPLIA
      CHARACTER*8  K8B,NOMA,MO,M8BLAN
      CHARACTER*8  KBETA,NONO1,NONO2,CHARGE,CMP,DDL2,LISTYP(10)
      CHARACTER*16 MOTFAC,CORES1,CORES2,TYMOCL(4),MOTCLE(4),NOMCMD
      CHARACTER*19 LIGRMO, NOMT19
      CHARACTER*19 LISREL
      CHARACTER*24 GEOM3, PARA
      CHARACTER*24 VALK(2)
C ----------------------------------------------------------------------

      CALL JEMARQ()
      NUL=0
      MOTFAC = 'LIAISON_CYCL'
      CALL GETFAC(MOTFAC,NOCC)
      IF (NOCC.EQ.0) GO TO 310

      CALL GETRES(KB,KB,NOMCMD)
      IF (NOMCMD.EQ.'AFFE_CHAR_MECA') THEN
        TYPLIA = 'DEPL'
      ELSE IF (NOMCMD.EQ.'AFFE_CHAR_THER') THEN
        TYPLIA = 'TEMP'
      ELSE
        CALL ASSERT(.FALSE.)
      END IF


      FONREE = 'REEL'
      TYPCOE = 'REEL'
      CHARGE = CHARGZ

      LISREL = '&&CALYRC.RLLISTE'
      ZERO = 0.0D0
      BETA = 0.0D0
      BETAC = (0.0D0,0.0D0)
      KBETA = ' '
      TYPLAG = '12'
      M8BLAN = '        '
      NDIM1 = 3
      LREORI = .FALSE.

      CALL DISMOI('F','NOM_MODELE',CHARGE(1:8),'CHARGE',IBID,MO,IER)
      LIGRMO = MO//'.MODELE'
      CALL JEVEUO(LIGRMO//'.LGRF','L',JNOMA)
      NOMA = ZK8(JNOMA)

      CALL DISMOI('F','DIM_GEOM',MO,'MODELE',NDIM,K8B,IER)
      IF (.NOT.(NDIM.EQ.2.OR.NDIM.EQ.3))
     &    CALL U2MESS('F','MODELISA2_6')

      IF (NDIM.EQ.2) THEN
        NBTYP = 3
        LISTYP(1) = 'SEG2'
        LISTYP(2) = 'SEG3'
        LISTYP(3) = 'SEG4'
      ELSE IF (NDIM.EQ.3) THEN
        NBTYP = 10
        LISTYP(1) = 'TRIA3'
        LISTYP(2) = 'TRIA6'
        LISTYP(3) = 'TRIA9'
        LISTYP(4) = 'QUAD4'
        LISTYP(5) = 'QUAD8'
        LISTYP(6) = 'QUAD9'
        LISTYP(7) = 'QUAD12'
        LISTYP(8) = 'SEG2'
        LISTYP(9) = 'SEG3'
        LISTYP(10) = 'SEG4'
      END IF
C
C --- RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE
C
      CALL JEEXIN ( NOMA//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( NOMA , 'CARA_GEOM' , NOMT19 )
         NBPAR = 0
         PARA = 'AR_MIN                  '
         CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, CBID, K8B,
     &                K8B, R8B , PARA, K8B, IBID, ARMIN, CBID,
     &                K8B, IRET )
          IF ( IRET .EQ. 0 ) THEN
             PREC = ARMIN*1.D-06
          ELSEIF ( IRET .EQ. 1 ) THEN
             PREC = 1.D-10
          ELSE
             CALL U2MESS('F','MODELISA2_13')
          ENDIF
      ELSE
         CALL U2MESS('F','MODELISA3_18')
      ENDIF

      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NNOMX,KB,IER)
      IDMAX = NNOMX + 3
      CALL WKVECT('&&CALYRC.NOMNOE','V V K8',IDMAX,IDNOMN)
      CALL WKVECT('&&CALYRC.NOMDDL','V V K8',IDMAX,IDNOMD)
      CALL WKVECT('&&CALYRC.COEF','V V R',IDMAX,IDCOEF)
      CALL WKVECT('&&CALYRC.COEMUC','V V C',1,JCMUC)
      CALL WKVECT('&&CALYRC.DIRECT','V V R',IDMAX*3,IDIREC)
      CALL WKVECT('&&CALYRC.DIMENSION','V V I',IDMAX,IDIMEN)

      CORES1 = '&&CALYRC.CORES1'
      CORES2 = '&&CALYRC.CORES2'

      DO 300 IOCC = 1,NOCC

        DNOR = .FALSE.
        IF (TYPLIA.EQ.'DEPL') THEN
          CALL GETVTX(MOTFAC,'DDL_ESCL',IOCC,1,1,DDL2,NDDL2)
          IF (NDDL2.GT.0) DNOR = .TRUE.
        END IF

C        1.1 RECUPERATION DE LA LISTE DES MAILLE_MAIT :
C        ----------------------------------------------
C        -- 1er groupe maitre --
        MOTCLE(1) = 'MAILLE_MAIT1'
        TYMOCL(1) = 'MAILLE'
        MOTCLE(2) = 'GROUP_MA_MAIT1'
        TYMOCL(2) = 'GROUP_MA'
        CALL RELIEM(MO,NOMA,'NU_MAILLE',MOTFAC,IOCC,2,MOTCLE,TYMOCL,
     &              '&&CALYRC.LIMANU1',NBMA1)
        CALL JEVEUO('&&CALYRC.LIMANU1','L',IAGMA1)
C        -- 2eme groupe maitre --
        MOTCLE(1) = 'MAILLE_MAIT2'
        TYMOCL(1) = 'MAILLE'
        MOTCLE(2) = 'GROUP_MA_MAIT2'
        TYMOCL(2) = 'GROUP_MA'
        CALL RELIEM(MO,NOMA,'NU_MAILLE',MOTFAC,IOCC,2,MOTCLE,TYMOCL,
     &              '&&CALYRC.LIMANU2',NBMA2)
        IF (NBMA2.GT.0) THEN
          CALL JEVEUO('&&CALYRC.LIMANU2','L',IAGMA2)
        ENDIF

C        1.2 RECUPERATION DES NOEUD_ESCL
C        -------------------------------
        IF (.NOT.DNOR) THEN

C        -- RECUPERATION DE LA LISTE DES NOEUD_ESCL :
C        --------------------------------------------
          MOTCLE(1) = 'NOEUD_ESCL'
          TYMOCL(1) = 'NOEUD'
          MOTCLE(2) = 'GROUP_NO_ESCL'
          TYMOCL(2) = 'GROUP_NO'
          MOTCLE(3) = 'MAILLE_ESCL'
          TYMOCL(3) = 'MAILLE'
          MOTCLE(4) = 'GROUP_MA_ESCL'
          TYMOCL(4) = 'GROUP_MA'
          CALL RELIEM(' ',NOMA,'NU_NOEUD',MOTFAC,IOCC,4,MOTCLE,TYMOCL,
     &                '&&CALYRC.LINONU',NBNO3)
          CALL JEVEUO('&&CALYRC.LINONU','L',IAGNO3)

        ELSE

C        -- RECUPERATION DE LA LISTE DES MAILLE_ESCL :
C        ---------------------------------------------
          MOTCLE(1) = 'MAILLE_ESCL'
          TYMOCL(1) = 'MAILLE'
          MOTCLE(2) = 'GROUP_MA_ESCL'
          TYMOCL(2) = 'GROUP_MA'
          CALL RELIEM(MO,NOMA,'NU_MAILLE',MOTFAC,IOCC,2,MOTCLE,TYMOCL,
     &                '&&CALYRC.LIMANU3',NBMA3)
          IF (NBMA3.EQ.0) THEN
            VALK(1) = MOTCLE(1)
            VALK(2) = MOTCLE(2)
            CALL U2MESG('F', 'MODELISA8_49',2,VALK,0,0,0,0.D0)
          END IF
          CALL JEVEUO('&&CALYRC.LIMANU3','L',IDMAI3)

          NORIEN = 0
          CALL ORILMA ( NOMA, NDIM, ZI(IDMAI3), NBMA3, NORIEN, NTRAIT,
     &                  LREORI, PREC, NUL, IBID )
          IF (NORIEN.NE.0) THEN
              CALL U2MESS('F','MODELISA3_19')
          END IF

C ---        CREATION DU TABLEAU DES NUMEROS DES NOEUDS '&&NBNLMA.LN'
C ---        ET DES NOMBRES D'OCCURENCES DE CES NOEUDS '&&NBNLMA.NBN'
C ---        DES MAILLES DE PEAU MAILLE_ESCL :
C            -------------------------------
          CALL NBNLMA(NOMA,NBMA3,ZI(IDMAI3),NBTYP,LISTYP,NBNO3)

C ---        CALCUL DES NORMALES EN CHAQUE NOEUD :
C            -----------------------------------
          CALL WKVECT('&&CALYRC.LISTK','V V K8',1,JLISTK)
          CALL JEVEUO('&&NBNLMA.LN','L',JNUNOE)

C ---        CREATION DU TABLEAU D'INDIRECTION ENTRE LES INDICES
C ---        DU TABLEAU DES NORMALES ET LES NUMEROS DES NOEUDS :
C            -------------------------------------------------
          CALL WKVECT('&&CALYRC.INDIRE','V V I',NNOMX,INDIRE)
          CALL JELIRA('&&NBNLMA.LN','LONUTI',LNO,KB)

          DO 20 I = 1,LNO
            ZI(INDIRE+ZI(JNUNOE+I-1)-1) = I
   20     CONTINUE

          CALL CANORT(NOMA,NBMA3,ZI(IDMAI3),ZK8(JLISTK),NDIM,NBNO3,
     &                ZI(JNUNOE),1)
          CALL JEVEUO('&&CANORT.NORMALE','L',JNORM)
          CALL JEDUPO('&&NBNLMA.LN3','V','&&CALYRC.LINONU',.FALSE.)
          CALL JEVEUO('&&CALYRC.LINONU','L',IAGNO3)
        END IF


C       1.3 TRANSFORMATION DE LA GEOMETRIE DE GRNO2 :
C       ------------------------------------------
        GEOM3 = '&&CALYRC.GEOM_TRANSF'
        CALL CALYRG(IOCC,NDIM,NOMA,'&&CALYRC.LINONU',GEOM3,MROTA,LROTA)

C       2. CALCUL DE CORRES :
C       -------------------
        IF (NDIM.EQ.2) THEN
C        -- 1er groupe esclave / 1er groupe maitre --
          CALL PJ2DCO('PARTIE',MO,MO,NBMA1,ZI(IAGMA1),NBNO3,ZI(IAGNO3),
     &                ' ',GEOM3,CORES1,.FALSE.,R8B)
          IF (NBMA2.GT.0) THEN
C        -- 1er groupe esclave  / 2eme groupe maitre --
          CALL PJ2DCO('PARTIE',MO,MO,NBMA2,ZI(IAGMA2),NBNO3,ZI(IAGNO3),
     &                ' ',GEOM3,CORES2,.FALSE.,R8B)
          ENDIF
        ELSE IF (NDIM.EQ.3) THEN
C        -- 1er groupe esclave / 1er groupe maitre --
          CALL PJ3DCO('PARTIE',MO,MO,NBMA1,ZI(IAGMA1),NBNO3,ZI(IAGNO3),
     &                ' ',GEOM3,CORES1,.FALSE.,R8B)
          IF (NBMA2.GT.0) THEN
C        -- 1er groupe esclave  / 2eme groupe maitre --
          CALL PJ3DCO('PARTIE',MO,MO,NBMA2,ZI(IAGMA2),NBNO3,ZI(IAGNO3),
     &                ' ',GEOM3,CORES2,.FALSE.,R8B)
          ENDIF
        END IF

C        -- 1er groupe maitre --
        CALL JEVEUO(CORES1//'.PJEF_NB','L',ICONB1)
        CALL JEVEUO(CORES1//'.PJEF_NU','L',ICONU1)
        CALL JEVEUO(CORES1//'.PJEF_CF','L',ICOCF1)
        CALL JELIRA(CORES1//'.PJEF_NB','LONMAX',NBNO2,KB)

        IF (NBMA2.GT.0) THEN
C        -- 2eme groupe maitre --
          CALL JEVEUO(CORES2//'.PJEF_NB','L',ICONB2)
          CALL JEVEUO(CORES2//'.PJEF_NU','L',ICONU2)
          CALL JEVEUO(CORES2//'.PJEF_CF','L',ICOCF2)
C         CALL JELIRA(CORES2//'.PJEF_NB','LONMAX',NBNO2,KB)
        ENDIF


C       3. ECRITURE DES RELATIONS LINEAIRES :
C       =====================================

C       3.0 RECUPERATION D'UN FACTEUR :
C       =================
C        -- 1er groupe maitre --
        CALL GETVR8(MOTFAC,'COEF_MAIT1',IOCC,1,1,COEF11,ICOEF1)
        IF (ICOEF1.LE.0) THEN
          COEF11 = 1.D0
        END IF
        IF (NBMA2.GT.0) THEN
C        -- 2eme groupe maitre --
        CALL GETVR8(MOTFAC,'COEF_MAIT2',IOCC,1,1,COEF12,ICOEF2)
          IF (ICOEF2.LE.0) THEN
            COEF12 = 1.D0
          END IF
        ENDIF
C        -- 1er groupe esclave --
        CALL GETVR8(MOTFAC,'COEF_ESCL',IOCC,1,1,COEF3,ICOEF3)
        IF (ICOEF3.LE.0) THEN
          COEF3 = 1.D0
        END IF

C       3.1 CAS "DEPL" :
C       =================
        IF (TYPLIA.EQ.'DEPL') THEN

C       -- 3.1.1 S'IL N'Y A PAS DE ROTATION :
C       -------------------------------------
          IF (.NOT.LROTA) THEN
            IDCAL1 = 0
            IDCAL2 = 0
            DO 90 INO2 = 1,NBNO3
C           NNO11: NB DE NOEUD_MAIT LIES A INO2 SELON CORES1
              NNO11 = ZI(ICONB1-1+INO2)
C           NNO12: NB DE NOEUD_MAIT LIES A INO2 SELON CORES2
               IF (NBMA2.GT.0) THEN
                NNO12 = ZI(ICONB2-1+INO2)
              ELSE
                NNO12 = 0
              ENDIF
              IF ((NNO11.EQ.0) .AND. (NNO12.EQ.0)) GO TO 90

              NUNO2 = INO2
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO2),NONO2)

              ZK8(IDNOMN-1+1) = NONO2
              ZR(IDCOEF-1+1) = -1.D0*COEF3

              DO 30,INO1 = 1,NNO11
                NUNO1 = ZI(ICONU1+IDCAL1-1+INO1)
                COEF1 = ZR(ICOCF1+IDCAL1-1+INO1)
                CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO1),NONO1)
                ZK8(IDNOMN+INO1) = NONO1
                ZR(IDCOEF+INO1) = COEF1*COEF11
   30         CONTINUE
              DO 40,INO1 = 1,NNO12
                NUNO1 = ZI(ICONU2+IDCAL2-1+INO1)
                COEF1 = ZR(ICOCF2+IDCAL2-1+INO1)
                CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO1),NONO1)
                ZK8(IDNOMN+NNO11+INO1) = NONO1
                ZR(IDCOEF+NNO11+INO1) = COEF1*COEF12
   40         CONTINUE

C           -- AFFECTATION DES RELATIONS CONCERNANT LE NOEUD INO2 :
C           -----------------------------------------------------
              IF (DNOR) THEN
                DO 60 INO1 = 1,NNO11 + NNO12 + 1
                  ZI(IDIMEN+INO1-1) = NDIM
                  ZK8(IDNOMD-1+INO1) = 'DEPL'
                  DO 50 IDIM = 1,NDIM
                    ZR(IDIREC+ (INO1-1)*NDIM1+IDIM-1) = ZR(JNORM+
     &                (ZI(INDIRE+INO2-1)-1)*NDIM+IDIM-1)
   50             CONTINUE
   60           CONTINUE
                CALL AFRELA(ZR(IDCOEF),ZC(JCMUC),ZK8(IDNOMD),
     &                      ZK8(IDNOMN),ZI(IDIMEN),ZR(IDIREC),
     &                      NNO11+NNO12+1,BETA,BETAC,KBETA,TYPCOE,
     &                      FONREE,TYPLAG,1.D-6,LISREL)
              ELSE
                DO 80,K = 1,NDIM
                  IF (K.EQ.1) CMP = 'DX'
                  IF (K.EQ.2) CMP = 'DY'
                  IF (K.EQ.3) CMP = 'DZ'
                  DO 70,INO1 = 1,NNO11 + NNO12 + 1
                    ZK8(IDNOMD-1+INO1) = CMP
   70             CONTINUE
                  CALL AFRELA(ZR(IDCOEF),ZC(JCMUC),ZK8(IDNOMD),
     &                        ZK8(IDNOMN),ZI(IDIMEN),ZR(IDIREC),
     &                        NNO11+NNO12+1,BETA,BETAC,KBETA,TYPCOE,
     &                        FONREE,TYPLAG,1.D-6,LISREL)
                  CALL IMPREL(MOTFAC,NNO11+NNO12+1,ZR(IDCOEF),
     &                        ZK8(IDNOMD),ZK8(IDNOMN),BETA)
   80           CONTINUE
              END IF
              IDCAL1 = IDCAL1 + NNO11
              IDCAL2 = IDCAL2 + NNO12
   90       CONTINUE

C       -- 3.1.2  S'IL Y A UNE ROTATION :
C       ---------------------------------
          ELSE
            IDCAL1 = 0
            IDCAL2 = 0

            DO 250 INO2 = 1,NBNO2

C ---       NNO1: NB DE NOEUD_MAIT LIES A INO2 :
C           ------------------------------------
              NNO11 = ZI(ICONB1-1+INO2)
              IF (NBMA2.GT.0) THEN
                NNO12 = ZI(ICONB2-1+INO2)
              ELSE
                NNO12 = 0
              ENDIF
              IF ((NNO11.EQ.0) .AND. (NNO12.EQ.0)) GO TO 250
              DO 110 K = 1,IDMAX
                ZK8(IDNOMN+K-1) = M8BLAN
                ZK8(IDNOMD+K-1) = M8BLAN
                ZR(IDCOEF+K-1) = ZERO
                ZI(IDIMEN+K-1) = 0
                DO 100 KK = 1,3
                  ZR(IDIREC+3* (K-1)+KK-1) = ZERO
  100           CONTINUE
  110         CONTINUE

              NORMAL(1) = ZERO
              NORMAL(2) = ZERO
              NORMAL(3) = ZERO

              NUNO2 = INO2
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO2),NONO2)

              IF (DNOR) THEN
                IJ = 1
              ELSE
                IJ = NDIM
              END IF

              DO 120,INO1 = 1,NNO11
                NUNO1 = ZI(ICONU1+IDCAL1-1+INO1)
                COEF1 = ZR(ICOCF1+IDCAL1-1+INO1)
                CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO1),NONO1)
                ZK8(IDNOMN+IJ+INO1-1) = NONO1
                ZR(IDCOEF+IJ+INO1-1) = COEF1*COEF11
  120         CONTINUE
              DO 130,INO1 = 1,NNO12
                NUNO1 = ZI(ICONU2+IDCAL2-1+INO1)
                COEF1 = ZR(ICOCF2+IDCAL2-1+INO1)
                CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO1),NONO1)
                ZK8(IDNOMN+NNO11+IJ+INO1-1) = NONO1
                ZR(IDCOEF+NNO11+IJ+INO1-1) = COEF1*COEF12
  130         CONTINUE


C           -- AFFECTATION DES RELATIONS CONCERNANT LE NOEUD INO2 :
C           -----------------------------------------------------
              IF (DNOR) THEN
                DO 150 IDIM = 1,NDIM
                  DO 140 JDIM = 1,NDIM
                    NORMAL(IDIM) = NORMAL(IDIM) +
     &                             MROTA(JDIM,IDIM)*ZR(JNORM+
     &                             (ZI(INDIRE+INO2-1)-1)*NDIM+JDIM-1)
  140             CONTINUE
  150           CONTINUE
                ZR(IDCOEF+1-1) = 1.0D0*COEF3
                ZK8(IDNOMN+1-1) = NONO2
                ZK8(IDNOMD+1-1) = 'DEPL'
                ZI(IDIMEN+1-1) = NDIM
                DO 160 IDIM = 1,NDIM
                  ZR(IDIREC+IDIM-1) = ZR(JNORM+
     &                                (ZI(INDIRE+INO2-1)-1)*NDIM+IDIM-1)
  160           CONTINUE
                DO 180 INO1 = 2,NNO11 + 1
                  ZI(IDIMEN+INO1-1) = NDIM
                  ZK8(IDNOMD-1+INO1) = 'DEPL'
                  DO 170 IDIM = 1,NDIM
                    ZR(IDIREC+ (INO1-1)*NDIM1+IDIM-1) = -NORMAL(IDIM)
  170             CONTINUE
  180           CONTINUE
                DO 200 INO1 = 2,NNO12 + 1
                  ZI(IDIMEN+NNO11+INO1-1) = NDIM
                  ZK8(IDNOMD-1+NNO11+INO1) = 'DEPL'
                  DO 190 IDIM = 1,NDIM
                    ZR(IDIREC+NNO11+ (INO1-1)*NDIM1+IDIM-
     &                1) = -NORMAL(IDIM)
  190             CONTINUE
  200           CONTINUE
                CALL AFRELA(ZR(IDCOEF),ZC(JCMUC),ZK8(IDNOMD),
     &                      ZK8(IDNOMN),ZI(IDIMEN),ZR(IDIREC),
     &                      NNO11+NNO12+1,BETA,BETAC,KBETA,TYPCOE,
     &                      FONREE,TYPLAG,1.D-6,LISREL)
              ELSE
                DO 240,K = 1,NDIM
                  IF (K.EQ.1) CMP = 'DX'
                  IF (K.EQ.2) CMP = 'DY'
                  IF (K.EQ.3) CMP = 'DZ'
                  DO 210,INO1 = 1,NNO11
                    ZK8(IDNOMD+NDIM+INO1-1) = CMP
  210             CONTINUE
                  DO 220,INO1 = 1,NNO12
                    ZK8(IDNOMD+NNO11+NDIM+INO1-1) = CMP
  220             CONTINUE
                  DO 230 KK = 1,NDIM
                    IF (KK.EQ.1) CMP = 'DX'
                    IF (KK.EQ.2) CMP = 'DY'
                    IF (KK.EQ.3) CMP = 'DZ'
                    ZK8(IDNOMN+KK-1) = NONO2
                    ZK8(IDNOMD+KK-1) = CMP
                    ZR(IDCOEF+KK-1) = -MROTA(KK,K)*COEF3
  230             CONTINUE
                  CALL AFRELA(ZR(IDCOEF),ZC(JCMUC),ZK8(IDNOMD),
     &                        ZK8(IDNOMN),ZI(IDIMEN),ZR(IDIREC),
     &                        NNO11+NNO12+NDIM,BETA,BETAC,KBETA,TYPCOE,
     &                        FONREE,TYPLAG,1.D-6,LISREL)
                  CALL IMPREL(MOTFAC,NNO11+NNO12+NDIM,ZR(IDCOEF),
     &                        ZK8(IDNOMD),ZK8(IDNOMN),BETA)
  240           CONTINUE
              END IF
              IDCAL1 = IDCAL1 + NNO11
              IDCAL2 = IDCAL2 + NNO12
  250       CONTINUE
          END IF


C       3.2 CAS "TEMP" :
C       =================
        ELSE IF (TYPLIA.EQ.'TEMP') THEN
          IDCAL1 = 0
          IDCAL2 = 0
          DO 290 INO2 = 1,NBNO2
C           NNO1: NB DE NOEUD_MAIT LIES A INO2
            NNO11 = ZI(ICONB1-1+INO2)
            NNO12 = ZI(ICONB2-1+INO2)
            IF ((NNO11.EQ.0) .AND. (NNO12.EQ.0)) GO TO 290

            NUNO2 = INO2
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO2),NONO2)

            ZK8(IDNOMN-1+1) = NONO2
            ZR(IDCOEF-1+1) = -1.D0*COEF3

            DO 260,INO1 = 1,NNO11
              NUNO1 = ZI(ICONU1+IDCAL1-1+INO1)
              COEF1 = ZR(ICOCF1+IDCAL1-1+INO1)
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO1),NONO1)
              ZK8(IDNOMN+INO1) = NONO1
              ZR(IDCOEF+INO1) = COEF1*COEF11
  260       CONTINUE
            DO 270,INO1 = 1,NNO12
              NUNO1 = ZI(ICONU2+IDCAL2-1+INO1)
              COEF1 = ZR(ICOCF2+IDCAL2-1+INO1)
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO1),NONO1)
              ZK8(IDNOMN+NNO11+INO1) = NONO1
              ZR(IDCOEF+NNO11+INO1) = COEF1*COEF12
  270       CONTINUE

C           -- AFFECTATION DE LA RELATION CONCERNANT LE NOEUD INO2 :
C           -----------------------------------------------------
            CMP = 'TEMP'
            DO 280,INO1 = 1,NNO11 + NNO12 + 1
              ZK8(IDNOMD-1+INO1) = CMP
  280       CONTINUE
            CALL AFRELA(ZR(IDCOEF),ZC(JCMUC),ZK8(IDNOMD),ZK8(IDNOMN),
     &                  ZI(IDIMEN),ZR(IDIREC),NNO11+NNO12+1,BETA,BETAC,
     &                  KBETA,TYPCOE,FONREE,TYPLAG,1.D-6,LISREL)
            CALL IMPREL(MOTFAC,NNO11+NNO12+1,ZR(IDCOEF),ZK8(IDNOMD),
     &                  ZK8(IDNOMN),BETA)
            IDCAL1 = IDCAL1 + NNO11
            IDCAL2 = IDCAL2 + NNO12
  290     CONTINUE
        ELSE
          CALL ASSERT(.FALSE.)
        END IF

        CALL DETRSD('CORRESP_2_MAILLA',CORES1)
        CALL DETRSD('CORRESP_2_MAILLA',CORES2)
        CALL JEDETR(GEOM3)
        CALL JEDETR('&&CALYRC.LIMANU1')
        CALL JEDETR('&&CALYRC.LIMANU2')
        CALL JEDETR('&&CALYRC.LIMANU3')
        CALL JEDETR('&&CALYRC.LINONU')
        CALL JEDETR('&&CALYRC.LISTK')
        CALL JEDETR('&&CALYRC.INDIRE')
        CALL JEDETR('&&NBNLMA.LN')
        CALL JEDETR('&&NBNLMA.NBN')
        CALL JEDETR('&&CANORT.NORMALE')

  300 CONTINUE

      CALL JEDETR('&&CALYRC.COEMUC')
      CALL JEDETR('&&CALYRC.NOMNOE')
      CALL JEDETR('&&CALYRC.NOMDDL')
      CALL JEDETR('&&CALYRC.COEF')
      CALL JEDETR('&&CALYRC.DIRECT')
      CALL JEDETR('&&CALYRC.DIMENSION')

C --- AFFECTATION DE LA LISTE DE RELATIONS A LA CHARGE :
C     ------------------------------------------------
      CALL AFLRCH(LISREL,CHARGE)

  310 CONTINUE
      CALL JEDEMA()
      END
