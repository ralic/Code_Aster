      SUBROUTINE NMDORC(MODELZ,COMPOZ,CARCRI)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C RESPONSABLE PROIX J-M.PROIX
      IMPLICIT NONE
      CHARACTER*(*) MODELZ,COMPOZ
      CHARACTER*24  CARCRI
C ----------------------------------------------------------------------
C MODIF ALGORITH  DATE 22/10/2007   AUTEUR PELLET J.PELLET 
C     SAISIE ET VERIFICATION DE LA RELATION DE COMPORTEMENT UTILISEE
C
C IN  MODELZ  : NOM DU MODELE
C OUT COMPOZ  : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
C OUT CARCRI  : CARTE DECRIVANT LES CRITERES LOCAUX DE CONVERGENCE
C                     0 : ITER_INTE_MAXI
C                     1 : COMPOSANTE INUTILISEE
C                     2 : RESI_INTE_RELA
C                     3 : THETA (POUR THM)
C                     4 : ITER_INTE_PAS
C                     5 : RESO_INTE (0: EULER_1, 1: RK_2, 2: RK_4)
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

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
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER NCMPMA,DIMAKI,N2,N3,IBID,NBOCC,I,ICMP,ICOMEL,II,JMA,JNCMP
      INTEGER JNOMA,JVALV,K,N1,NBMA,NBMO1,NBVARI,NC1,NC2
      INTEGER NBMAT,JMAIL,NCOMEL,NS1,JMESM,IMA,IM,IRET,ICPRI,NBSYST
      INTEGER INV,DIMANV,NBMONO,NUNIT,ITEINT,ITEPAS,NUMGD,JACMP,NBCRIT
      INTEGER JCRIT,JVALC,IMPEXP,TYPTGT,ITDEBO
      REAL*8 RBID,RESI,THETA,R8VIDE,PERT,RESID
      COMPLEX*16 CBID
      LOGICAL      BUG, NIVO
      CHARACTER*1  K1BID
C    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
      PARAMETER (DIMAKI=9)
C    DIMAKI = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
      PARAMETER (DIMANV=4)
      PARAMETER (NCMPMA=7+DIMAKI+DIMANV)
      LOGICAL EXIST,GETEXM,EXICP,EXI1D,CRILOC,EXIFIB
      CHARACTER*8 NOMA,NOMGRD,NOMCMP(NCMPMA),K8B,TYPMCL(2),SDCOMP,CHMAT
      CHARACTER*16 COMP,DEFO,MOCLEF(2),K16BID,NOMCMD,MOCLES(2),TYPMAT
      CHARACTER*16 VALCMP(NCMPMA),TXCP,TX1D,RESO
      CHARACTER*19 COMPOR,CHS(2),CHS3
      CHARACTER*24 LIGRMO,MODELE,MESMAI
      CHARACTER*50 CHAIN1, CHAIN2, VALK(4)
C    POUR COMPORTEMENT KIT_
      INTEGER NBVEL(DIMAKI)
      INTEGER NBNVI(DIMANV)
      CHARACTER*16 COMEL(DIMAKI)

      REAL*8 LCOER(2)
      COMPLEX*16 LCOEC(2)
      LOGICAL LCUMU(2),LCOC(2)
      DATA LCUMU/.FALSE.,.FALSE./
      DATA LCOC/.FALSE.,.FALSE./
      DATA LCOER/1.D0,1.D0/
      DATA PERT/0.D0/

      DATA NOMGRD/'COMPOR  '/
      DATA NOMCMP/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',
     &     'C_PLAN  ','XXXX1','XXXX2','KIT1    ','KIT2    ','KIT3    ',
     &     'KIT4    ','KIT5    ','KIT6    ','KIT7    ','KIT8    ',
     &     'KIT9    ', 'NVI_C   ', 'NVI_T   ', 'NVI_H   ', 'NVI_M   '/
C     ------------------------------------------------------------------
      CALL JEMARQ()

      CALL GETRES(K8B,K16BID,NOMCMD)
C                           1234567890123
      CRILOC=.FALSE.
      EXIFIB=.FALSE.
      ITDEBO=1
      TYPTGT=0

      IF (NOMCMD(1:13).NE.'THER_LINEAIRE') THEN

        COMPOR = '&&NMDORC.COMPOR'
        MODELE = MODELZ

        IF (NOMCMD(1:13).EQ.'THER_NON_LINE') THEN
          NBMO1 = 1
          MOCLEF(1) = 'COMP_THER_NL'
        ELSE IF (NOMCMD(1:9).EQ.'CALC_META') THEN
          NBMO1 = 1
          MOCLEF(1) = 'COMP_INCR'
        ELSE
          NBMO1 = 2
          IF ((NOMCMD(1:13).EQ.'STAT_NON_LINE')  .OR.
     &        (NOMCMD(1:13).EQ.'DYNA_NON_LINE')  .OR.
     &        (NOMCMD(1:15).EQ.'DYNA_TRAN_EXPLI').OR.
     &        (NOMCMD(1:6) .EQ.'CALCUL')) CRILOC=.TRUE.
          MOCLEF(1) = 'COMP_INCR'
          MOCLEF(2) = 'COMP_ELAS'
        END IF

        MOCLES(1) = 'GROUP_MA'
        MOCLES(2) = 'MAILLE'
        TYPMCL(1) = 'GROUP_MA'
        TYPMCL(2) = 'MAILLE'
        MESMAI = '&&NMDORC.MES_MAILLES'

        LIGRMO = MODELE(1:8)//'.MODELE'
        CALL JEVEUO(LIGRMO(1:19)//'.LGRF','L',JNOMA)
        NOMA = ZK8(JNOMA)

C    -----------------------------------------------------------
C     POUR LA COMMANDE CALC_G
C     SI AUCUN DES DEUX COMPORTEMENTS COMP_ELAS ET COMP_INCR N'EST
C     SPECIFIE PAR L'UTILISATEUR, ON CREE UNE CARTE PAR DEFAUT
C     AVEC LES CARACTERISTIQUES SUIVANTES :
C          COMP_ELAS ( RELATION    : ELAS
C                      DEFORMATION : PETIT
C                      TOUT        : OUI
C                    )

        IF(NOMCMD.EQ.'CALC_G') THEN
          CALL GETFAC('COMP_INCR',N2)
          CALL GETFAC('COMP_ELAS',N3)
          IF ((N2.EQ.0) .AND. (N3.EQ.0)) THEN
            VALCMP(1) = 'ELAS'
            VALCMP(2) = '1'
            VALCMP(3) = 'PETIT'
            VALCMP(4) = 'COMP_ELAS'
            VALCMP(5) = '??'
            VALCMP(6) = '0'
            VALCMP(7) = '0'
            CALL MECACT('V',COMPOR,'MAILLA',NOMA,NOMGRD,NCMPMA,NOMCMP,
     &                  IBID,RBID,CBID,VALCMP)
            GO TO 170
          END IF
        END IF

C    -----------------------------------------------------------
C    ON VERIFIE SI UNE MAILLE SUR LAQUELLE ON ESSAIE D'AFFECTER
C    UN COMPORTEMENT NE DISPOSE PAS DEJA D'UN COMPORTEMENT

        CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMAT,K8B,IRET)
        CALL WKVECT ( '&&VERI_MAILLE', 'V V K16', NBMAT, JMAIL )
        BUG = .FALSE.
        CHAIN1 = ' MAILLE:     COMPORTEMENT:        RELATION:'
        CHAIN2 = ' '
        CHAIN2(13:28) = MOCLEF(1)
        DO 80 I = 1,NBMO1
          CALL GETFAC(MOCLEF(I),NBOCC)
          DO 70 K = 1,NBOCC
            NBMA = 0
            NIVO = .FALSE.
            CALL GETVTX(MOCLEF(I),'RELATION',K,1,1,COMP,N1)
            CALL GETVTX(MOCLEF(I),'TOUT',K,1,1,K8B,N1)
            IF (N1.NE.0) THEN
              IF (I.EQ.1) THEN
                DO 10 IMA = 1,NBMAT
                  ZK16(JMAIL+IMA-1) = COMP
   10           CONTINUE
              ELSE
                DO 20 IMA = 1,NBMAT
                  IF (ZK16(JMAIL+IMA-1).NE.' ') THEN
                    CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',IMA),K8B)
                    CHAIN2(2:9) = K8B
                    CHAIN2(32:47) = ZK16(JMAIL+IMA-1)
                    IF (.NOT.BUG) THEN
                      CALL U2MESS('E','ALGORITH_25')
                    END IF
                    IF (.NOT.NIVO) THEN
                      NIVO = .TRUE.
                      VALK(1)=MOCLEF(I)
                      VALK(2)=COMP
                      VALK(3)=CHAIN1
                      VALK(4)=CHAIN2
                      CALL U2MESK ('E', 'ALGORITH11_85', 4, VALK)
                    END IF
                    BUG = .TRUE.
                  END IF
   20           CONTINUE
              END IF
            ELSE
              CALL RELIEM(MODELE,NOMA,'NU_MAILLE',MOCLEF(I),K,2,MOCLES,
     &                    TYPMCL,MESMAI,NBMA)
              IF (NBMA.NE.0) THEN
                CALL JEVEUO(MESMAI,'L',JMESM)
                IF (I.EQ.1) THEN
                  DO 30 IM = 1,NBMA
                    IMA = ZI(JMESM+IM-1)
                    ZK16(JMAIL+IMA-1) = COMP
   30             CONTINUE
                ELSE
                  DO 40 IM = 1,NBMA
                    IMA = ZI(JMESM+IM-1)
                    IF (ZK16(JMAIL+IMA-1).NE.' ') THEN
                      CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',IMA),K8B)
                      CHAIN2(2:9) = K8B
                      CHAIN2(32:47) = ZK16(JMAIL+IMA-1)
                      IF (.NOT.BUG) THEN
                          CALL U2MESS('E','ALGORITH_25')
                      END IF
                      IF (.NOT.NIVO) THEN
                        NIVO = .TRUE.
                      VALK(1)=MOCLEF(I)
                      VALK(2)=COMP
                      VALK(3)=CHAIN1
                      VALK(4)=CHAIN2
                      CALL U2MESK ('E', 'ALGORITH11_85', 4, VALK)
                      END IF
                      BUG = .TRUE.
                    END IF
   40             CONTINUE
                END IF
                CALL JEDETR(MESMAI)
              ELSE
                IF (I.EQ.1) THEN
                  DO 50 IMA = 1,NBMAT
                    ZK16(JMAIL+IMA-1) = COMP
   50             CONTINUE
                ELSE
                  DO 60 IMA = 1,NBMAT
                    IF (ZK16(JMAIL+IMA-1).NE.' ') THEN
                      CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',IMA),K8B)
                      CHAIN2(2:9) = K8B
                      CHAIN2(32:47) = ZK16(JMAIL+IMA-1)
                      IF (.NOT.BUG) THEN
                         CALL U2MESS('E','ALGORITH_25')
                      END IF
                      IF (.NOT.NIVO) THEN
                        NIVO = .TRUE.
                      VALK(1)=MOCLEF(I)
                      VALK(2)=COMP
                      VALK(3)=CHAIN1
                      VALK(4)=CHAIN2
                      CALL U2MESK ('E', 'ALGORITH11_85', 4, VALK)
                      END IF
                      BUG = .TRUE.
                    END IF
   60             CONTINUE
                END IF
              END IF
            END IF
   70     CONTINUE
   80   CONTINUE
        IF (BUG) THEN
          CALL U2MESS('F','MODELISA4_1')
        END IF
        CALL ALCART('V',COMPOR,NOMA,NOMGRD)
        CALL JEVEUO(COMPOR//'.NCMP','E',JNCMP)
        CALL JEVEUO(COMPOR//'.VALV','E',JVALV)


        DO 90 ICMP = 1,NCMPMA
          ZK8(JNCMP+ICMP-1) = NOMCMP(ICMP)
   90   CONTINUE

        IF (CRILOC) THEN
C CARTE DES CRITERES DE CONVERGENCES LOCAUX
          CALL ALCART('V',CARCRI,NOMA,'CARCRI')
          CALL JEVEUO(CARCRI(1:19)//'.NCMP','E',JCRIT)
          CALL JEVEUO(CARCRI(1:19)//'.VALV','E',JVALC)

          CALL JENONU(JEXNOM('&CATA.GD.NOMGD' ,'CARCRI'),NUMGD)
          CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'L',JACMP)
          CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'LONMAX',
     &                NBCRIT,K1BID)

          DO 95 ICMP = 1,NBCRIT
            ZK8(JCRIT+ICMP-1) = ZK8(JACMP+ICMP-1)
   95     CONTINUE

        ENDIF
C     ------------------------------------------------------------------
C                       REMPLISSAGE DES CARTES :
C     ------------------------------------------------------------------

        DO 160 I = 1,NBMO1

          CALL GETFAC(MOCLEF(I),NBOCC)

          DO 150 K = 1,NBOCC

            ZK16(JVALV-1+6) = ' '
            ZK16(JVALV-1+7) = ' '
            NBVARI = 0
            TXCP = 'ANALYTIQUE      '
            DEFO = ' '
            DO 100 ICOMEL = 1,DIMAKI
              NBVEL(ICOMEL) = 0
  100       CONTINUE

            IF(CRILOC.AND.I.EQ.1)THEN
               CALL GETVR8(MOCLEF(I),'PARM_THETA',K,1,1,THETA,IRET)
            ELSE
               THETA=1.D0
            ENDIF

            CALL GETVTX(MOCLEF(I),'RELATION',K,1,1,COMP,N1)
            IF (COMP(1:4).EQ.'KIT_') THEN

C  POUR COMPORTEMENT KIT_

              CALL GETVTX(MOCLEF(I),'RELATION_KIT',K,1,DIMAKI,COMEL(1),
     &                    N1)
              IF (N1.EQ.0) THEN
                CALL U2MESS('F','ALGORITH7_56')
              ELSE IF (N1.GT.DIMAKI) THEN
                CALL U2MESS('F','ALGORITH7_57')
              ELSE
                NCOMEL = N1
                DO 110 II = NCOMEL + 1,DIMAKI
                  COMEL(II) = '        '
  110           CONTINUE
              END IF
              DO 120 ICOMEL = 1,NCOMEL
                EXIST = GETEXM(MOCLEF(I),COMEL(ICOMEL))
                IF (EXIST) THEN
                  CALL GETVIS(MOCLEF(I),COMEL(ICOMEL),K,1,1,
     &                        NBVEL(ICOMEL),N1)
                  NBVARI = NBVARI + NBVEL(ICOMEL)
                ENDIF
  120         CONTINUE

              IF ((COMP(1:5).EQ.'KIT_H').OR.
     +            (COMP(1:6).EQ.'KIT_TH')) THEN
                DO 122 INV = 1, DIMANV
                   NBNVI(INV) = 0
 122            CONTINUE
                CALL NMTHMC(COMP,MODELE,MOCLEF(I),K,COMEL(1),NCOMEL,
     &          NBNVI(1))

              ELSEIF((COMP(1:7).EQ.'KIT_DDI') .AND. NCOMEL .GE. 2) THEN
                IF(COMEL(1)(1:4) .EQ. 'GLRC') NBVARI = NBVARI + 10
              END IF

            ELSE IF (COMP(1:4).EQ.'META') THEN
              EXIST = GETEXM(MOCLEF(I),COMP)
              IF (EXIST) THEN
                CALL GETVIS(MOCLEF(I),COMP,K,1,1,NBVARI,N1)
                CALL GETVTX(MOCLEF(I),'RELATION_KIT',K,1,DIMAKI,
     &                      COMEL(1),N1)
                IF (N1.EQ.0) THEN
                  CALL U2MESS('F','ALGORITH7_56')
                ELSE IF (N1.GT.1) THEN
                  CALL U2MESS('F','ALGORITH7_59')
                ELSE
                  NCOMEL = N1
                  DO 130 II = NCOMEL + 1,DIMAKI
                    COMEL(II) = '        '
  130             CONTINUE
                END IF
                EXIST = GETEXM(MOCLEF(I),COMEL(1))
                IF (EXIST) THEN
                  CALL GETVIS(MOCLEF(I),COMEL(1),K,1,1,NBVEL(1),N1)
                  NBVARI = NBVARI*NBVEL(1) + NBVARI + 1
                END IF
              END IF

            ELSEIF (COMP(1:8).EQ.'MONOCRIS') THEN
                CALL GETVID(MOCLEF(I),'COMPOR',K,1,1,SDCOMP,N1)
                CALL JEVEUO(SDCOMP//'.CPRI','L',ICPRI)
                CALL ASSERT(ZI(ICPRI).EQ.1)
                NBSYST=ZI(ICPRI-1+5)
                NBVARI=ZI(ICPRI-1+3)
                ZK16(JVALV-1+6) = SDCOMP//'.CPRK'
                WRITE (ZK16(JVALV-1+7),'(I16)') NBSYST

            ELSEIF (COMP(1:8).EQ.'POLYCRIS') THEN
                CALL GETVID(MOCLEF(I),'COMPOR',K,1,1,SDCOMP,N1)
                CALL JEVEUO(SDCOMP//'.CPRI','L',ICPRI)
                CALL ASSERT(ZI(ICPRI).EQ.2)
                NBMONO=ZI(ICPRI-1+2)
                NBVARI=ZI(ICPRI-1+3)
                ZK16(JVALV-1+6) = SDCOMP
                WRITE (ZK16(JVALV-1+7),'(I16)') NBMONO
CCC MULTIFIBRE

            ELSEIF (COMP(1:8).EQ.'MULTIFIB') THEN
                 EXIFIB=.TRUE.


            ELSE
C   AUTRES COMPORTEMENTS : NOMBRE DE VARIABLES INTERNES
                EXIST = GETEXM(MOCLEF(I),COMP)
                IF (EXIST) CALL GETVIS(MOCLEF(I),COMP,K,1,1,NBVARI,N1)
                IF (COMP(1:4).EQ.'ZMAT') THEN
                  CALL GETVIS(MOCLEF(I),'NB_VARI',K,1,1,NBVARI,N1)
                  CALL GETVIS(MOCLEF(I),'UNITE',K,1,1,NUNIT,N1)
                  WRITE (ZK16(JVALV-1+6),'(I16)') NUNIT
                ENDIF
            END IF

C RELATION SIMO_MIEHE POUR VMIS_ISOT_XXX ET META_XXX_IL
C ET META_XXX_INL

            EXIST = GETEXM(MOCLEF(I),'DEFORMATION')
            IF (EXIST) THEN
              CALL GETVTX(MOCLEF(I),'DEFORMATION',K,1,1,DEFO,N1)
              IF (DEFO.EQ.'SIMO_MIEHE') THEN
                IF (COMP(1:9).EQ.'META_P_IL' .OR.
     &              COMP(1:10).EQ.'META_P_INL' .OR.
     &              COMP(1:9).EQ.'META_V_IL' .OR.
     &              COMP(1:10).EQ.'META_V_INL') THEN
                  NBVARI = NBVARI + 1
                END IF
                IF ((COMP(1:9).EQ.'VMIS_ISOT').OR.
     &                  COMP(1:9).EQ.'VISC_ISOT') THEN
                  CALL GETVIS(MOCLEF(I),COMP,K,1,1,NBVARI,N1)
                  NBVARI = NBVARI + 6
                END IF
              END IF
            END IF

C           CPLAN DEBORST  ET COMP1D DEBORST
C           DEBORST SEULEMENT EN COMP_INCR
            IF ((I.EQ.1) .AND. (CRILOC)) THEN
              EXICP = GETEXM(MOCLEF(I),'ALGO_C_PLAN')
              EXI1D = GETEXM(MOCLEF(I),'ALGO_1D')
              IF (EXICP) THEN
                 CALL GETVTX(MOCLEF(I),'ALGO_C_PLAN',K,1,1,TXCP,N1)
                 IF (TXCP.EQ.'DEBORST') NBVARI = NBVARI + 4
              END IF
              IF (EXI1D) THEN
                 CALL GETVTX(MOCLEF(I),'ALGO_1D',K,1,1,TX1D,N1)
                 IF (TX1D.EQ.'DEBORST') THEN
                    IF(TXCP.EQ.'DEBORST')THEN
                        CALL U2MESS('F','ALGORITH7_58')
                    ELSE
                       NBVARI = NBVARI + 4
                       TXCP=TX1D
                    ENDIF
                 ENDIF
              END IF
            END IF

            EXIST = GETEXM(MOCLEF(I),'DEFORMATION')
            IF (EXIST) THEN
              CALL GETVTX(MOCLEF(I),'DEFORMATION',K,1,1,DEFO,N1)
            END IF

C ======================================================================
C --- ON STOCKE LE NOMBRE DE VARIABLES INTERNES PAR RELATION -----------
C --- DE COMPORTEMENT --------------------------------------------------
C ======================================================================

            ZK16(JVALV-1+1) = COMP
            WRITE (ZK16(JVALV-1+2),'(I16)') NBVARI
            ZK16(JVALV-1+3) = DEFO
            ZK16(JVALV-1+4) = MOCLEF(I)
            ZK16(JVALV-1+5) = TXCP

C  POUR COMPORTEMENT KIT_

            DO 140 ICOMEL = 1,DIMAKI
              IF ((COMP(1:4).EQ.'KIT_') .OR. (COMP(1:4).EQ.'META')) THEN
                ZK16(JVALV-1+ICOMEL+7) = COMEL(ICOMEL)
              ELSE
                ZK16(JVALV-1+ICOMEL+7) = '        '
              END IF
  140       CONTINUE
            IF ((COMP(1:5).EQ.'KIT_H').OR.(COMP(1:6).EQ.'KIT_TH')) THEN
               DO 180 INV = 1, DIMANV
                  WRITE (ZK16(JVALV-1+7+DIMAKI+INV),'(I16)') NBNVI(INV)
 180           CONTINUE
            ENDIF

C  LECTURE DES PARAMETRES DE CONVERGENCE A STOCKER DANS CARCRI

            IF (CRILOC) THEN
               CALL GETVTX(MOCLEF(I),'RESO_INTE',K,1,1,RESO,IRET)
               CALL GETVR8(MOCLEF(I),'RESI_INTE_RELA',K,1,1,RESI,IRET)
               CALL GETVIS(MOCLEF(I),'ITER_INTE_MAXI',K,1,1,ITEINT,
     &                   IRET)
               IF (RESI.NE.R8VIDE()  .AND. RESI.GT.1.0001D-6)
     &           CALL U2MESS('A','ALGORITH7_60')
               ITEPAS = 0
               CALL GETVIS('COMP_INCR','ITER_INTE_PAS' ,1,1,1,ITEPAS,
     &                      IRET)
               IF(RESO(1:9) .EQ.'IMPLICITE')     IMPEXP = 0
               IF(RESO(1:13).EQ.'RUNGE_KUTTA_2') IMPEXP = 1
               IF(RESO(1:13).EQ.'RUNGE_KUTTA_4') IMPEXP = 2

C              CPLAN DEBORST  ET COMP1D DEBORST SEULEMENT EN COMP_INCR
               RESID=1.D-6
               IF (I.EQ.1) THEN
                   CALL GETVTX(MOCLEF(I),'ALGO_C_PLAN',K,1,1,TXCP,N1)
                      IF (TXCP.EQ.'DEBORST') THEN
                         CALL GETVIS(MOCLEF(I),'ITER_MAXI_DEBORST',
     &                           K,1,1,ITDEBO,IRET)
                         CALL GETVR8(MOCLEF(I),'RESI_DEBORST',K,1,1,
     &                           RESID,IRET)
                   ENDIF
C                  dans ZR(JVALC+1) on stocke le type de matrice tgte
                   CALL GETVTX(MOCLEF(I),'TYPE_MATR_TANG',K,1,1,
     &                         TYPMAT ,IRET)
                   IF (IRET.EQ.0) THEN
                      TYPTGT = 0
                   ELSE
                      IF (TYPMAT.EQ.'PERTURBATION') THEN
                         TYPTGT = 1
                      ELSEIF (TYPMAT.EQ.'VERIFICATION') THEN
                         TYPTGT = 2
                      ENDIF
                      CALL GETVR8(MOCLEF(I),'VALE_PERT_RELA',K,1,1,
     &                            PERT,IRET)
                   ENDIF
               ENDIF
            ENDIF

C  STOCKAGE DE LA CARTE CARCRI

            CALL RELIEM(MODELE,NOMA,'NU_MAILLE',MOCLEF(I),K,2,MOCLES,
     &                  TYPMCL,MESMAI,NBMA)
            IF (NBMA.NE.0) THEN
              CALL JEVEUO(MESMAI,'L',JMA)
              CALL NOCART(COMPOR,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',
     &                    NCMPMA)
              IF (CRILOC) THEN
                ZR(JVALC)   = ITEINT
                ZR(JVALC+1) = TYPTGT
                ZR(JVALC+2) = RESI
                ZR(JVALC+3) = THETA
                ZR(JVALC+4) = ITEPAS
                ZR(JVALC+5) = IMPEXP
                ZR(JVALC+6) = PERT
                ZR(JVALC+7) = RESID
                ZR(JVALC+8) = ITDEBO
                CALL NOCART(CARCRI,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',
     &                    NBCRIT)
              ENDIF
              CALL JEDETR(MESMAI)

            ELSE

C ------- PAR DEFAUT C'EST TOUT='OUI'
              CALL NOCART(COMPOR,1,K8B,K8B,0,K8B,IBID,K8B,NCMPMA)
              IF (CRILOC) THEN
                ZR(JVALC)   = ITEINT
                ZR(JVALC+1) = TYPTGT
                ZR(JVALC+2) = RESI
                ZR(JVALC+3) = THETA
                ZR(JVALC+4) = ITEPAS
                ZR(JVALC+5) = IMPEXP
                ZR(JVALC+6) = PERT
                ZR(JVALC+7) = RESID
                ZR(JVALC+8) = ITDEBO
                CALL NOCART(CARCRI,1,K8B,K8B,0,K8B,IBID,K8B,NBCRIT)
              ENDIF

            ENDIF

  150     CONTINUE

  160   CONTINUE
  170   CONTINUE

        CALL JEDETR(COMPOR//'.NCMP')
        CALL JEDETR(COMPOR//'.VALV')
        IF (CRILOC) THEN
          CALL JEDETR(CARCRI(1:19)//'.NCMP')
          CALL JEDETR(CARCRI(1:19)//'.VALV')
        ENDIF

C SI PRESENCE DE MULTIFIBRE, ON FUSIONNE LES CARTES
C   S'IL EXISTE UNE FOIS MULTIFIBRE LA CARTE A ETE CREEE DANS RCCOMP
C   DE AFFE_MATERIAU / AFFE_COMPOR

          IF(EXIFIB)THEN
C ON RECUPERE LA CARTE ET ON EN FAIT UN CHAM_ELEM_S
            CALL GETVID(' ','CHAM_MATER',1,1,1,CHMAT,N1)
            CHS(1)='&&NMDORC.CHS1'
            CHS(2)='&&NMDORC.CHS2'
            CHS3='&&NMDORC.CHS3'
            CALL CARCES(COMPOR,'ELEM',' ','V',CHS(1),IBID)
            CALL CARCES(CHMAT//'.COMPOR','ELEM',' ','V',CHS(2),IBID)
            CALL DETRSD('CARTE',COMPOR)
            CALL CESFUS(2,CHS,LCUMU,LCOER,LCOEC,LCOC,'V',CHS3)
            CALL CESCAR(CHS3,COMPOR,'V')
            CALL DETRSD('CHAM_ELEM_S',CHS(1))
            CALL DETRSD('CHAM_ELEM_S',CHS(2))
            CALL DETRSD('CHAM_ELEM_S',CHS3)
C REMARQUE : ON GARDE LA CARTE CHMAT//'.COMPOR' CAR ELLE SERA UTILE POUR
C            LE CALCUL DES OPTIONS (ELLE RESTE LIEE A CHMAT)
        ENDIF

        COMPOZ = COMPOR

      END IF
C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
