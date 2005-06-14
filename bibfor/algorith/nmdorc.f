      SUBROUTINE NMDORC(MODELZ,COMPOZ)
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
      IMPLICIT NONE
      CHARACTER*(*) MODELZ,COMPOZ
C ----------------------------------------------------------------------
C MODIF ALGORITH  DATE 04/04/2005   AUTEUR MCOURTOI M.COURTOIS 
C     SAISIE ET VERIFICATION DE LA RELATION DE COMPORTEMENT UTILISEE
C
C IN  MODELZ  : NOM DU MODELE
C OUT COMPOZ  : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
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
      INTEGER JNOMA,JVALV,K,N1,NBAP,NBET,NBMA,NBMO1,NBVARI,NC1,NC2
      INTEGER NBMAT,JMAIL,NCOMEL,NS1,JMESM,IMA,IM,IRET,ICPRI,NBSYST
      INTEGER INV,DIMANV,NBMONO,NUNIT
      REAL*8 RBID
      COMPLEX*16 CBID
      LOGICAL      BUG, NIVO
C    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
      PARAMETER (DIMAKI=9)
C    DIMAKI = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
      PARAMETER (DIMANV=4)
      PARAMETER (NCMPMA=7+DIMAKI+DIMANV)
      LOGICAL EXIST,GETEXM,EXICP,EXI1D
      CHARACTER*8 NOMA,NOMGRD,NOMCMP(NCMPMA),K8B,TYPMCL(2),SDCOMP
      CHARACTER*16 COMP,DEFO,MOCLEF(2),K16BID,NOMCMD,MOCLES(2)
      CHARACTER*16 VALCMP(NCMPMA),TXCP,TX1D
      CHARACTER*19 COMPOR
      CHARACTER*24 LIGRMO,MODELE,MESMAI
      CHARACTER*50 CHAIN1, CHAIN2
C    POUR COMPORTEMENT KIT_
      INTEGER NBVEL(DIMAKI)
      INTEGER NBNVI(DIMANV)
      CHARACTER*16 COMEL(DIMAKI)

      DATA NOMGRD/'COMPOR  '/
      DATA NOMCMP/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',
     &     'C_PLAN  ','XXXX1','XXXX2','KIT1    ','KIT2    ','KIT3    ',
     &     'KIT4    ','KIT5    ','KIT6    ','KIT7    ','KIT8    ',
     &     'KIT9    ', 'NVI_C   ', 'NVI_T   ', 'NVI_H   ', 'NVI_M   '/
C     ------------------------------------------------------------------
      CALL JEMARQ()

      CALL GETRES(K8B,K16BID,NOMCMD)
C                           1234567890123
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
          MOCLEF(1) = 'COMP_INCR'
          MOCLEF(2) = 'COMP_ELAS'
        END IF

        MOCLES(1) = 'GROUP_MA'
        MOCLES(2) = 'MAILLE'
        TYPMCL(1) = 'GROUP_MA'
        TYPMCL(2) = 'MAILLE'
        MESMAI = '&&NMDORC.MES_MAILLES'

        LIGRMO = MODELE(1:8)//'.MODELE'
        CALL JEVEUO(LIGRMO(1:19)//'.NOMA','L',JNOMA)
        NOMA = ZK8(JNOMA)

C    -----------------------------------------------------------
C     POUR LES COMMANDES CALC_G_THETA_T ET CALC_G_LOCAL_T, SI AUCUN DES
C     DEUX COMPORTEMENTS COMP_ELAS ET COMP_INCR N'EST SPECIFIE PAR
C     L'UTILISATEUR, ON CREE UNE CARTE PAR DEFAUT AVEC LES
C     CARACTERISTIQUES SUIVANTES :
C          COMP_ELAS ( RELATION    : ELAS
C                      DEFORMATION : PETIT
C                      TOUT        : OUI
C                    )

        IF ((NOMCMD.EQ.'CALC_G_THETA_T') .OR.
     &      (NOMCMD.EQ.'CALC_G_LOCAL_T')) THEN
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
        NBAP = 0
        NBET = 0
        BUG = .FALSE.
        CHAIN1 = ' MAILLE:     COMPORTEMENT:        RELATION:'
        CHAIN2 = ' '
        CHAIN2(13:28) = MOCLEF(1)
        DO 80 I = 1,NBMO1
          CALL GETFAC(MOCLEF(I),NBOCC)
          NBAP = NBAP + NBOCC
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
                      CALL UTDEBM('E','NMDORC','DONNEES INCOMPATIBLES:')
                    END IF
                    IF (.NOT.NIVO) THEN
                      NIVO = .TRUE.
                      CALL UTIMPK('L',
     &                            ' ON VEUT AFFECTER UN COMPORTEMENT ',
     &                            1,MOCLEF(I))
                      CALL UTIMPK('S',' AVEC UNE RELATION ',1,COMP)
                      CALL UTIMPK('S',' SUR UNE MAILLE DEJA AFFECTEE '
     &                              //'PAR UN AUTRE COMPORTEMENT',
     &                            0,K8B)
                      CALL UTIMPK('L',CHAIN1,0,K8B)
                    END IF
                    CALL UTIMPK('L',CHAIN2,0,K8B)
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
                        CALL UTDEBM('E','NMDORC',
     &                              'DONNEES INCOMPATIBLES:')
                      END IF
                      IF (.NOT.NIVO) THEN
                        NIVO = .TRUE.
                        CALL UTIMPK('L',
     &                              ' ON VEUT AFFECTER UN COMPORTEMENT '
     &                              ,1,MOCLEF(I))
                        CALL UTIMPK('S',' AVEC UNE RELATION ',1,COMP)
                        CALL UTIMPK('S',' SUR UNE MAILLE DEJA AFFECTEE '
     &                              //'PAR UN AUTRE COMPORTEMENT',
     &                            0,K8B)
                        CALL UTIMPK('L',CHAIN1,0,K8B)
                      END IF
                      CALL UTIMPK('L',CHAIN2,0,K8B)
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
                        CALL UTDEBM('E','NMDORC',
     &                              'DONNEES INCOMPATIBLES:')
                      END IF
                      IF (.NOT.NIVO) THEN
                        NIVO = .TRUE.
                        CALL UTIMPK('L',
     &                              ' ON VEUT AFFECTER UN COMPORTEMENT '
     &                              ,1,MOCLEF(I))
                        CALL UTIMPK('S',' AVEC UNE RELATION ',1,COMP)
                        CALL UTIMPK('S',' SUR UNE MAILLE DEJA AFFECTEE '
     &                              //'PAR UN AUTRE COMPORTEMENT',
     &                            0,K8B)
                        CALL UTIMPK('L',CHAIN1,0,K8B)
                      END IF
                      CALL UTIMPK('L',CHAIN2,0,K8B)
                      BUG = .TRUE.
                    END IF
   60             CONTINUE
                END IF
              END IF
            END IF
            NBET = NBET + NBMA
   70     CONTINUE
   80   CONTINUE
        IF (BUG) THEN
          CALL UTFINM()
          CALL UTMESS('F','NMDORC','ARRET SUR ERREURS')
        END IF

        CALL ALCART('V',COMPOR,NOMA,NOMGRD,NBAP+1,NBET)

        CALL JEVEUO(COMPOR//'.NCMP','E',JNCMP)
        CALL JEVEUO(COMPOR//'.VALV','E',JVALV)

        DO 90 ICMP = 1,NCMPMA
          ZK8(JNCMP+ICMP-1) = NOMCMP(ICMP)
   90   CONTINUE

C     ------------------------------------------------------------------
C                       REMPLISSAGE DE LA CARTE :
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

            CALL GETVTX(MOCLEF(I),'RELATION',K,1,1,COMP,N1)
            IF (COMP(1:4).EQ.'KIT_') THEN

C  POUR COMPORTEMENT KIT_

              CALL GETVTX(MOCLEF(I),'RELATION_KIT',K,1,DIMAKI,COMEL(1),
     &                    N1)
              IF (N1.EQ.0) THEN
                CALL UTMESS('F','NMDORC','LISTE RELATION_KIT VIDE')
              ELSE IF (N1.GT.DIMAKI) THEN
                CALL UTMESS('F','NMDORC',
     &                      'LISTE RELATION_KIT TROP LONGUE')
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
                      CALL UTMESS('F','NMDORC','1D OU C_PLAN ?')
                   ELSE
                      NBVARI = NBVARI + 4
                      TXCP=TX1D
                   ENDIF
                ENDIF
              END IF
              
              IF ((COMP(1:6).EQ.'KIT_HM').OR.(COMP(1:7).EQ.'KIT_HHM').
     &         OR.(COMP(1:6).EQ.'KIT_TH')) THEN
                DO 122 INV = 1, DIMANV
                   NBNVI(INV) = 0
 122            CONTINUE
                CALL NMTHMC(COMP,MODELE,MOCLEF(I),K,COMEL(1),NCOMEL,
     &          NBNVI(1))
              END IF
            ELSE IF (COMP(1:4).EQ.'META') THEN
              EXIST = GETEXM(MOCLEF(I),COMP)
              IF (EXIST) THEN
                CALL GETVIS(MOCLEF(I),COMP,K,1,1,NBVARI,N1)
                CALL GETVTX(MOCLEF(I),'RELATION_KIT',K,1,DIMAKI,
     &                      COMEL(1),N1)
                IF (N1.EQ.0) THEN
                  CALL UTMESS('F','NMDORC','LISTE RELATION_KIT VIDE')
                ELSE IF (N1.GT.1) THEN
                  CALL UTMESS('F','NMDORC','LISTE RELATION_KIT TROP')
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
              
            ELSEIF (COMP(1:4).EQ.'ZMAT') THEN
                CALL GETVIS(MOCLEF(I),'NB_VARI',K,1,1,NBVARI,N1)
                CALL GETVIS(MOCLEF(I),'UNITE',K,1,1,NUNIT,N1)
                WRITE (ZK16(JVALV-1+6),'(I16)') NUNIT

            ELSE
              EXIST = GETEXM(MOCLEF(I),COMP)
              IF (EXIST) THEN
                CALL GETVIS(MOCLEF(I),COMP,K,1,1,NBVARI,N1)

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
                        CALL UTMESS('F','NMDORC','1D OU C_PLAN ?')
                    ELSE
                       NBVARI = NBVARI + 4
                       TXCP=TX1D
                    ENDIF
                 ENDIF
               END IF
              
              END IF
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
                IF (COMP(1:9).EQ.'VMIS_ISOT') THEN
                  CALL GETVIS(MOCLEF(I),COMP,K,1,1,NBVARI,N1)
                  NBVARI = NBVARI + 1
                END IF
              END IF
            END IF


            EXIST = GETEXM(MOCLEF(I),'DEFORMATION')
            IF (EXIST) THEN
              CALL GETVTX(MOCLEF(I),'DEFORMATION',K,1,1,DEFO,N1)
            END IF

            ZK16(JVALV-1+1) = COMP
            WRITE (ZK16(JVALV-1+2),'(I16)') NBVARI
            ZK16(JVALV-1+3) = DEFO
            ZK16(JVALV-1+4) = MOCLEF(I)
            ZK16(JVALV-1+5) = TXCP
CCC            ZK16(JVALV-1+6) = ' '
CCC            ZK16(JVALV-1+7) = ' '

C  POUR COMPORTEMENT KIT_

            DO 140 ICOMEL = 1,DIMAKI
              IF ((COMP(1:4).EQ.'KIT_') .OR. (COMP(1:4).EQ.'META')) THEN
                ZK16(JVALV-1+ICOMEL+7) = COMEL(ICOMEL)
              ELSE
                ZK16(JVALV-1+ICOMEL+7) = '        '
              END IF
  140       CONTINUE

C ======================================================================
C --- ON STOCKE LE NOMBRE DE VARIABLES INTERNES PAR RELATION -----------
C --- DE COMPORTEMENT --------------------------------------------------
C ======================================================================
            IF ((COMP(1:6).EQ.'KIT_HM').OR.(COMP(1:7).EQ.'KIT_HHM').
     &         OR.(COMP(1:6).EQ.'KIT_TH')) THEN
               DO 180 INV = 1, DIMANV
                  WRITE (ZK16(JVALV-1+7+DIMAKI+INV),'(I16)') NBNVI(INV)
 180           CONTINUE
            ENDIF

            CALL RELIEM(MODELE,NOMA,'NU_MAILLE',MOCLEF(I),K,2,MOCLES,
     &                  TYPMCL,MESMAI,NBMA)
            IF (NBMA.NE.0) THEN
              CALL JEVEUO(MESMAI,'L',JMA)
              CALL NOCART(COMPOR,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',
     &                    NCMPMA)
              CALL JEDETR(MESMAI)
            ELSE
C ------- PAR DEFAUT C'EST TOUT='OUI'
C            CALL GETVTX ( MOCLEF(I), 'TOUT'  , K,1,1, OUI   , NT )
              CALL NOCART(COMPOR,1,K8B,K8B,0,K8B,IBID,K8B,NCMPMA)
            END IF

  150     CONTINUE

  160   CONTINUE
  170   CONTINUE

        CALL JEDETR(COMPOR//'.NCMP')
        CALL JEDETR(COMPOR//'.VALV')

        COMPOZ = COMPOR

      END IF
C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
