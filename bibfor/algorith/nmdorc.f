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
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE <IMPLIED WARRANTY OF
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
C MODIF ALGORITH  DATE 30/06/2008   AUTEUR PROIX J-M.PROIX 
C     SAISIE ET VERIFICATION DE LA RELATION DE COMPORTEMENT UTILISEE
C
C IN  MODELZ  : NOM DU MODELE
C OUT COMPOZ  : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
C OUT CARCRI  : CARTE DECRIVANT LES CRITERES LOCAUX DE CONVERGENCE
C                     0 : ITER_INTE_MAXI
C                     1 : COMPOSANTE INUTILISEE
C                     2 : RESI_INTE_RELA
C                     3 : THETA 
C                     4 : ITER_INTE_PAS
C                     5 : RESO_INTE (0: EULER_1, 1: RK_2, 2: RK_4)
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C ----------------------------------------------------------------------
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      INTEGER NCMPMA,DIMAKI,N2,N3,IBID,NBOCC,I,ICMP,II,JMA,JNCMP
      INTEGER JNOMA,JVALV,K,N1,NBMA,NBMO1,NBVARI,NBVARM,NBVARZ
      INTEGER NBMAT,NBKIT,IMA,IM,IRET,ICPRI,NBSYST,IRETT
      INTEGER INV,DIMANV,NBMONO,NUMGD,NUMLC,NUNIT,NUMLC2
C    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
      PARAMETER (DIMAKI=9)
C    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
      PARAMETER (DIMANV=4)
      INTEGER NBNVI(DIMANV),NCOMEL,NVMETA
      PARAMETER (NCMPMA=7+DIMAKI+DIMANV)
      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='NMDORC')
      CHARACTER*8  NOMA,NOMCMP(NCMPMA),NOMGRD,K8B,TYPMCL(2),SDCOMP
      CHARACTER*16 COMP,DEFO,MOCLEF(2),K16BID,NOMCMD,MOCLES(2)
      CHARACTER*16 LCOMEL(5),COMCOD,TEXTE(2)
      CHARACTER*16 TXCP,TX1D,TYMATG,NOMKIT(DIMAKI)
      CHARACTER*19 COMPOR,CHS(2),CHS3
      CHARACTER*24 LIGRMO,MODELE,MESMAI
      LOGICAL EXIST,GETEXM,EXICP,EXI1D,CRILOC,EXIPMF
      
      DATA NOMCMP/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',
     &     'C_PLAN  ','XXXX1','XXXX2','KIT1    ','KIT2    ','KIT3    ',
     &     'KIT4    ','KIT5    ','KIT6    ','KIT7    ','KIT8    ',
     &     'KIT9    ', 'NVI_C   ', 'NVI_T   ', 'NVI_H   ', 'NVI_M   '/
      DATA NOMGRD/'COMPOR  '/
C     ------------------------------------------------------------------

      CALL JEMARQ()

C     initialisations
      CRILOC=.FALSE.
      MODELE = MODELZ
      EXIPMF=.FALSE.

      CALL GETRES(K8B,K16BID,NOMCMD)

      COMPOR = '&&'//NOMPRO//'.COMPOR'
      IF (NOMCMD(1:13).EQ.'THER_NON_LINE') THEN
        NBMO1 = 1
        MOCLEF(1) = 'COMP_THER_NL'
      ELSE IF (NOMCMD(1:9).EQ.'LIRE_RESU') THEN
        NBMO1 = 1
        MOCLEF(1) = 'COMP_INCR'
      ELSE
        NBMO1 = 2
        IF ((NOMCMD(1:13).EQ.'STAT_NON_LINE')  .OR.
     &      (NOMCMD(1:13).EQ.'DYNA_NON_LINE')  .OR.
     &      (NOMCMD(1:6) .EQ.'CALCUL')) CRILOC=.TRUE.
        MOCLEF(1) = 'COMP_INCR'
        MOCLEF(2) = 'COMP_ELAS'
      END IF

      MOCLES(1) = 'GROUP_MA'
      MOCLES(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&'//NOMPRO//'.MES_MAILLES'

      LIGRMO = MODELE(1:8)//'.MODELE'
      CALL JEVEUO(LIGRMO(1:19)//'.LGRF','L',JNOMA)
      NOMA = ZK8(JNOMA)

C     SI COMP_ELAS ET COMP_INCR SONT ABSENTS, ON CREE UNE CARTE ELAS
C     SUR TOUT LE MAILLAGE. UTILISE EN PARTICULIER PAR CALC_G
      CALL CRCMEL(NBMO1,MOCLEF,COMPOR,NOMA,NCMPMA,NOMCMP,IRET)
      IF (IRET.NE.0) GOTO 170

C ======================================================================
C                       REMPLISSAGE DE LA CARTE COMPOR :
C --- ON STOCKE LE NOMBRE DE VARIABLES INTERNES PAR COMPORTEMENT 
C ======================================================================

      CALL ALCART('V',COMPOR,NOMA,NOMGRD)
      CALL JEVEUO(COMPOR//'.NCMP','E',JNCMP)
      CALL JEVEUO(COMPOR//'.VALV','E',JVALV)
      DO 90 ICMP = 1,NCMPMA
        ZK8(JNCMP+ICMP-1) = NOMCMP(ICMP)
   90 CONTINUE

C     MOTS CLES FACTEUR
      DO 160 I = 1,NBMO1
        CALL GETFAC(MOCLEF(I),NBOCC)

C       NOMBRE D'OCCURRENCES
        DO 150 K = 1,NBOCC

          CALL GETVTX(MOCLEF(I),'RELATION',K,1,1,COMP,N1)
          NCOMEL=1
          LCOMEL(NCOMEL)=COMP

C         POUR COMPORTEMENTS KIT_
          CALL NMDOKI(MOCLEF(I),MODELE,COMP,K,DIMAKI,NBKIT,NOMKIT,
     &                NBNVI,NCOMEL,LCOMEL,NUMLC,NVMETA)

C         SAISIE ET VERIFICATION DU TYPE DE DEFORMATION UTILISEE
          CALL NMDOGD(MOCLEF(I),COMP,K,NCOMEL,LCOMEL,DEFO)
                
C         SAISIE ET VERIFICATION DE DEBORST
          CALL NMDOCP(MOCLEF(I),COMP,K,NCOMEL,LCOMEL,TXCP)
          
C         APPEL A LCINFO POUR RECUPERER LE NOMBRE DE VARIABLES INTERNES
          CALL LCCREE(NCOMEL, LCOMEL, COMCOD)
          CALL LCINFO(COMCOD, NUMLC, NBVARI)
          
C         CAS PARTICULIER DE META A INTEGRER DANS cata_comportement.py
          IF (COMP(1:4).EQ.'META') THEN
             IF (DEFO.EQ.'SIMO_MIEHE') NVMETA=NVMETA+1
             NBVARI=NVMETA
          ENDIF
          
C         Verif que DEFO est possible pour COMP
          CALL LCTEST(COMCOD,'DEFORMATION',DEFO,IRETT)
          IF (IRETT.EQ.0) THEN
             TEXTE(1)=DEFO
             TEXTE(2)=COMP
             CALL U2MESG('F','COMPOR1_44',2,TEXTE,0,0,0,0.D0)
          ENDIF

C ======================================================================
C         CAS PARTICULIERS
          TYMATG=' '
          EXIST = GETEXM(MOCLEF(I),'TYPE_MATR_TANG')
          IF (EXIST) THEN
             CALL GETVTX(MOCLEF(I),'TYPE_MATR_TANG',K,1,1,TYMATG,N1)
             IF (N1.GT.0) THEN
                IF (TYMATG.EQ.'TANGENTE_SECANTE') NBVARI=NBVARI+1
             ENDIF
          ENDIF          
C         CAS PARTICULIER DE MONOCRISTAL  
          IF (COMP(1:8).EQ.'MONOCRIS') THEN
              CALL GETVID(MOCLEF(I),'COMPOR',K,1,1,SDCOMP,N1)
              CALL JEVEUO(SDCOMP//'.CPRI','L',ICPRI)
              NBVARI=ZI(ICPRI-1+3)
              ZK16(JVALV-1+7) = SDCOMP
              IF (TXCP.EQ.'DEBORST') NBVARI=NBVARI+4
          ELSEIF (COMP(1:8).EQ.'POLYCRIS') THEN
              CALL GETVID(MOCLEF(I),'COMPOR',K,1,1,SDCOMP,N1)
              CALL JEVEUO(SDCOMP//'.CPRI','L',ICPRI)
              NBVARI=ZI(ICPRI-1+3)
              ZK16(JVALV-1+7) = SDCOMP
              IF (TXCP.EQ.'DEBORST') NBVARI=NBVARI+4
          ENDIF
          IF (COMP(1:8).EQ.'MULTIFIB') EXIPMF=.TRUE.
          IF (COMP(1:4).EQ.'ZMAT') THEN
             CALL GETVIS(MOCLEF,'NB_VARI',K,1,1,NBVARZ,N1)
             NBVARI=NBVARZ+NBVARI
             CALL GETVIS(MOCLEF,'UNITE',K,1,1,NUNIT,N1)
             WRITE (ZK16(JVALV-1+7),'(I16)') NUNIT
          ENDIF
C         POUR COMPORTEMENT KIT_
          DO 140 II = 1,DIMAKI
            IF ((COMP(1:4).EQ.'KIT_') .OR. (COMP(1:4).EQ.'META')) THEN
              ZK16(JVALV-1+II+7) = NOMKIT(II)
            ELSE
              ZK16(JVALV-1+II+7) = '        '
            END IF
  140     CONTINUE
          IF ((COMP(1:5).EQ.'KIT_H').OR.(COMP(1:6).EQ.'KIT_TH')) THEN
             DO 180 INV = 1, DIMANV
                WRITE (ZK16(JVALV-1+7+DIMAKI+INV),'(I16)') NBNVI(INV)
 180         CONTINUE
          ENDIF
          IF (COMP.EQ.'KIT_DDI') THEN
              IF ((NOMKIT(1)(1:4).EQ.'GLRC').OR.
     &            (NOMKIT(2)(1:4).EQ.'GLRC')) THEN
                  NBVARI=NBVARI+10
              ENDIF
          END IF
C         FIN DES CAS PARTICULIERS
C ======================================================================

C         REMPLISSAGE DES CMP

          ZK16(JVALV-1+1) = COMP
          WRITE (ZK16(JVALV-1+2),'(I16)') NBVARI
          ZK16(JVALV-1+3) = DEFO
          ZK16(JVALV-1+4) = MOCLEF(I)
          ZK16(JVALV-1+5) = TXCP
C         ON ECRIT NUMLC EN POSITION 6 (CMP XXX1)
          IF (COMP(1:8).NE.'MULTIFIB') THEN
             WRITE (ZK16(JVALV-1+6),'(I16)') NUMLC
          ENDIF
          
          CALL RELIEM(MODELE,NOMA,'NU_MAILLE',MOCLEF(I),K,2,MOCLES,
     &                TYPMCL,MESMAI,NBMA)
     
          IF (NBMA.NE.0) THEN
            CALL JEVEUO(MESMAI,'L',JMA)
            CALL NOCART(COMPOR,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',NCMPMA)
            CALL JEDETR(MESMAI)
          ELSE
C -----   PAR DEFAUT C'EST TOUT='OUI'
            CALL NOCART(COMPOR,1,K8B,K8B,0,K8B,IBID,K8B,NCMPMA)
          END IF

C ======================================================================
C         CARTE DE CRITERES LOCAUX
          IF (CRILOC) THEN
            CALL NMDOCR(CARCRI,MODELE,NOMA,MOCLEF(I),K,COMP,COMCOD,
     &                TXCP,DEFO,TYMATG,IRET)
          ENDIF
C ======================================================================
  150   CONTINUE
  160 CONTINUE
  170 CONTINUE

C ======================================================================

      CALL JEDETR(COMPOR//'.NCMP')
      CALL JEDETR(COMPOR//'.VALV')
      
C ======================================================================
C     SI MULTIFIBRE, ON FUSIONNE AVEC LA CARTE CREEE DANS AFFE_MATERIAU
C      / AFFE_COMPOR - RCCOMP.F
      IF (EXIPMF) THEN
         CALL NMDPMF(COMPOR)
      ENDIF
C ======================================================================

      COMPOZ = COMPOR
      CALL JEDEMA()
      END
