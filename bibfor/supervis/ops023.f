      SUBROUTINE OPS023 ( ICMD , ICOND , IER )
      IMPLICIT   NONE
      INTEGER             ICMD , ICOND , IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 26/06/2002   AUTEUR DURAND C.DURAND 
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
C                   MACR_GOUJ2E_CALC
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C12345678901234567890123456789012345678901234567890123456789012345678901
      INTEGER      LNA, INUMEX, N1, IERUSR, I8, I16, INFO, NTIT,
     &             NOCC, IBID, NF(6),
     &             IADNF(6), I, J, K, L, NPAS, LFORMA, VERSIO
      INTEGER      L1I16(2), L2I16(4)
      INTEGER      NCLIM, NINCH, NITER, NITGLO
      INTEGER      IADSGO
      REAL*8       R8PI, KTDL(2), FOAPP, R8B1, R8B2, SEGOU, REGOU
      REAL*8       TAB2(4), REGLRE, VRI
      LOGICAL      IMPR
      CHARACTER*1  CH1
      CHARACTER*3  CH3
      CHARACTER*8  NOMGEO, VARIAN, NOMASS
      CHARACTER*8  K8B(9), MAT(9), POBJ(9), K8B1, K8B2, K8B3, K8B4,
     &             K8B5, K8B6, K8B7, K8B8(2), K8B9, K8B10(4), K8B11,
     &             K8B12(4), K8B13(3), K8B01(3)
      CHARACTER*8  MAILLA, MODELE, CARAEL, AFFMAT, DEFIFO, NOMTAB
      CHARACTER*8  NEWTAB
      CHARACTER*8  DEFIRE, AFCHME, NOMGMA, MATGOU, MATBRI, TFI(6)
      CHARACTER*16 TYPRES, NOMCMD, STATUT, NOMRES, K16B1, K16B2, K16B3,
     &             K16B4, K16B5, K16B6(2), K16B7, K16B8, K16B9(2),
     &             K16B10(4), TOBJ, TCALC, FORMAT
      CHARACTER*80 MONTIT
C     ------------------------------------------------------------------
      DATA NOCC / 0 /
      DATA NF   / 0 , 0 , 0 , 0 , 0 , 0 /
      DATA TOBJ / '        ' /
      DATA POBJ / '        ' , '        ' , '        ' , '        ' ,
     &            '        ' , '        ' , '        ' , '        ' ,
     &            '        ' /
      DATA TFI  / '        ' , '        ' , '        ' , '        ' ,
     &            '        ' , '        ' /
      DATA CH3  / '   ' /
      DATA KTDL / 1.0D+07 , 1.0D+07 /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
      IF ( ICOND .NE. -1 ) GOTO 9999
C
C     ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES, TYPRES, NOMCMD )
C
C     --- RECUPERATION DU NOM DE MAILLAGE ---
C
      CALL GETVID ( ' ', 'MAILLAGE'  , 1,1,1, MAILLA, N1 )
C
C     --- RECUPERATION DES CARACTERISTIQUES DU GOUJON ---
C     ---                ET DES FILETS                ---
C
C     --- CONSTRUCTION DU NOM DE L'ASSEMBLAGE
C
      CALL GETVTX('DEFI_GOUJ','TYPE',1,1,1,NOMGEO,L)
      CALL GETLTX('DEFI_GOUJ','TYPE',1,1,1,LNA,L)
      CALL GETVTX('DEFI_GOUJ','VARIANTE',1,1,1,VARIAN,L)
      NOMASS = NOMGEO(1:LNA)//VARIAN(1:1)
C
C     --- RECUPERATION DU TYPE ET DES NUMEROS DES FILETS ---
C     ---                  PARTICULIERS                  ---
C
      CALL GETVIS ( 'DEFI_GOUJ', 'FILET_TRONQUE', 1,1,0, IBID, NF(1) )
      IF ( NF(1) .LT. 0 ) THEN
        NF(1) = -NF(1)
        CALL WKVECT ( '&&OPS023.NUM_FIL1', 'V V I', NF(1),IADNF(1) )
        CALL GETVIS ( 'DEFI_GOUJ', 'FILET_TRONQUE', 1,1,NF(1),
     &                 ZI(IADNF(1)), N1 )
        IF ( NF(1) .GT. 1 ) THEN
          CALL ORDIS ( ZI(IADNF(1)), NF(1) )
        ENDIF
        TFI(1) = 'FT'//NOMASS
        NOCC = NOCC + 1
      ENDIF
C
      CALL GETVIS ( 'DEFI_GOUJ', 'FILET_TRONQA', 1,1,0, IBID, NF(2) )
      IF ( NF(2) .LT. 0 ) THEN
        NF(2) = -NF(2)
        CALL WKVECT ( '&&OPS023.NUM_FIL2', 'V V I', NF(2),IADNF(2) )
        CALL GETVIS ( 'DEFI_GOUJ', 'FILET_TRONQA', 1,1,NF(2),
     &                 ZI(IADNF(2)), N1 )
        IF ( NF(2) .GT. 1 ) THEN
          CALL ORDIS ( ZI(IADNF(2)), NF(2) )
        ENDIF
        TFI(2) = 'FTA'//NOMASS
        NOCC = NOCC + 1
      ENDIF
C
      CALL GETVIS ( 'DEFI_GOUJ', 'FILET_TRONQB', 1,1,0, IBID, NF(3) )
      IF ( NF(3) .LT. 0 ) THEN
        NF(3) = -NF(3)
        CALL WKVECT ( '&&OPS023.NUM_FIL3', 'V V I', NF(3),IADNF(3) )
        CALL GETVIS ( 'DEFI_GOUJ', 'FILET_TRONQB', 1,1,NF(3),
     &                 ZI(IADNF(3)), N1 )
        IF ( NF(3) .GT. 1 ) THEN
          CALL ORDIS ( ZI(IADNF(3)), NF(3) )
        ENDIF
        TFI(3) = 'FTB'//NOMASS
        NOCC = NOCC + 1
      ENDIF
C
      CALL GETVIS ( 'DEFI_GOUJ', 'FILET_JEU_HT', 1,1,0, IBID, NF(4) )
      IF ( NF(4) .LT. 0 ) THEN
        NF(4) = -NF(4)
        CALL WKVECT ( '&&OPS023.NUM_FIL4', 'V V I', NF(4),IADNF(4) )
        CALL GETVIS ( 'DEFI_GOUJ', 'FILET_JEU_HT', 1,1,NF(4),
     &                 ZI(IADNF(4)), N1 )
        IF ( NF(4) .GT. 1 ) THEN
          CALL ORDIS ( ZI(IADNF(4)), NF(4) )
        ENDIF
        TFI(4) = 'JHT'//NOMASS
        NOCC = NOCC + 1
      ENDIF
C
      CALL GETVIS ( 'DEFI_GOUJ', 'FILET_JEU_HTA', 1,1,0, IBID, NF(5) )
      IF ( NF(5) .LT. 0 ) THEN
        NF(5) = -NF(5)
        CALL WKVECT ( '&&OPS023.NUM_FIL5', 'V V I', NF(5),IADNF(5) )
        CALL GETVIS ( 'DEFI_GOUJ', 'FILET_JEU_HTA', 1,1,NF(5),
     &                 ZI(IADNF(5)), N1 )
        IF ( NF(5) .GT. 1 ) THEN
          CALL ORDIS ( ZI(IADNF(5)), NF(5) )
        ENDIF
        TFI(5) = 'HTA'//NOMASS
        NOCC = NOCC + 1
      ENDIF
C
      CALL GETVIS ( 'DEFI_GOUJ', 'FILET_JEU_HTB', 1,1,0, IBID, NF(6) )
      IF ( NF(6) .LT. 0 ) THEN
        NF(6) = -NF(6)
        CALL WKVECT ( '&&OPS023.NUM_FIL6', 'V V I', NF(6),IADNF(6) )
        CALL GETVIS ( 'DEFI_GOUJ', 'FILET_JEU_HTB', 1,1,NF(6),
     &                 ZI(IADNF(6)), N1 )
        IF ( NF(6) .GT. 1 ) THEN
          CALL ORDIS ( ZI(IADNF(6)), NF(6) )
        NOCC = NOCC + 1
        ENDIF
        TFI(6) = 'HTB'//NOMASS
        NOCC = NOCC + 1
      ENDIF
C
C     --- RECUPERATION DU CHARGEMENT  MOT-CLE FACTEUR EXCIT ---
C
C     CONDITIONS AUX LIMITES
      CALL GETVIS ( 'EXCIT', 'TYPE_BLOCAGE', 1,1,1, NCLIM, N1 )
C     FORCE TOTALE APPLIQQUEE A LA STRUCTURE
      CALL GETVR8 ( 'EXCIT', 'FORCE_GOUJ', 1,1,1, FOAPP, N1 )
C
C     --- RECUPERATION DU TYPE DE CALCUL : ELASTIQUE OU ---
C     ---               ELASTOPLASTIQUE                 ---
C
      CALL GETVTX ( 'CALCUL', 'TYPE_CALCUL', 1,1,1, TCALC, N1 )
C
C     NOMBRE D'INCREMENT DE CHARGE (PAR LEQUEL LA FORCE TOTALE EST
C     DIVISEE.)
C
      CALL GETVIS ( 'CALCUL', 'NB_INCR', 1,1,1, NINCH, N1 )
C
      TAB2(1) = 0.0D0
      TAB2(2) = 0.0D0
      TAB2(3) = NINCH
      TAB2(4) = FOAPP
C
C     --- IMPRESSION ---
C
      IMPR = .FALSE.
      CALL GETFAC ( 'IMPRESSION', N1 )
      IF ( N1 .NE. 0 ) THEN
         IMPR = .TRUE.
         CALL GETVTX ( 'IMPRESSION', 'FORMAT' , 1,1,1, FORMAT, N1 )
         CALL GETLTX ( 'IMPRESSION', 'FORMAT' , 1,1,1, LFORMA, N1 )
         CALL GETVIS ( 'IMPRESSION', 'VERSION', 1,1,1, VERSIO, N1 )
      ENDIF
C
C     --- INFO ET TITRE ---
C
      CALL GETVIS ( ' ', 'INFO' , 1,1,1, INFO  , N1 )
      CALL GETVTX ( ' ', 'TITRE', 1,1,1, MONTIT, NTIT )
C
C     ------------------------------------------------------------------
C
C     --- DESTRUCTION DE LA COMMANDE COURANTE --
C
      IERUSR = 0
      CALL SMCDEL(ICMD,0,IERUSR)
      ICMD   = ICMD - 1
C
C     ------------------------------------------------------------------
C
C     --- COMMANDE DEFI_GROUP ---
C
      K8B01(1) = 'GOUJ_NO'
      K8B01(2) = 'FILET_NO'
      K8B01(3) = 'BRIDE_NO'
      K8B1 = 'GOUJON'
      K8B2 = 'FILETS'
      K8B3 = 'BRIDE'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//MAILLA, 'DEFI_GROUP', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'NOM', 1, K8B01(1), IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'NOM', 1, K8B01(2), IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B2, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'NOM', 1, K8B01(3), IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B3, IERUSR )
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE AFFE_MODELE ---
C
      I16 = 16
      K16B1 = 'MECANIQUE'
      K16B2 = 'POU_D_E'
      K16B3 = '2D_DIS_T'
      K16B4 = 'AXIS'
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , MODELE )
      CALL SMDCMD ( ICMD, MODELE, 'AFFE_MODELE', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B2, I16, IERUSR)
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B2, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B3, I16, IERUSR)
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B3, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B4, I16, IERUSR)
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE AFFE_CARA_ELEM ---
C
C     --- RECUPERATION DE LA SECTION DU GOUJON ---
      CALL GETTCO ( 'SG'//NOMASS, TOBJ )
      IF ( TOBJ .EQ. 'R8              ' ) THEN
        CALL JEVEUO ( 'SG'//NOMASS, 'L', IADSGO )
        SEGOU = ZR(IADSGO)
      ELSE
        CALL UTMESS ('F', 'OPS023_F0', 'LE CONCEPT R8 SG'//NOMASS
     &              //' N''EST PAS DEFINI.')
      ENDIF
      REGOU = SQRT(SEGOU/R8PI())
C
      I8 = 8
      K8B4 = 'CERCLE'
      K8B5 = 'R'
      K8B6 = 'K_T_D_L'
      ICMD = ICMD + 1
        CALL GCNCON ( '.' , CARAEL )
        CALL SMDCMD ( ICMD, CARAEL, 'AFFE_CARA_ELEM', IERUSR )
          CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
           CALL SMDMCF ( 'POUTRE', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
             CALL PUTVTX ( 'SECTION'    , 1, K8B4, I8, IERUSR)
             CALL PUTVTX ( 'CARA'    , 1, K8B5, I8, IERUSR)
             CALL PUTVR8 ( 'VALE'    , 1, REGOU  , IERUSR )
           CALL SMFMCF ( IERUSR )
           CALL SMDMCF ( 'DISCRET', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, K8B2, IERUSR )
             CALL PUTVTX ( 'CARA'    , 1, K8B6, I8, IERUSR)
             CALL PUTVR8 ( 'VALE'    , 2, KTDL  , IERUSR )
           CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDES DEFI_MATERIAU ---
C
C     --- DEFINITION DES MATERIAUX DES FILETS ---
C
      TOBJ = '        '
C     -- NOM DES TYPES DE FILET
      K8B(1) = 'PF'//NOMASS
      K8B(2) = 'DF'//NOMASS
      K8B(3) = 'FC'//NOMASS
      K8B(4) = 'FT'//NOMASS
      K8B(5) = 'FTA'//NOMASS
      K8B(6) = 'FTB'//NOMASS
      K8B(7) = 'JHT'//NOMASS
      K8B(8) = 'HTA'//NOMASS
      K8B(9) = 'HTB'//NOMASS
C     -- NOM DES MATERIAUX COORESPONDANT AUX TYPES DE FILET
C
      MAT(1) = 'MF_1'
      MAT(2) = 'MF_2'
      MAT(3) = 'MF_C'
      MAT(4) = 'MF_T'
      MAT(5) = 'MF_TA'
      MAT(6) = 'MF_TB'
      MAT(7) = 'MF_HT'
      MAT(8) = 'MF_HTA'
      MAT(9) = 'MF_HTB'
C
      CALL EXISD ( 'FONCTION',K8B(1), IERUSR )
      IF ( IERUSR .EQ. 1 ) THEN
         POBJ(1) = 'PF'//NOMASS
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, MAT(1), 'DEFI_MATERIAU', IERUSR )
           CALL SMDMCF ( 'TRACTION', IERUSR )
             CALL PUTVID ( 'SIGM', 1, K8B(1), IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
      CALL EXISD ( 'FONCTION',K8B(2), IERUSR )
      IF ( IERUSR .EQ. 1 ) THEN
         POBJ(2) = 'DF'//NOMASS
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, MAT(2), 'DEFI_MATERIAU', IERUSR )
           CALL SMDMCF ( 'TRACTION', IERUSR )
             CALL PUTVID ( 'SIGM', 1, K8B(2), IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
      CALL EXISD ( 'FONCTION',K8B(3), IERUSR )
      IF ( IERUSR .EQ. 1 ) THEN
         POBJ(3) = 'FC'//NOMASS
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, MAT(3), 'DEFI_MATERIAU', IERUSR )
           CALL SMDMCF ( 'TRACTION', IERUSR )
             CALL PUTVID ( 'SIGM', 1, K8B(3), IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
      CALL EXISD ( 'FONCTION',K8B(4), IERUSR )
      IF ( IERUSR .EQ. 1 ) THEN
         POBJ(4) = 'FT'//NOMASS
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, MAT(4), 'DEFI_MATERIAU', IERUSR )
           CALL SMDMCF ( 'TRACTION', IERUSR )
             CALL PUTVID ( 'SIGM', 1, K8B(4), IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
      CALL EXISD ( 'FONCTION',K8B(5), IERUSR )
      IF ( IERUSR .EQ. 1 ) THEN
         POBJ(5) = 'FTA'//NOMASS
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, MAT(5), 'DEFI_MATERIAU', IERUSR )
           CALL SMDMCF ( 'TRACTION', IERUSR )
             CALL PUTVID ( 'SIGM', 1, K8B(5), IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
      CALL EXISD ( 'FONCTION',K8B(6), IERUSR )
      IF ( IERUSR .EQ. 1 ) THEN
         POBJ(6) = 'FTB'//NOMASS
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, MAT(6), 'DEFI_MATERIAU', IERUSR )
           CALL SMDMCF ( 'TRACTION', IERUSR )
             CALL PUTVID ( 'SIGM', 1, K8B(6), IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
      CALL EXISD ( 'FONCTION',K8B(7), IERUSR )
      IF ( IERUSR .EQ. 1 ) THEN
         POBJ(7) = 'JHT'//NOMASS
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, MAT(7), 'DEFI_MATERIAU', IERUSR )
           CALL SMDMCF ( 'TRACTION', IERUSR )
             CALL PUTVID ( 'SIGM', 1, K8B(7), IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
      CALL EXISD ( 'FONCTION',K8B(8), IERUSR )
      IF ( IERUSR .EQ. 1 ) THEN
         POBJ(8) = 'HTA'//NOMASS
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, MAT(8), 'DEFI_MATERIAU', IERUSR )
           CALL SMDMCF ( 'TRACTION', IERUSR )
             CALL PUTVID ( 'SIGM', 1, K8B(8), IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
      CALL EXISD ( 'FONCTION',K8B(9), IERUSR )
      IF ( IERUSR .EQ. 1 ) THEN
         POBJ(9) = 'HTB'//NOMASS
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, MAT(9), 'DEFI_MATERIAU', IERUSR )
           CALL SMDMCF ( 'TRACTION', IERUSR )
             CALL PUTVID ( 'SIGM', 1, K8B(9), IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
C
C     --- DEFINITION DES MATERIAUX DU GOUJON ET DE LA BRIDE ---
C
      CALL GETTCO ( 'MG'//NOMASS, TOBJ )
      IF ( TOBJ .EQ. 'MATER           ' ) THEN
         MATGOU = 'MG'//NOMASS
         TOBJ = '        '
      ELSE
        CALL UTMESS ('F', 'OPS023_F1', 'LE MATERIAU DU GOUJON '
     &              //'N''EST PAS DEFINI DANS LA BASE DE DONNEES.')
      ENDIF
C
      CALL GETTCO ( 'MB'//NOMASS, TOBJ )
      IF ( TOBJ .EQ. 'MATER           ' ) THEN
         MATBRI = 'MB'//NOMASS
         TOBJ = '        '
      ELSE
        CALL UTMESS ('F', 'OPS023_F2', 'LE MATERIAU DE LA BRIDE '
     &              //'N''EST PAS DEFINI DANS LA BASE DE DONNEES.')
      ENDIF
C
C     --- COMMANDES AFFE_MATERIAU ---
C
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , AFFMAT )
      CALL SMDCMD ( ICMD, AFFMAT, 'AFFE_MATERIAU', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA',1 , K8B1, IERUSR )
          CALL PUTVID ( 'MATER',1 , MATGOU, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA',1 , K8B2, IERUSR )
          CALL PUTVID ( 'MATER',1 , MAT(3), IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA',1 , K8B3, IERUSR )
          CALL PUTVID ( 'MATER',1 , MATBRI, IERUSR )
        CALL SMFMCF ( IERUSR )
        IF ( POBJ(1) .EQ. K8B(1) ) THEN
          CALL CODENT ( 1, 'D0', CH3 )
          NOMGMA = 'FIL'//CH3
          CALL SMDMCF ( 'AFFE', IERUSR )
            CALL PUTVID ( 'GROUP_MA',1 , NOMGMA, IERUSR )
            CALL PUTVID ( 'MATER',1 , MAT(1), IERUSR )
          CALL SMFMCF ( IERUSR )
        ENDIF
        IF ( POBJ(2) .EQ. K8B(2) ) THEN
          CALL CODENT ( 2, 'D0', CH3 )
          NOMGMA = 'FIL'//CH3
          CALL SMDMCF ( 'AFFE', IERUSR )
            CALL PUTVID ( 'GROUP_MA',1 , NOMGMA, IERUSR )
            CALL PUTVID ( 'MATER',1 , MAT(2), IERUSR )
          CALL SMFMCF ( IERUSR )
        ENDIF
        IF ( NOCC .NE. 0 ) THEN
          DO 20 I = 1, 6
            DO 30 J = 1, 9
              IF ( TFI(I) .EQ. POBJ(J) ) THEN
                DO 40 K = 1, NF(I)
                  CALL CODENT ( ZI(IADNF(I)+K-1), 'D0', CH3 )
                  NOMGMA = 'FIL'//CH3
                  CALL SMDMCF ( 'AFFE', IERUSR )
                    CALL PUTVID ( 'GROUP_MA',1 , NOMGMA, IERUSR )
                    CALL PUTVID ( 'MATER',1 , MAT(J), IERUSR )
                  CALL SMFMCF ( IERUSR )
 40             CONTINUE
              ENDIF
 30         CONTINUE
 20       CONTINUE
        ENDIF
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDES AFFE_CHAR_MECA ---
C
      K8B7 = 'OUI'
      K8B8(1) = 'DBRIDE'
      K8B8(2) = 'BBRIDE'
      K8B9 = 'PHGOUJ'
      R8B1 = 0.0D0
      R8B2 = 1.0D0
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , AFCHME )
      CALL SMDCMD ( ICMD, AFCHME, 'AFFE_CHAR_MECA', IERUSR )
        CALL PUTVID ( 'MODELE', 1, MODELE, IERUSR )
        CALL SMDMCF ( 'DDL_IMPO', IERUSR )
          CALL PUTVID ( 'GROUP_NO', 3, K8B01, IERUSR )
          CALL PUTVR8 ( 'DX', 1, R8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'FACE_IMPO', IERUSR )
          IF ( NCLIM .EQ. 1 ) THEN
            CALL PUTVID ( 'GROUP_MA', 1, K8B8(1), IERUSR )
            CALL PUTVR8 ( 'DY', 1, R8B1, IERUSR )
          ELSEIF ( NCLIM .EQ. 2 ) THEN
            CALL PUTVID ( 'GROUP_MA', 2, K8B8, IERUSR )
            CALL PUTVR8 ( 'DY', 1, R8B1, IERUSR )
          ELSEIF ( NCLIM .EQ. 3 ) THEN
            CALL PUTVID ( 'GROUP_MA', 1, K8B8(2), IERUSR )
            CALL PUTVR8 ( 'DY', 1, R8B1, IERUSR )
          ELSE
            CALL UTMESS ('F', 'OPS023_F3', 'LES CONDITIONS AUX'
     &              //' LIMITES DEMANDEES NE SONT PAS PREVUES.')
          ENDIF
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'FACE_IMPO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
          CALL PUTVR8 ( 'DRY', 1, R8B1, IERUSR )
          CALL PUTVR8 ( 'DZ', 1, R8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'FORCE_NODALE', IERUSR )
          CALL PUTVID ( 'GROUP_NO', 1, K8B9, IERUSR )
          CALL PUTVR8 ( 'FY', 1, R8B2, IERUSR )
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C
C     --- COMMANDE DEFI_FONCTION POUR LE CHARGEMENT ---
C
      K8B10(1) = 'INST'
      K8B10(2) = 'EXCLU'
      K8B10(3) = 'LIN'
      K16B5 = 'CROISSANT'
      ICMD = ICMD + 1
      CALL GCNCON ( '.', DEFIFO )
      CALL SMDCMD ( ICMD, DEFIFO, 'DEFI_FONCTION', IERUSR )
        CALL PUTVTX ( 'NOM_PARA', 1, K8B10(1), I8, IERUSR )
        CALL PUTVR8 ( 'VALE', 4, TAB2, IERUSR )
        CALL PUTVTX ( 'PROL_DROITE', 1, K8B10(2), I8, IERUSR )
        CALL PUTVTX ( 'PROL_GAUCHE', 1, K8B10(2), I8, IERUSR )
        CALL PUTVTX ( 'INTERPOL', 1, K8B10(3), I8, IERUSR )
        CALL PUTVTX ( 'VERIF', 1, K16B5, I16, IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE DEFI_LIST_REEL ---
C
      NPAS = 1
      ICMD = ICMD + 1
      CALL GCNCON ( '.', DEFIRE )
      CALL SMDCMD ( ICMD, DEFIRE, 'DEFI_LIST_REEL', IERUSR )
        CALL PUTVR8 ( 'DEBUT', 1, R8B1, IERUSR )
        DO 50 I = 1, NINCH
          CALL SMDMCF ( 'INTERVALLE', IERUSR )
            VRI = I
            CALL PUTVR8 ( 'JUSQU_A', 1, VRI, IERUSR )
            CALL PUTVIS ( 'NOMBRE', 1, NPAS, IERUSR )
          CALL SMFMCF ( IERUSR )
 50     CONTINUE
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE STAT_NON_LINE ---
C
      NITER = 3
      NITGLO = 20
      REGLRE = 1.0D-6
      K8B11 = 'ELAS'
      K16B6(1) = 'DIS_GOUJ2E_ELAS'
      K16B6(2) = 'DIS_GOUJ2E_PLAS'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, NOMRES, 'STAT_NON_LINE', IERUSR )
        CALL PUTVID ( 'MODELE', 1, MODELE, IERUSR )
        CALL PUTVID ( 'CHAM_MATER', 1, AFFMAT, IERUSR )
        CALL PUTVID ( 'CARA_ELEM', 1, CARAEL, IERUSR )
        CALL SMDMCF ( 'EXCIT', IERUSR )
          CALL PUTVID ( 'CHARGE', 1, AFCHME, IERUSR )
          CALL PUTVID ( 'FONC_MULT', 1, DEFIFO, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'COMP_INCR', IERUSR )
          CALL PUTVTX ( 'RELATION', 1, K8B11, I8, IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'COMP_INCR', IERUSR )
          CALL PUTVTX ( 'RELATION', 1, K8B11, I8, IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B3, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'COMP_INCR', IERUSR )
          IF ( TCALC(1:9) .EQ. 'ELASTIQUE' ) THEN
           CALL PUTVTX ( 'RELATION', 1, K16B6(1), I16, IERUSR )
         ELSEIF ( TCALC(1:15) .EQ. 'ELASTOPLASTIQUE' ) THEN
           CALL PUTVTX ( 'RELATION', 1, K16B6(2), I16, IERUSR )
         ELSE
           CALL UTMESS ('F', 'OPS023_F4', 'LE TYPE DE CALCUL'
     &                //' DEMANDE N''EST PAS PREVU. ON PEUT FAIRE,'
     &                //' SOIT UN CALCUL ELASTIQUE, SOIT UN CALCUL'
     &                //' ELASTOPLASTIQUE.')
         ENDIF
           CALL PUTVID ( 'GROUP_MA', 1, K8B2, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'INCREMENT', IERUSR )
          CALL PUTVID ( 'LIST_INST', 1, DEFIRE, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'NEWTON', IERUSR )
          CALL PUTVIS ( 'REAC_ITER', 1, NITER, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'CONVERGENCE', IERUSR )
          CALL PUTVR8 ( 'RESI_GLOB_RELA', 1, REGLRE, IERUSR )
          CALL PUTVIS ( 'ITER_GLOB_MAXI', 1, NITGLO, IERUSR )
          CALL PUTVTX ( 'ARRET', 1, K8B7, I8, IERUSR )
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE CALC_NO ---
C
      K16B7 = 'FORC_NODA'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//NOMRES, 'CALC_NO', IERUSR )
        CALL PUTVID ( 'MODELE', 1, MODELE, IERUSR )
        CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
        CALL PUTVID ( 'CHAM_MATER', 1, AFFMAT, IERUSR )
        CALL PUTVID ( 'CARA_ELEM', 1, CARAEL, IERUSR )
        CALL PUTVID ( 'GROUP_MA', 1, K8B2, IERUSR )
        CALL SMDMCF ( 'EXCIT', IERUSR )
          CALL PUTVID ( 'CHARGE', 1, AFCHME, IERUSR )
          CALL PUTVID ( 'FONC_MULT', 1, DEFIFO, IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL PUTVTX ( 'OPTION', 1, K16B7, I16, IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- POST-TRAITEMENTS ---
C
      IF ( IMPR ) THEN
C
        IF ( FORMAT(1:5) .EQ. 'TABLE' ) THEN
          K8B12(1) = 'RESU_T1'
          K8B12(2) = 'NDFILETS'
          K8B12(3) = 'DY'
          K16B8 = 'EXTRACTION'
C     --- COMMANDE POST_RELEVE_T ---
C
          ICMD = ICMD + 1
          CALL GCNCON ( '.', NOMTAB )
          CALL SMDCMD ( ICMD, NOMTAB, 'POST_RELEVE_T', IERUSR )
            CALL SMDMCF ( 'ACTION', IERUSR )
              CALL PUTVTX ( 'INTITULE', 1, K8B12(1), I8, IERUSR )
              CALL PUTVID ( 'GROUP_NO', 1, K8B12(2), IERUSR )
              CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
              CALL PUTVTX ( 'NOM_CHAM', 1, K16B7, I16, IERUSR )
              CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B7, I8, IERUSR )
              CALL PUTVTX ( 'NOM_CMP', 1, K8B12(3), I8, IERUSR )
              CALL PUTVTX ( 'OPERATION', 1, K16B8, I16, IERUSR )
            CALL SMFMCF ( IERUSR )
          CALL SMFCMD ( IERUSR )
C     --- COMMANDE POST_GOUJ2E ---
C
          ICMD = ICMD + 1
          CALL GCNCON ( '.', NEWTAB )
          CALL SMDCMD ( ICMD, NEWTAB, 'POST_GOUJ2E', IERUSR )
            CALL PUTVID ( 'TABLE', 1, NOMTAB, IERUSR )
          CALL SMFCMD ( IERUSR )
C     --- COMMANDE IMPR_TABLE ---
C
          K8B13(1) = 'NON_VIDE'
          K8B13(2) = 'AGRAF'
          K8B13(3) = 'EQ'
          K16B9(1) = 'NUME_FILET'
          K16B9(2) = 'NOEUDS'
          K16B10(1) = 'NUME_ORDRE'
          K16B10(2) = 'NUME_FILET'
          K16B10(3) = 'REACTION'
          K16B10(4) = 'REACTION_CUMU'
          L1I16(1) = 16
          L1I16(2) = 16
          L2I16(1) = 16
          L2I16(2) = 16
          L2I16(3) = 16
          L2I16(4) = 16
          ICMD = ICMD + 1
          CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
            CALL PUTVID ( 'TABLE', 1, NEWTAB, IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 2, K16B9, L1I16, IERUSR )
            CALL SMDMCF ( 'FILTRE', IERUSR )
              CALL PUTVTX ( 'NOM_PARA', 1, K16B9(2), I16, IERUSR )
              CALL PUTVTX ( 'CRIT_COMP', 1, K8B13(1), I8, IERUSR )
            CALL SMFMCF ( IERUSR )
            CALL PUTVTX ( 'FORMAT', 1, K8B13(2), I8, IERUSR )
          CALL SMFCMD ( IERUSR )
          DO 60 I = 1, NINCH
            ICMD = ICMD + 1
            CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
              CALL PUTVID ( 'TABLE', 1, NEWTAB, IERUSR )
              CALL PUTVTX ( 'NOM_PARA', 4, K16B10, L2I16, IERUSR )
              CALL SMDMCF ( 'FILTRE', IERUSR )
                CALL PUTVTX ( 'NOM_PARA', 1, K16B10(1), I16, IERUSR )
                CALL PUTVTX ( 'CRIT_COMP', 1, K8B13(3), I8, IERUSR )
                CALL PUTVIS ( 'VALE_I' , 1, I, IERUSR )
              CALL SMFMCF ( IERUSR )
              CALL PUTVTX ( 'FORMAT', 1, K8B13(2), I8, IERUSR )
            CALL SMFCMD ( IERUSR )
 60       CONTINUE
C
        ELSEIF ( FORMAT(1:8) .EQ. 'RESULTAT' .OR.
     &           FORMAT(1:5) .EQ. 'IDEAS' .OR.
     &           FORMAT(1:5) .EQ. 'ASTER' .OR.
     &           FORMAT(1:6) .EQ. 'CASTEM' .OR.
     &           FORMAT(1:7) .EQ. 'ENSIGHT' .OR.
     &           FORMAT(1:3) .EQ. 'MED' ) THEN
C     --- COMMANDE IMPR_RESU ---
C
          ICMD = ICMD + 1
          CALL SMDCMD ( ICMD, ' ', 'IMPR_RESU', IERUSR )
            CALL PUTVID ( 'MODELE'    , 1, MODELE, IERUSR )
            CALL SMDMCF ( 'RESU', IERUSR )
              CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
              CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
              CALL PUTVTX ( 'FORMAT'  , 1, FORMAT, LFORMA, IERUSR)
              CALL PUTVIS ( 'VERSION' , 1, VERSIO, IERUSR )
            CALL SMFMCF ( IERUSR )
          CALL SMFCMD ( IERUSR )
        ELSE
          CALL UTMESS ( 'F', 'OPS023_F6', 'OPTION '
     &                //'D''IMPRESSION NON PREVUE.' )
        ENDIF
      ENDIF
C
C     ---------------------------------------------------------------
C
C
 9998 CONTINUE
      IF ( IERUSR .GT. 0 ) THEN
         CALL UTMESS('E',NOMCMD,'ERREURS CONSTATEES DANS LA MACRO')
         IER = IER + IERUSR
      ENDIF
C
 9999 CONTINUE
C
C     --"MENAGE"
C     -----------
      CALL JEDETC('V','&&OPS023',1)
      CALL JEDEMA()
      END
