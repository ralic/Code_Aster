      SUBROUTINE OPS022 ( ICMD , ICOND , IER )
      IMPLICIT   NONE
      INTEGER             ICMD , ICOND , IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 27/05/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C                   MACR_GOUJ2E_MAIL
C
C     ON UTILISE LES UNITES :
C     POUR GIBI :
C         70 : POUR ECRIRE LES DONNEES GIBI DE LA PROCEDURE
C
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
C
      INTEGER       LNA, INUMEX, N1, UNITD, INFO, IERUSR, LLOGIE,
     &              LFORMA, LFICHI, NF, VERSIO, UNITS, UNITEF, I8,
     &              NFIL, IBID, NFM, IADFM, UNITP, L, LRM
      INTEGER       IADRIB, IADREG, IADHFI, NGIEXE, NGIIMP
      REAL*8        HCSBRI, REBRI, RIBRI, PAFIL,
     &              REGOU, HHTBRI, HBSBRI, VALR1
      LOGICAL       IMPR, FIMA
      CHARACTER*8   NOMGEO, VARIAN, NOMRES, NOMASS, K8B1, K8B2, K8B3,
     &              DONGIB, NOPAR1, NOPAR2, PROGIB
      CHARACTER*16  TYPRES, NOMCMD, STATUT, FORMAT, FICHIE, TOBJ
      CHARACTER*128 REP, LOGIEL

C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
      IF ( ICOND .NE. -1 ) GOTO 9999
C     ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES, TYPRES, NOMCMD )
C
C     --- L'EXECUTABLE ---
C
      CALL GETFAC ( 'EXEC_MAILLAGE', N1 )
C
      CALL GETVTX ( 'EXEC_MAILLAGE', 'LOGICIEL'  , 1,1,1, LOGIEL, N1)
      CALL GETVIS ( 'EXEC_MAILLAGE', 'UNITE_DATG', 1,1,1, UNITD, N1)
      CALL GETVIS ( 'EXEC_MAILLAGE', 'UNITE_MGIB', 1,1,1, UNITS, N1)
      CALL GETVIS ( 'EXEC_MAILLAGE', 'NIVE_GIBI' , 1,1,1, NGIEXE, N1)
C
C     --- CONSTRUCTION DU NOM DE l'ASSEMBLAGE
C
      CALL GETVTX('DEFI_GOUJ','TYPE',1,1,1,NOMGEO,L)
      CALL GETLTX('DEFI_GOUJ','TYPE',1,1,1,LNA,L)
      CALL GETVTX('DEFI_GOUJ','VARIANTE',1,1,1,VARIAN,L)
      NOMASS = NOMGEO(1:LNA)//VARIAN(1:1)
C
C     --- GEOMETRIE DE L'ASSEMBLAGE ---
C
      CALL GETVIS ( 'GEOM_GOUJ_BRID', 'NB_FILET'  , 1,1,1, NFIL, N1 )
      CALL GETVR8 ( 'GEOM_GOUJ_BRID', 'H_CORP_BRID' , 1,1,1,
     &               HCSBRI, N1 )
      CALL GETVR8 ( 'GEOM_GOUJ_BRID', 'R_EXT_BRID' , 1,1,1, REBRI, N1 )
C
C     --- RECUPERATION DU RAYON INTERNE DE LA BRIDE ---
      CALL GETTCO ( 'RIB'//NOMASS, TOBJ )
      IF ( TOBJ .EQ. 'R8              ' ) THEN
        CALL JEVEUO ( 'RIB'//NOMASS, 'L', IADRIB )
        RIBRI = ZR(IADRIB)
      ELSE
        CALL UTMESS ('F', 'OPS022_F0', 'LE CONCEPT R8 RIB'//NOMASS
     &              //' N''EST PAS DEFINI.')
      ENDIF
C
C     --- RECUPERATION DU RAYON EXTERIEUR DU GOUJON ---
      CALL GETTCO ( 'REG'//NOMASS, TOBJ )
      IF ( TOBJ .EQ. 'R8              ' ) THEN
        CALL JEVEUO ( 'REG'//NOMASS, 'L', IADREG )
        REGOU = ZR(IADREG)
      ELSE
        CALL UTMESS ('F', 'OPS022_F1', 'LE CONCEPT R8 REG'//NOMASS
     &              //' N''EST PAS DEFINI.')
      ENDIF
C
C     --- RECUPERATION DE LA HAUTEUR OU DU PAS DU FILET ---
      CALL GETTCO ( 'HF'//NOMASS, TOBJ )
      IF ( TOBJ .EQ. 'R8              ' ) THEN
        CALL JEVEUO ( 'HF'//NOMASS, 'L', IADHFI )
        PAFIL = ZR(IADHFI)
      ELSE
        CALL UTMESS ('F', 'OPS022_F2', 'LE CONCEPT R8 HF'//NOMASS
     &              //' N''EST PAS DEFINI.')
      ENDIF
C
      CALL GETVR8 ( 'GEOM_GOUJ_BRID', 'H_HAUT_BRID' , 1,1,1,
     &               HHTBRI , N1 )
      CALL GETVR8 ( 'GEOM_GOUJ_BRID', 'H_BAS_BRID' , 1,1,1,
     &              HBSBRI , N1 )
C
C     --- FILETS MANQUANTS OU ABSENTS ---
C
      FIMA = .FALSE.
      CALL GETVIS ( 'GEOM_GOUJ_BRID', 'FILET_ABST', 1,1,0,
     &               IBID, NFM )
      IF ( NFM .LT. 0 ) THEN
        NFM = -NFM
        FIMA = .TRUE.
        CALL WKVECT ( '&&OPS022.NUM_FIL', 'V V I', NFM, IADFM )
        CALL GETVIS ( 'GEOM_GOUJ_BRID', 'FILET_ABST' , 1,1,NFM,
     &                ZI(IADFM) , N1 )
        IF ( NFM .GT. 0 ) THEN
          CALL ORDIS ( ZI(IADFM), NFM )
        ENDIF
      ENDIF
C
      VALR1 = NFIL*PAFIL
      IF ( VALR1 .GT. HCSBRI ) THEN
        CALL UTMESS ('F', 'OPS022_F3', 'LA HAUTEUR DE BRIDE EST '
     &               //'INFERIEURE AU NOMBRE DE FILETS MULTIPLIE '
     &               //'PAR LE PAS.')
      ENDIF
C
      IF ( RIBRI .GE. REBRI ) THEN
        CALL UTMESS ('F', 'OPS022_F4', 'LE RAYON INTERIEUR DE LA BRIDE'
     &               //' EST SUPERIEUR A SON RAYON EXTERIEUR.')
      ENDIF
C
C     --- VERIFICATION DE LA VALEUR DU RAYON EXTERNE DU GOUJON ---
C
      IF ( REGOU .GE. RIBRI ) THEN
        CALL UTMESS ('F', 'OPS022_F5', 'LE RAYON EXTERIEUR DU GOUJON'
     &               //' EST SUPERIEUR AU RAYON INTERIEUR DE LA BRIDE.')
      ENDIF
C
C     --- IMPRESSION ---
C
      IMPR = .FALSE.
      CALL GETFAC ( 'IMPRESSION', N1 )
      IF ( N1 .NE. 0 ) THEN
         IMPR = .TRUE.
         CALL GETVTX ( 'IMPRESSION', 'FORMAT'   , 1,1,1, FORMAT, N1 )
         CALL GETLTX ( 'IMPRESSION', 'FORMAT'   , 1,1,1, LFORMA, N1 )
         CALL GETVIS ( 'IMPRESSION', 'VERSION'  , 1,1,1, VERSIO, N1 )
         CALL GETVIS ( 'IMPRESSION', 'NIVE_GIBI', 1,1,1, NGIIMP, N1 )
         CALL GETVTX ( 'IMPRESSION', 'FICHIER'  , 1,1,1, FICHIE, NF )
         CALL GETLTX ( 'IMPRESSION', 'FICHIER'  , 1,1,1, LFICHI, N1 )
         CALL GETVIS ( 'IMPRESSION', 'UNITE'    , 1,1,1, UNITEF, N1 )
      ENDIF
C
C     --- INFO ---
C
      CALL GETVIS ( ' ', 'INFO' , 1,1,1, INFO  , N1 )
C
C     ------------------------------------------------------------------
C
C     --- DESTRUCTION DE LA COMMANDE COURANTE --
      IERUSR = 0
      CALL SMCDEL(ICMD,0,IERUSR)
      ICMD   = ICMD - 1
C
C     ---------------------------------------------------------------
C
      CALL REPOUT ( 1, LRM, REP )
      LOGIEL = REP(1:LRM)//'gibi'
      LLOGIE = LRM+4
C
C     --- ECRITURE SUR LE FICHIER .datg  DE LA PROCEDURE ---
C
      UNITP = 71
      PROGIB = 'provgibi'
      CALL ULDEFI( UNITP , PROGIB , 'A', 'N', 'O')
C
      DONGIB = 'donngib'
      CALL ULDEFI( UNITD , DONGIB , 'A', 'N', 'O')
C
C     --- PROCEDURE DE MAILLAGE ---
C
      CALL GOUJP1 (UNITD, NGIEXE)
      CALL GOUJDO (UNITD, NFIL, HCSBRI, REBRI, RIBRI, REGOU, PAFIL,
     &             HBSBRI, HHTBRI)
      CALL GOUJP2 (UNITD, UNITP, NFM, ZI(IADFM), FIMA)
C
      CLOSE (UNIT = UNITP)
      CLOSE (UNIT = UNITD)
C
C     --- COMMANDE EXEC_LOGICIEL ---
C
      CALL CODENT ( UNITD, 'G', K8B1 )
      NOPAR1 = 'fort.'//K8B1
      CALL CODENT ( UNITS, 'G', K8B1 )
      NOPAR2 = 'fort.'//K8B1
      I8 = 8
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, ' ', 'EXEC_LOGICIEL', IERUSR )
        CALL PUTVTX ( 'LOGICIEL', 1, LOGIEL, LLOGIE, IERUSR)
        CALL SMDMCF ( 'ARGUMENT', IERUSR )
          CALL PUTVTX ( 'NOM_PARA', 1, NOPAR1, I8, IERUSR)
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'ARGUMENT', IERUSR )
          CALL PUTVTX ( 'NOM_PARA', 1, NOPAR2, I8, IERUSR)
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  PRE_GIBI  ---
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, ' ', 'PRE_GIBI', IERUSR )
        CALL PUTVIS ( 'UNITE_GIBI', 1, UNITS, IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  LIRE_MAILLAGE  ---
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, NOMRES, 'LIRE_MAILLAGE', IERUSR )
        CALL PUTVIS ( 'INFO' , 1, INFO , IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  DEFI_GROUP  ---
C
      K8B2 = 'NDFILETS'
      K8B3 = 'CORPSGOU'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//NOMRES, 'DEFI_GROUP', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, NOMRES, IERUSR )
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'NOM', 1, K8B2, IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B3, IERUSR )
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  DEFI_FICHIER, IMPR_RESU  ---
C
      IF ( IMPR ) THEN
C
         IF ( NF .NE. 0 ) THEN
            ICMD = ICMD + 1
            CALL SMDCMD ( ICMD, ' ', 'DEFI_FICHIER', IERUSR )
            CALL PUTVTX ( 'FICHIER'   , 1, FICHIE, LFICHI, IERUSR)
            CALL PUTVIS ( 'UNITE' , 1, UNITEF, IERUSR )
            CALL SMFCMD ( IERUSR )
         ENDIF
C
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_RESU', IERUSR )
           CALL SMDMCF ( 'RESU', IERUSR )
             CALL PUTVID ( 'MAILLAGE', 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'FORMAT'  , 1, FORMAT, LFORMA, IERUSR)
             IF (FORMAT(1:5).EQ.'IDEAS') THEN
                CALL PUTVIS ( 'VERSION' , 1, VERSIO, IERUSR )
             ELSEIF (FORMAT(1:6).EQ.'CASTEM') THEN
                CALL PUTVIS ( 'NIVE_GIBI' , 1, NGIIMP, IERUSR )
             ENDIF
             IF ( NF .NE. 0 ) THEN
               CALL PUTVTX ( 'FICHIER'  , 1, FICHIE, LFICHI, IERUSR)
             ENDIF
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
C
C     ---------------------------------------------------------------
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
      CALL JEDETC('V','&&OPS022',1)
      CALL JEDEMA()
      END
