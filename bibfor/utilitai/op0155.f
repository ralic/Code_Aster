      SUBROUTINE OP0155 ( IER )
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 23/08/2004   AUTEUR CIBHHLV L.VIVAN 
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
C     OPERATEUR   IMPR_TABLE
C     ------------------------------------------------------------------
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      INTEGER             IER
C
C 0.2. ==> COMMUNS
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'OP0155' )
C
      INTEGER IFM, NIV
      INTEGER      IFR, IUNIFI, N1, NPARFI, NBTRI, NPARIM, NPARPG
      INTEGER      JPAIM, JPAPG, NTI
      INTEGER IAUX, JAUX, IBID, IRET
      INTEGER NRPASS, NBPASS
      INTEGER ADRECG
      LOGICAL      ULEXIS
C
      CHARACTER*3  IMPRFO
      CHARACTER*8  FORMAR, FORMAC
      CHARACTER*8  NOPASE
      CHARACTER*16 FICHIE, FORMAT
      CHARACTER*19 TABLE1, NEWTAB, NEWTA1, NEWTA2
      CHARACTER*24 PARIM, PARPG
      CHARACTER*24 NORECG
      CHARACTER*80 TITRE
C     ------------------------------------------------------------------
C
C====
C 1. PREALABLES
C====
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
C
      IER = 0
C               12   345678   9012345678901234
      NORECG = '&&'//NOMPRO//'_PARA_SENSI     '
C
C====
C  2. RECUPERATION DES OPERANDES
C====  
C                        --- LA TABLE ---
C
C     ------------------------------------------------------------------
      CALL GETVID ( ' ', 'TABLE' , 1,1,1, TABLE1 , N1 )
      IF ( NIV.GE.2 ) THEN
        CALL UTMESS('I',NOMPRO,'IMPRESSION DE LA TABLE '//TABLE1)
      ENDIF
C     ------------------------------------------------------------------
C                 --- SENSIBILITE : NOMBRE DE PASSAGES ---
C     ------------------------------------------------------------------
      IAUX = 1
      JAUX = 1
      CALL PSRESE(' ',IBID,IAUX,TABLE1,JAUX,NBPASS,NORECG,IRET)
      CALL JEVEUO(NORECG,'L',ADRECG)
C
C     ------------------------------------------------------------------
C
C                    --- LE FICHIER D'IMPRESSION ---
C
C     ------------------------------------------------------------------
C
      IFR    = 0
      FICHIE = ' '
      CALL GETVIS ( ' ', 'UNITE'  , 1,1,1, IFR   , N1 )
      IF ( .NOT. ULEXIS( IFR ) ) THEN
         CALL ULOPEN ( IFR, ' ', FICHIE, 'NEW', 'O' )
      ENDIF
      IF (IFR.EQ.25) THEN
         CALL UTMESS('A',NOMPRO,'ATTENTION, LES VALEURS SERONT'//
     +             ' ECRITES SUR LE FICHIER DE TYPE DIGR DANS ASTERIX')
      ENDIF
      WRITE(IFR,1000)
C
      CALL GETVTX(' ','TITRE_TABLE',1,1,1,TITRE,NTI)
      IF (NTI.NE.0) WRITE(IFR,'(''TITRE:'',/A80)') TITRE
C     ------------------------------------------------------------------
C
C                 --- TRAITEMENT DU MOT CLE "FILTRE" ---
C
C     ------------------------------------------------------------------
      CALL GETFAC ( 'FILTRE' , NPARFI )
C     ------------------------------------------------------------------
C 
C                 --- TRAITEMENT DU MOT CLE "TRI" ---
C
C     ------------------------------------------------------------------
      CALL GETFAC ( 'TRI' , NBTRI )
C     ------------------------------------------------------------------
C
C                   --- LES FORMATS D'IMPRESSION ---
C
C     ------------------------------------------------------------------
      CALL GETVTX ( ' ', 'FORMAT_R' , 1,1,1, FORMAR, N1 )
C
      CALL GETVTX ( ' ', 'FORMAT_C' , 1,1,1, FORMAC, N1 )
C
      CALL GETVTX ( ' ', 'FORMAT'   , 1,1,1, FORMAT, N1 )
C     ------------------------------------------------------------------
C
C                  --- IMPRESSION DES FONCTIONS ---
C
C     ------------------------------------------------------------------
      CALL GETVTX (' ','IMPR_FONCTION', 1,1,1, IMPRFO, N1 )
C     ------------------------------------------------------------------
      
C====
C 3. IMPRESSIONS
C====
C
      DO 30 , NRPASS = 1 , NBPASS
C
C 3.1. ==> QUELLE STRUCTURE FAUT-IL IMPRIMER ?
C        POUR LE PASSAGE NUMERO NRPASS :
C        . NEWTAB = NOM DE LA TABLE A IMPRIMER
C        . NOPASE = NOM DU PARAMETRE DE SENSIBILITE (BLANC SI STANDARD)
C
        NEWTAB = ZK24(ADRECG+2*NRPASS-2)(1:8)
        NOPASE = ZK24(ADRECG+2*NRPASS-1)(1:8)
C
        IF ( NIV.GE.2 ) THEN
          IF ( NOPASE.NE.'        ' ) THEN
            CALL UTMESS('I',NOMPRO,'SENSIBILITE AU PARAMETRE '//NOPASE)
          ENDIF
        ENDIF
C
C 3.2. ==> LES PARAMETRES ---
C
        PARIM = '&&'//NOMPRO//'.PARA_IMPR'
        PARPG = '&&'//NOMPRO//'.PARA_PAGI'
        CALL TBIMPA ( NEWTAB, PARIM, NPARIM, PARPG, NPARPG )
        CALL JEVEUO ( PARIM, 'L', JPAIM )
        CALL JEVEUO ( PARPG, 'L', JPAPG )
C
C 3.3. ==> FILTRAGE EVENTUEL
C
        IF ( NPARFI .NE. 0 ) THEN
          NEWTA1 = '&&'//NOMPRO//'.FILTRE '
          CALL TBIMFI ( NPARFI, NEWTAB, NEWTA1 )
          NEWTAB = NEWTA1
        ENDIF 
C
C 3.4. ==> TRI EVENTUEL
C
        IF ( NBTRI .NE. 0 ) THEN
          NEWTA2 = '&&'//NOMPRO//'.TRI '
          CALL TBIMTR ( NEWTAB, NEWTA2 )
          NEWTAB = NEWTA2
        ENDIF
C
C 3.5. ==> IMPRESSION DE LA TABLE
C
        CALL TBIMPR ( NEWTAB, NOPASE, FORMAT, IFR, 
     >                NPARIM, ZK24(JPAIM), 
     >                NPARPG, ZK24(JPAPG), FORMAR, FORMAC )
C
C 3.6. ==> IMPRESSION DES FONCTIONS
C
        IF ( IMPRFO .EQ. 'OUI' ) THEN
          CALL TBIMFO ( NEWTAB, IFR)
        ENDIF
C
C 3.7. ==> MENAGE EVENTUEL
C
        IF ( NPARFI.NE.0 ) THEN
          CALL DETRSD ( 'TABLE' , NEWTA1 )
        ENDIF
        IF ( NBTRI.NE.0 ) THEN
          CALL DETRSD ( 'TABLE' , NEWTA2 )
        ENDIF
C
        CALL JEDETR ( PARIM )
        CALL JEDETR ( PARPG )
C
   30 CONTINUE
C
C====
C 4. MENAGE
C====
C
 9999 CONTINUE
C
 1000 FORMAT(/,80('-'))
C
      CALL JEDETC('V','&&',1)
      CALL JEDEMA()
      END
