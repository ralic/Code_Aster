      SUBROUTINE OP0155 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 24/03/2003   AUTEUR MCOURTOI M.COURTOIS 
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
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IFR, IUNIFI, N1, NPARFI, NBTRI, NPARIM, NPARPG,
     +             JPAIM, JPAPG, NTI
      CHARACTER*3  IMPRFO
      CHARACTER*8  FORMAR, FORMAC
      CHARACTER*16 FICHIE, FORMAT
      CHARACTER*19 TABLE, NEWTAB, NEWTA1, NEWTA2
      CHARACTER*24 PARIM, PARPG
      CHARACTER*80 TITRE
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
C     ------------------------------------------------------------------
C
C                    --- LE FICHIER D'IMPRESSION ---
C
C     ------------------------------------------------------------------
      CALL GETVTX ( ' ', 'FICHIER', 1,1,1, FICHIE, N1 )
      IFR = IUNIFI(FICHIE)
      IF ( IFR .EQ. 0 ) THEN 
         CALL UTMESS('A','IMPR_TABLE','DESOLE, PAS D''IMPRESSION CAR'//
     +               ' MAUVAISE DEFINITION DU FICHIER D''IMPRESSION.')
         GOTO 9999
      ELSEIF (IFR.EQ.25) THEN
         CALL UTMESS('A','IMPR_TABLE','ATTENTION, LES VALEURS SERONT'//
     +             ' ECRITES SUR LE FICHIER DE TYPE DIGR DANS ASTERIX')
      ENDIF
      WRITE(IFR,1000)
C
      CALL GETVTX(' ','TITRE_TABLE',1,1,1,TITRE,NTI)
      IF (NTI.NE.0) WRITE(IFR,'(''TITRE:'',/A80)') TITRE
C     ------------------------------------------------------------------
C  
C                        --- LA TABLE ---
C
C     ------------------------------------------------------------------
      CALL GETVID ( ' ', 'TABLE' , 1,1,1, TABLE , N1 )
      NEWTAB = TABLE
C     ------------------------------------------------------------------
C
C                 --- TRAITEMENT DU MOT CLE "FILTRE" ---
C
C     ------------------------------------------------------------------
      CALL GETFAC ( 'FILTRE' , NPARFI )
      IF ( NPARFI .NE. 0 ) THEN
         NEWTA1 = '&&OP0155.FILTRE '
         CALL TBIMFI ( NPARFI, NEWTAB, NEWTA1 )
         NEWTAB = NEWTA1
      ENDIF 
C     ------------------------------------------------------------------
C 
C                 --- TRAITEMENT DU MOT CLE "TRI" ---
C
C     ------------------------------------------------------------------
      CALL GETFAC ( 'TRI' , NBTRI )
      IF ( NBTRI .NE. 0 ) THEN
         NEWTA2 = '&&OP0155.TRI '
         CALL TBIMTR ( NEWTAB, NEWTA2 )
         NEWTAB = NEWTA2
      ENDIF
C     ------------------------------------------------------------------
C
C                        --- LES PARAMETRES ---
C
C     ------------------------------------------------------------------
      PARIM = '&&OP0155.PARA_IMPR'
      PARPG = '&&OP0155.PARA_PAGI'
      CALL TBIMPA ( NEWTAB, PARIM, NPARIM, PARPG, NPARPG )
      CALL JEVEUO ( PARIM, 'L', JPAIM )
      CALL JEVEUO ( PARPG, 'L', JPAPG )
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
C                    --- IMPRESSION DE LA TABLE ---
C
C     ------------------------------------------------------------------
      CALL TBIMPR ( NEWTAB, FORMAT, FICHIE, NPARIM, ZK24(JPAIM), 
     +              NPARPG, ZK24(JPAPG), FORMAR, FORMAC )
C     ------------------------------------------------------------------
C
C                  --- IMPRESSION DES FONCTIONS ---
C
C     ------------------------------------------------------------------
      CALL GETVTX (' ','IMPR_FONCTION', 1,1,1, IMPRFO, N1 )
      IF ( IMPRFO .EQ. 'OUI' ) THEN
         CALL TBIMFO ( NEWTAB, FICHIE, FORMAR, FORMAC )
      ENDIF
C     ------------------------------------------------------------------
C
      CALL JEDETR ( PARIM )
      CALL JEDETR ( PARPG )
      IF ( NPARFI .NE. 0 )  CALL DETRSD ( 'TABLE' , NEWTA1 )
      IF ( NBTRI  .NE. 0 )  CALL DETRSD ( 'TABLE' , NEWTA2 )
C
 9999 CONTINUE
C
 1000 FORMAT(/,80('-'))
C
      CALL JEDEMA()
      END
