      SUBROUTINE OP0177 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
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
C
C     COMMANDE:  TEST_TABLE
C
C ----------------------------------------------------------------------
      INTEGER      IBID, N1, N2, N3, IRET, IUNIFI, IFIC, NPARFI, 
     +             VALI, REFI
      REAL*8       R8B, VALR, REFR, PREC
      COMPLEX*16   CBID, VALC, REFC
      CHARACTER*1  TYPR
      CHARACTER*4  TESTOK
      CHARACTER*8  K8B, NOMFI, CRIT, CTYPE, TYPTES, TABLE
      CHARACTER*12 LABEL
      CHARACTER*19 NEWTAB, NEWTA1
      CHARACTER*24 PARA
      CHARACTER*80 VALK
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      TESTOK = 'NOOK'
      LABEL  = ' '
C
      CALL GETVTX ( ' ', 'FICHIER', 0,1,1, NOMFI, N1 )
      IF ( N1 .NE. 0 ) THEN
         IFIC = IUNIFI( NOMFI )
         IF ( IFIC .EQ. 0 ) THEN
            CALL UTMESS('A','TEST_TABLE',
     +                  'LE NOM DU FICHIER D''IMPRESSION EST '//
     +                  'INCONNU : '//NOMFI//' ON PRENDRA LE FICHIER'//
     +                  ' "RESULTAT". ')
            IFIC = IUNIFI('RESULTAT')
         ENDIF
      ELSE
         IFIC = IUNIFI('RESULTAT')
      ENDIF
      WRITE(IFIC,1000)
C
      CALL GETVID ( ' ', 'TABLE' ,1,1,1, TABLE, N1 )
      LABEL(1:8) = TABLE
      NEWTAB = TABLE
C
C     ------------------------------------------------------------------
C
C                 --- TRAITEMENT DU MOT CLE "FILTRE" ---
C
C     ------------------------------------------------------------------
      CALL GETFAC ( 'FILTRE' , NPARFI )
      IF ( NPARFI .NE. 0 ) THEN
         NEWTA1 = '&&OP0177.FILTRE '
         CALL TBIMFI ( NPARFI, NEWTAB, NEWTA1 )
         NEWTAB = NEWTA1
      ENDIF 
C     ------------------------------------------------------------------
C
      CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, PREC, N1 )
      CALL GETVTX ( ' ', 'CRITERE'  , 1,1,1, CRIT, N1 )
C
      CALL GETVR8 ( ' ', 'VALE'  , 1,1,1, REFR, N1 )
      CALL GETVIS ( ' ', 'VALE_I', 1,1,1, REFI, N2 )
      CALL GETVC8 ( ' ', 'VALE_C', 1,1,1, REFC, N3 )
      TYPR = 'R'
      IF ( N2 .NE. 0 ) TYPR = 'I'
      IF ( N3 .NE. 0 ) TYPR = 'C'
C
      CALL GETVTX ( ' ', 'NOM_PARA', 1,1,1, PARA, N1 )
C
      WRITE (IFIC,*) '---- TABLE: ', TABLE, ' NOM_PARA: ', PARA
      CALL UTEST3 ( IFIC, ' ', 1 )
C
      CALL GETVTX ( ' ', 'TYPE_TEST', 1,1,1, TYPTES, N1 )
C
      IF ( N1. NE. 0 ) THEN
         CALL UTEST0 ( NEWTAB, PARA, TYPTES, TYPR, REFI, REFR, REFC, 
     +                                       PREC, CRIT, IFIC )
         GOTO 9999 
      ENDIF
C
      CALL TBLIVA ( NEWTAB, 0, K8B, IBID, R8B, CBID, K8B, K8B, R8B, 
     +                      PARA, CTYPE, VALI, VALR, VALC, VALK, IRET )
      IF ( IRET .EQ. 0 ) THEN
      ELSEIF ( IRET .EQ. 1 ) THEN
         WRITE (IFIC,*) TESTOK,' LE NOM_PARA N''EXISTE PAS '
         GOTO 9999
      ELSEIF ( IRET .EQ. 2 ) THEN
         WRITE (IFIC,*) TESTOK,' 0 LIGNE TROUVEE POUR LES NOM_PARA '
         GOTO 9999
      ELSEIF ( IRET .EQ. 3 ) THEN
         WRITE (IFIC,*) TESTOK,' PLUSIEURS LIGNES TROUVEES '
         GOTO 9999
      ELSE
         WRITE (IFIC,*) TESTOK,' CODE RETOUR DE "TBLIVA" INCONNU '
         GOTO 9999
      ENDIF
      IF ( CTYPE(1:1) .NE. TYPR ) THEN
         WRITE (IFIC,*) TESTOK,' LES TYPES NE CORRESPONDENT PAS '
         GOTO 9999
      ENDIF
      CALL UTITES ( LABEL, PARA, TYPR, REFI, REFR, REFC,
     +                           VALI, VALR, VALC, PREC, CRIT, IFIC )
C
 9999 CONTINUE
      IF ( NPARFI .NE. 0 )  CALL DETRSD ( 'TABLE' , NEWTA1 )
C
1000  FORMAT(/,80('-'))
      CALL JEDEMA ( )
C
      END
