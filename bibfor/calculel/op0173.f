      SUBROUTINE OP0173 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 21/01/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C     COMMANDE:  EXTR_TABLE
C
C ----------------------------------------------------------------------
      INTEGER       IBID, N1, IRET, NPARFI, VALI
      REAL*8        R8B, VALR
      COMPLEX*16    CBID, VALC
      CHARACTER*8   K8B, NOMRES, CTYPE, TABLE
      CHARACTER*16  NOMCMD, CONCEP, TYPESD
      CHARACTER*19  NEWTAB, NEWTA1
      CHARACTER*24  PARA
      CHARACTER*80  VALK
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
      CALL GETVID ( ' ', 'TABLE'    ,1,1,1, TABLE, N1 )
      NEWTAB = TABLE
C
      CALL GETVTX ( ' ', 'NOM_PARA' , 1,1,1, PARA, N1 )
C
      CALL GETVTX ( ' ', 'TYPE_RESU', 1,1,1, TYPESD, N1 )
C
      CALL GETFAC ( 'FILTRE' , NPARFI )
      IF ( NPARFI .NE. 0 ) THEN
         NEWTA1 = '&&OP0173.FILTRE '
         CALL TBIMFI ( NPARFI, NEWTAB, NEWTA1 )
         NEWTAB = NEWTA1
      ENDIF 
C
      CALL TBLIVA ( NEWTAB, 0, K8B, IBID, R8B, CBID, K8B, K8B, R8B, 
     +                      PARA, CTYPE, VALI, VALR, VALC, VALK, IRET )
      IF ( IRET .EQ. 0 ) THEN
      ELSEIF ( IRET .EQ. 1 ) THEN
         CALL UTMESS('F',NOMCMD,'LE NOM_PARA N''EXISTE PAS' )
      ELSEIF ( IRET .EQ. 2 ) THEN
         CALL UTMESS('F',NOMCMD,'0 LIGNE TROUVEE POUR LE NOM_PARA')
      ELSEIF ( IRET .EQ. 3 ) THEN
         CALL UTMESS('F',NOMCMD,'PLUSIEURS LIGNES TROUVEES')
      ELSE
         CALL UTMESS('F',NOMCMD,'CODE RETOUR DE "TBLIVA" INCONNU')
      ENDIF
C
      IF ( TYPESD .EQ. 'MATR_ASSE_GENE_R' ) THEN
C          ------------------------------
         CALL COPISD ( 'MATR_ASSE_GENE', 'G', VALK, NOMRES )
C
      ELSE
         CALL UTMESS('F',NOMCMD,'TYPE_RESU INCONNU: '//TYPESD)
      ENDIF
C
      IF ( NPARFI .NE. 0 )  CALL DETRSD ( 'TABLE' , NEWTA1 )
C
      CALL TITRE()
C
      CALL JEDEMA ( )
C
      END
