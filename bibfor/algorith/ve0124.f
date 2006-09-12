      SUBROUTINE VE0124 ( )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/09/2006   AUTEUR REZETTE C.REZETTE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C     COMMANDE: CREA_RESU
C     VERIFICATION DE NIVEAU 1
C ----------------------------------------------------------------------
      CHARACTER*8   K8BID, RESU
      CHARACTER*16  TYPE, OPER, TYPRES
C     ------------------------------------------------------------------
      CALL GETRES(RESU,TYPE,OPER)
      CALL GETVTX(' ','TYPE_RESU',0,1,1,TYPRES,N1)
      CALL GETFAC('AFFE',IOCC)
C
      IF ( TYPRES .EQ. 'EVOL_THER' ) THEN
        DO 700 K = 1,IOCC
          CALL GETVTX('AFFE','NOM_CAS',K,1,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"NOM_CAS" N''EST PAS UNE VARI'//
     +           'ABLE D''ACCES D''UN RESULTAT DE TYPE "EVOL_THER".')
          ENDIF
          CALL GETVIS('AFFE','NUME_MODE',K,1,1,IBID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"NUME_MODE" N''EST PAS UNE VA'//
     +         'RIABLE D''ACCES D''UN RESULTAT DE TYPE "EVOL_THER".')
          ENDIF
 700    CONTINUE
C
      ELSEIF ( TYPRES .EQ. 'MULT_ELAS' ) THEN
        DO 702 K = 1,IOCC
          CALL GETVIS('AFFE','NUME_MODE',K,1,1,IBID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"NUME_MODE" N''EST PAS UNE VA'//
     +         'RIABLE D''ACCES D''UN RESULTAT DE TYPE "MULT_ELAS".')
          ENDIF
          CALL GETVR8('AFFE','INST',K,1,1,R8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"INST" N''EST PAS UNE VARIA'//
     +           'BLE D''ACCES D''UN RESULTAT DE TYPE "MULT_ELAS".')
          ENDIF
          CALL GETVID('AFFE','LIST_INST',K,1,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"INST" N''EST PAS UNE VARIA'//
     +           'BLE D''ACCES D''UN RESULTAT DE TYPE "MULT_ELAS".')
          ENDIF
 702    CONTINUE
C
      ELSEIF ( TYPRES .EQ. 'FOURIER_ELAS' ) THEN
        DO 704 K = 1,IOCC
          CALL GETVTX('AFFE','NOM_CAS',K,1,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"NOM_CAS" N''EST PAS UNE VAR'//
     +       'IABLE D''ACCES D''UN RESULTAT DE TYPE "FOURIER_ELAS".')
          ENDIF
          CALL GETVR8('AFFE','INST',K,1,1,R8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"INST" N''EST PAS UNE VARIA'//
     +         'BLE D''ACCES D''UN RESULTAT DE TYPE "FOURIER_ELAS".')
          ENDIF
          CALL GETVID('AFFE','LIST_INST',K,1,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"INST" N''EST PAS UNE VARIA'//
     +         'BLE D''ACCES D''UN RESULTAT DE TYPE "FOURIER_ELAS".')
          ENDIF
 704    CONTINUE
C 
      ELSEIF ( TYPRES .EQ. 'FOURIER_THER' ) THEN
        DO 705 K = 1,IOCC
          CALL GETVTX('AFFE','NOM_CAS',K,1,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"NOM_CAS" N''EST PAS UNE VAR'//
     +       'IABLE D''ACCES D''UN RESULTAT DE TYPE "FOURIER_THER".')
          ENDIF
          CALL GETVR8('AFFE','INST',K,1,1,R8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"INST" N''EST PAS UNE VARIA'//
     +         'BLE D''ACCES D''UN RESULTAT DE TYPE "FOURIER_THER".')
          ENDIF
          CALL GETVID('AFFE','LIST_INST',K,1,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL UTMESS('E','OP0124','"INST" N''EST PAS UNE VARIA'//
     +         'BLE D''ACCES D''UN RESULTAT DE TYPE "FOURIER_THER".')
          ENDIF
 705    CONTINUE
      ENDIF

      CALL GETFAC ( 'PERM_CHAM', IOCC )
      IF ( IOCC .GT. 0 ) THEN
         CALL GETVID ( ' ', 'RESU_INIT'    , 1,1,0, K8BID, N1 )
         IF ( N1 .EQ. 0 ) THEN
            CALL UTMESS('E','OP0124','"RESU_INIT" EST OBLIGATOIRE')
         ENDIF
         CALL GETVID ( ' ', 'MAILLAGE_INIT', 1,1,0, K8BID, N1 )
         IF ( N1 .EQ. 0 ) THEN
            CALL UTMESS('E','OP0124','"MAILLAGE_INIT" EST OBLIGATOIRE')
         ENDIF
         CALL GETVID ( ' ', 'RESU_FINAL'   , 1,1,0, K8BID, N1 )
         IF ( N1 .EQ. 0 ) THEN
            CALL UTMESS('E','OP0124','"RESU_FINAL" EST OBLIGATOIRE')
         ENDIF
         CALL GETVID ( ' ', 'MAILLAGE_FINAL' , 1,1,0, K8BID, N1 )
         IF ( N1 .EQ. 0 ) THEN
            CALL UTMESS('E','OP0124','"MAILLAGE_FINAL" EST OBLIGATOIRE')
         ENDIF
         DO 706 K = 1,IOCC
            CALL GETVR8 ( 'PERM_CHAM', 'TRAN' , K,1,0, R8BID, N1 )
            IF ( N1 .NE. -3 ) THEN
               CALL UTMESS('E','OP0124','3 VALEURS POUR "TRAN"')
            ENDIF
 706     CONTINUE
      ENDIF
C
      END
