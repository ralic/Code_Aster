      SUBROUTINE VE0124 ( )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     COMMANDE: CREA_RESU
C     VERIFICATION DE NIVEAU 1
C ----------------------------------------------------------------------
      CHARACTER*8   K8BID, RESU
      CHARACTER*16  TYPE, OPER, TYPRES
      INTEGER      IARG
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IBID ,IOCC ,K ,N0 ,N1 
      REAL*8 R8BID 
C-----------------------------------------------------------------------
      CALL GETRES(RESU,TYPE,OPER)
      CALL GETVTX(' ','TYPE_RESU',0,IARG,1,TYPRES,N1)
      CALL GETFAC('AFFE',IOCC)
C
      IF ( TYPRES .EQ. 'EVOL_THER' ) THEN
        DO 700 K = 1,IOCC
          CALL GETVTX('AFFE','NOM_CAS',K,IARG,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_7')
          ENDIF
          CALL GETVIS('AFFE','NUME_MODE',K,IARG,1,IBID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_8')
          ENDIF
 700    CONTINUE
C
      ELSEIF ( TYPRES .EQ. 'MULT_ELAS' ) THEN
        DO 702 K = 1,IOCC
          CALL GETVIS('AFFE','NUME_MODE',K,IARG,1,IBID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_9')
          ENDIF
          CALL GETVR8('AFFE','INST',K,IARG,1,R8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_10')
          ENDIF
          CALL GETVID('AFFE','LIST_INST',K,IARG,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_10')
          ENDIF
 702    CONTINUE
C
      ELSEIF ( TYPRES .EQ. 'FOURIER_ELAS' ) THEN
        DO 704 K = 1,IOCC
          CALL GETVTX('AFFE','NOM_CAS',K,IARG,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_11')
          ENDIF
          CALL GETVR8('AFFE','INST',K,IARG,1,R8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_12')
          ENDIF
          CALL GETVID('AFFE','LIST_INST',K,IARG,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_12')
          ENDIF
 704    CONTINUE
C
      ELSEIF ( TYPRES .EQ. 'FOURIER_THER' ) THEN
        DO 705 K = 1,IOCC
          CALL GETVTX('AFFE','NOM_CAS',K,IARG,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_13')
          ENDIF
          CALL GETVR8('AFFE','INST',K,IARG,1,R8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_14')
          ENDIF
          CALL GETVID('AFFE','LIST_INST',K,IARG,1,K8BID,N0)
          IF ( N0 .NE. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_14')
          ENDIF
 705    CONTINUE
      ENDIF

      CALL GETFAC ( 'PERM_CHAM', IOCC )
      IF ( IOCC .GT. 0 ) THEN
         CALL GETVID ( ' ', 'RESU_INIT'    , 1,IARG,0, K8BID, N1 )
         IF ( N1 .EQ. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_15')
         ENDIF
         CALL GETVID ( ' ', 'MAILLAGE_INIT', 1,IARG,0, K8BID, N1 )
         IF ( N1 .EQ. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_16')
         ENDIF
         CALL GETVID ( ' ', 'RESU_FINAL'   , 1,IARG,0, K8BID, N1 )
         IF ( N1 .EQ. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_17')
         ENDIF
         CALL GETVID ( ' ', 'MAILLAGE_FINAL' , 1,IARG,0, K8BID, N1 )
         IF ( N1 .EQ. 0 ) THEN
            CALL U2MESS('E','ALGORITH11_18')
         ENDIF
      ENDIF
C
      END
