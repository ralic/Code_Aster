      SUBROUTINE OP0028(IER)
      IMPLICIT   NONE
      INTEGER    IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/07/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     GENE_VARI_ALEA
C
C ----------------------------------------------------------------------
      INTEGER      N1, IBID
      REAL*8       MOYEN, A, B, DELTA, GEVAT1, GEVAT2, GEVAT3, SUM
      COMPLEX*16   CBID
      CHARACTER*8  K8B, RESULT, INIT
      CHARACTER*16 NOMCMD, CONCEP, CTYPE
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
C      
      IER=0
C
      CALL GETRES ( RESULT, CONCEP, NOMCMD )
C
      CALL GETVTX ( ' ', 'TYPE'    , 1,1,1, CTYPE, N1 )
      CALL GETVR8 ( ' ', 'VALE_MOY', 1,1,1, MOYEN, N1 )
      CALL GETVR8 ( ' ', 'DELTA'   , 1,1,1, DELTA, N1 )
      CALL GETVR8 ( ' ', 'A'       , 1,1,1, A    , N1 )
      CALL GETVR8 ( ' ', 'B'       , 1,1,1, B    , N1 )
      CALL GETVTX ( ' ', 'INIT'    , 1,1,1, INIT , N1 ) 
      
      IF (INIT(1:3).EQ.'OUI') CALL INIRAN()
C
      CALL TBCRSD ( RESULT, 'G')
      CALL TBAJPA ( RESULT, 1, 'NBRE', 'R' )
C
      IF ( CTYPE(1:12) .EQ. 'EXP_TRONQUEE' ) THEN
C                            
         SUM = GEVAT1(A,B,MOYEN)
C
      ELSEIF ( CTYPE(1:13) .EQ. 'EXPONENTIELLE' ) THEN
C
         SUM = GEVAT2(A,MOYEN)
C
      ELSEIF ( CTYPE(1:5) .EQ.'GAMMA' ) THEN
C
         SUM = GEVAT3(A,MOYEN,DELTA) 
C
      ENDIF
C
      CALL TBAJLI ( RESULT, 1, 'NBRE', IBID, SUM, CBID, K8B,0 )
C
      CALL JEDEMA()
      END
