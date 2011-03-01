      SUBROUTINE OP0020()
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 01/03/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ECRITURE DES CATALOGUES D'ELEMENTS COMPILES
C     ET PRODUCTION DES IMPRESSIONS DE COMPLETUDE DES OPTIONS ET TYPELEM
C     ------------------------------------------------------------------
C
      INTEGER       OUIELT,IMPR,N1,NBOCC,IER
      CHARACTER*24  FICHIE
      LOGICAL       ULEXIS
C     ------------------------------------------------------------------
C
      IMPR   = 0
      FICHIE = ' '
      CALL GETVIS ( ' ', 'UNITE'  , 1,1,1, IMPR  , N1 )
      IF ( .NOT. ULEXIS( IMPR ) ) THEN
         CALL ULOPEN ( IMPR, ' ', FICHIE, 'NEW', 'O' )
      ENDIF
      CALL GETFAC( 'ELEMENT '  , OUIELT )
C     
C     ---------- COMPILATION DES CATATLOGUES ---------------------------
      IF ( OUIELT .NE. 0 ) THEN
         CALL IBCAEL('ECRIRE')
      ENDIF
C
C     ---------- TRAITEMENT DE CE QUI EST RELATIF A LA COMPLETUDE ------
      CALL GETFAC( 'TYPE_ELEM', NBOCC )
      IF ( NBOCC .GT. 0 ) CALL AIDTY2( IMPR )
C     ------------------------------------------------------------------
C
      END
