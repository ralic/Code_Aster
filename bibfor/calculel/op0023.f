      SUBROUTINE OP0023(IER)
      IMPLICIT   NONE
      INTEGER IER
C ----------------------------------------------------------------------
C MODIF CALCULEL  DATE 09/05/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C ----------------------------------------------------------------------
C     COMMANDE:  TEST_RESU
C ----------------------------------------------------------------------
C     REMARQUES:  RESU:( RESULTAT:
C                        PRECISION: ( PREC1 , PREC2 )          L_R8
C                        CRITERE  : ( CRIT1 , CRIT2 )          L_TXM
C     PREC1 ET CRIT1 SONT LA PRECISION ET LE CRITERE DU TEST
C     PREC2 ET CRIT2 SONT LA PRECISION ET LE CRITERE DE L'EXTRACTION
C ----------------------------------------------------------------------
      INTEGER       IFIC, N1, NOCC
      LOGICAL       ULEXIS
      CHARACTER*16  NOMFI
C     ------------------------------------------------------------------

      CALL INFMAJ()

      IFIC = 0
      NOMFI = ' '
      CALL GETVIS ( ' ', 'UNITE', 1,1,1, IFIC , N1 )
      IF ( .NOT. ULEXIS( IFIC ) ) THEN
        CALL ULOPEN ( IFIC, ' ', NOMFI, 'NEW', 'O' )
      ENDIF
      WRITE (IFIC,1000)

C     --- TRAITEMENT D'UN OBJET JEVEUX  ---

      CALL GETFAC ( 'OBJET', NOCC )
      IF ( NOCC .NE. 0 )  CALL TRJEVE ( IFIC, NOCC )

C     --- TRAITEMENT D'UN CHAM_NO ---

      CALL GETFAC ( 'CHAM_NO', NOCC )
      IF ( NOCC .NE. 0 )  CALL TRCHNO ( IFIC, NOCC )

C     --- TRAITEMENT D'UN CHAM_ELEM ---

      CALL GETFAC ( 'CHAM_ELEM', NOCC )
      IF ( NOCC .NE. 0 )  CALL TRCHEL ( IFIC, NOCC )

C     --- TRAITEMENT D'UN CONCEPT RESULTAT ---

      CALL GETFAC ( 'RESU', NOCC )
      IF ( NOCC .NE. 0 )  CALL TRRESU ( IFIC, NOCC )

C     --- TRAITEMENT D'UN CONCEPT GENE ---

      CALL GETFAC ( 'GENE', NOCC )
      IF ( NOCC .NE. 0 )  CALL TRGENE ( IFIC, NOCC )

 1000 FORMAT (/,80 ('-'))
      END
