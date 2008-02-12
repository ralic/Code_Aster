      SUBROUTINE CFAMIL(TYPEMA,FAMIL ,NOMFAM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8   TYPEMA
      INTEGER       FAMIL
      CHARACTER*8   NOMFAM            
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN - UTILITAIRE
C
C DONNE FAMILLE DE POINT DE GAUSS SUIVANT TYPE MAILLE ET NUM.FAM.
C FAMILLE DE NOM UTILE DANS LE CATALOQUE DE L'OPTION ARLQ_COUPL
C
C ----------------------------------------------------------------------
C
C
C IN  TYPEMA : TYPE DE LA MAILLE
C IN  FAMIL  : FAMILLE D'INTEGRATION
C OUT NOMFAM : NOM DE LA FAMILLE DE POINTS DE GAUSS
C
C
C ----------------------------------------------------------------------
C

C
C ----------------------------------------------------------------------
C    
      NOMFAM = '        '
C   
      IF ( TYPEMA(1:3) .EQ. 'TRI' ) THEN
        IF (FAMIL.EQ.1) THEN
          NOMFAM = 'ARLQ_1'
        ELSEIF (FAMIL.EQ.2) THEN
          NOMFAM = 'ARLQ_2'
        ELSEIF (FAMIL.EQ.3) THEN
          NOMFAM = 'ARLQ_3'
        ELSEIF (FAMIL.EQ.4) THEN
          NOMFAM = 'ARLQ_4'
        ELSEIF (FAMIL.EQ.5) THEN
          NOMFAM = 'ARLQ_5'
        ELSEIF (FAMIL.EQ.6) THEN
          NOMFAM = 'ARLQ_6'
        ELSEIF (FAMIL.EQ.7) THEN
          NOMFAM = 'ARLQ_7'
        ELSEIF (FAMIL.EQ.8) THEN
          NOMFAM = 'ARLQ_8'
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSEIF ( TYPEMA(1:3) .EQ. 'QUA' ) THEN
        IF (FAMIL.EQ.1) THEN
          NOMFAM = 'ARLQ_1'
        ELSEIF (FAMIL.EQ.2) THEN
          NOMFAM = 'ARLQ_2'
        ELSEIF (FAMIL.EQ.3) THEN
          NOMFAM = 'ARLQ_3'
        ELSEIF (FAMIL.EQ.4) THEN
          NOMFAM = 'ARLQ_4'
        ELSEIF (FAMIL.EQ.5) THEN
          NOMFAM = 'ARLQ_5'
        ELSEIF (FAMIL.EQ.6) THEN
          NOMFAM = 'ARLQ_6'
        ELSEIF (FAMIL.EQ.7) THEN
          NOMFAM = 'ARLQ_7'
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE
        CALLA SSERT(.FALSE.)
      ENDIF

      END
