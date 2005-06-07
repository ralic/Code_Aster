      SUBROUTINE UTEST3 ( IFIC, MCLFAC, IOCC )
      IMPLICIT   NONE
      INTEGER             IFIC, IOCC
      REAL*8                          REFR,            EPSI
      CHARACTER*(*)       MCLFAC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 15/06/99   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
C IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C IN  : MCLFAC : MOT CLE FACTEUR
C IN  : IOCC   : NUMERO D'OCCURRENCE
C OUT : IMPRESSION SUR LISTING
C ----------------------------------------------------------------------
      INTEGER       N1, N2
      CHARACTER*16  REFE, VERS
C     ------------------------------------------------------------------
C
      CALL GETVTX ( MCLFAC, 'REFERENCE', IOCC,1,1, REFE, N1 )
      CALL GETVTX ( MCLFAC, 'VERSION'  , IOCC,1,1, VERS, N2 )
C
      IF ( N1 .NE. 0 ) THEN
         IF ( N2 .NE. 0 ) THEN
            WRITE(IFIC,1010) REFE, VERS
         ELSE
            WRITE(IFIC,1000) REFE
         ENDIF
      ELSE
         WRITE(IFIC,1000) 'NON_DEFINI'
      ENDIF
C
 1000 FORMAT ( 'REFERENCE: ', A )
 1010 FORMAT ( 'REFERENCE: ', A, ' VERSION: ', A )
C
      END
