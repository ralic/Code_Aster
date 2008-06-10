      SUBROUTINE LXDELI( TAB , NBVAL )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*1        TAB(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     DEFINITION DES SEPARATEURS ADMIS PAR L'ANALYSEUR LEXICAL
C     ------------------------------------------------------------------
C                CETTE ROUTINE A VOCATION A ETRE SURCHARGEE
C     ------------------------------------------------------------------
C OUT TAB   : TABLEAU CONTENANT LES SEPARATEURS
C VAR NBVAL : NOMBRE MAXIMUM DE SEPARATEURS   (EN ENTREE)
C           : NOMBRE EFFECTIF DE SEPARATEURS  (EN SORTIE)
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN LXDELI
C     ------------------------------------------------------------------
      IF  ( NBVAL.GE. 11) THEN
         TAB( 1) = '='
         TAB( 2) = '('
         TAB( 3) = ')'
         TAB( 4) = ':'
         TAB( 5) = ';'
         TAB( 6) = ','
         TAB( 7) = '%'
         TAB( 8) = '&'
         TAB( 9) = '*'
         TAB(10) = '/'
         TAB(11) = '!'
         NBVAL   =  11
      ELSE
         NBVAL   = 0
      ENDIF
C
      END
