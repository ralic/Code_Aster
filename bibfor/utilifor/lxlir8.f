      SUBROUTINE LXLIR8(CHAINE,RVAL,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CHAINE
      REAL*8                   RVAL
      INTEGER                       IER
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
C     DECODAGE D'UN REEL ECRIT EN CHAINE DE CARACTERES
C     ------------------------------------------------------------------
C IN  CHAINE : CH*(*) : CHAINE DE CARACTERES CONTENANT L'ENTIER
C OUT IVAL   : R8     : REEL DECODE
C OUT IER    : IS     : CODE RETOUR
C              = 0  PAS D'ERREUR ON A BIEN LU UN ENTIER (IVAL)
C              = 1  ON A LU AUTRE CHOSE QU'UN ENTIER
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         LXSCAN
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN LXLIR8
C     ------------------------------------------------------------------
      CHARACTER*80 CVAL
      INTEGER      IVAL
C
      IER  = 0
      ICOL = 1
      CALL LXSCAN(CHAINE,ICOL,ICLASS,IVAL,RVAL,CVAL)
C     ------------------------------------------------------------------
C                          ICLASS      CODE DE CE QUE L'ON A TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C          -1 FIN DE LIGNE   (RIEN A LIRE)
C           0 ERREUR         CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C           1 ENTIER         IVAL DE TYPE INTEGER
C           2 REEL           RVAL DE TYPE REAL*8
C           3 IDENTIFICATEUR CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C           4 TEXTE          CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C           5 SEPARATEUR     CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C     ------------------------------------------------------------------
      IF (ICLASS.NE.2) IER = 1
      END
