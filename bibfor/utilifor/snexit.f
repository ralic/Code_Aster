      SUBROUTINE SNEXIT( DELIM , NBDEL , ICODER )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      DELIM(*)
      INTEGER                    NBDEL , ICODER
C
C     ------------------------------------------------------------------
C     AVANCE JUSQU'A TROUVER UN SEPARATEUR DONNE (DANS UNE LISTE)
C     ------------------------------------------------------------------
C IN  DELIM(*)    LISTE DES SEPARATEURS
C IN  NBDEL       NOMBRE DE SEPARATEURS
C OUT ICODER      CODE RETOUR
C           -- ICODER -----   ----- SIGNIFICATION ----------------------
C               -1            FIN DE FICHIER RENCONTREE
C            0 < I <= NBDEL   INDICE DU SEPARATEUR DANS LE TABLEAU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 29/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         SNLIRE
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN SNEXIT
C     ------------------------------------------------------------------
C
      REAL*8        RVAL
      INTEGER       IVAL , ICLASS
      CHARACTER*80  CVAL
C
 10   CONTINUE
      CALL SNLIRE(ICLASS,IVAL,RVAL,CVAL)
C-DEL CALL SNDBG (6,ICLASS,IVAL,RVAL,CVAL)
         IF ( ICLASS .GT. 6 ) THEN
            DO 20 IC = 1 , NBDEL
               IF ( CVAL(1:1) .EQ. DELIM(IC)(1:1) ) THEN
                  ICODER = IC
                  GOTO 30
               ENDIF
 20         CONTINUE
         ELSEIF ( ICLASS .EQ. -1 ) THEN
C           EOF RENCONTREE
            ICODER = -1
            GOTO 30
         ENDIF
      GOTO 10
 30   CONTINUE
C
      END
