      SUBROUTINE UTREMT ( MOT, LISTE, NBVAL, PLACE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       MOT, LISTE(*)
      INTEGER                         NBVAL, PLACE
C
C     ------------------------------------------------------------------
C     RECHERCHE UN MOT DONNE DANS UN LISTE (TABLEAU) DE MOTS
C     ------------------------------------------------------------------
C IN  MOT      : CH*(*) : LE MOT CHERCHE DANS LA LISTE
C IN  LISTE(*) : CH*(*) : LISTE DE MOT PROPOSE
C IN  NBVAL    : IS     : NOMBRE DE MOT DANS LA LISTE
C OUT PLACE    : IS     : PLACE DU MOT DANS LA LISTE
C                         = 0  SI LE MOT EST ABSENT DE LA LISTE
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
C
      PLACE = 0
      DO 10 I = 1, NBVAL
         IF ( MOT .EQ. LISTE(I) ) THEN
            PLACE = I
            GOTO 9999
         ENDIF
  10  CONTINUE
 9999 CONTINUE
      END
