      SUBROUTINE FIDPIL(JTERM,TERM,LPILE,PILE,ICHOSE,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                 TERM(*),   PILE(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 03/07/96   AUTEUR CIBHHLV L.VIVAN 
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
C     VIDER LA PILE DANS TERM JUSQU'A TROUVER ICHOSE
C     ------------------------------------------------------------------
      IF (LPILE.EQ.0) THEN
         IER = IER + 1
         CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIDPIL.01)',
     +                                            'EXPRESSION ERRONE')
         GOTO 9999
      ENDIF
C
  20  CONTINUE
      IF( PILE(LPILE).NE.ICHOSE) THEN
        JTERM = JTERM + 1
        TERM(JTERM) = PILE(LPILE)
        LPILE = LPILE-1
        IF (LPILE.EQ.0) THEN
           IF ( ICHOSE.NE.-1) THEN
              IER = IER + 1
              CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIDPIL.02)',
     +                                            'EXPRESSION ERRONE')
           ENDIF
           GOTO 9999
        ELSE
           GOTO 20
        ENDIF
      ELSE
        LPILE = LPILE-1
      ENDIF
 9999 CONTINUE
      END
