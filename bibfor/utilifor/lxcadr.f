      SUBROUTINE LXCADR(CHAINE)
      IMPLICIT NONE
      CHARACTER*(*)     CHAINE
C
C     ------------------------------------------------------------------
C      CADRAGE A GAUCHE D'UN TEXTE
C     ------------------------------------------------------------------
C VAR CHAINE CHAINE A CADRER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C         -
C     ROUTINE(S) FORTRAN     :
C         LEN
C     ------------------------------------------------------------------
C FIN LXCADR
C     ------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
      INTEGER ILONG ,LDEC ,LONG 
C-----------------------------------------------------------------------
      LONG = LEN(CHAINE)
      DO 10 ILONG = 1, LONG
         IF ( CHAINE(ILONG:ILONG) .NE. ' ' ) THEN
            LDEC = ILONG-1
            GOTO 11
         ENDIF
  10  CONTINUE
C     --- CAS DE LA CHAINE BLANCHE ---
      LDEC = 0
C     --- CAS STANDARD ---
  11  CONTINUE
      IF ( ILONG .GT. 0 ) THEN
         DO 20 ILONG = 1 , LONG-LDEC
            CHAINE(ILONG:ILONG) = CHAINE(ILONG+LDEC:ILONG+LDEC)
  20     CONTINUE
         CHAINE(LONG-LDEC+1:) = ' '
      ENDIF
      END
