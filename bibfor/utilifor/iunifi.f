      INTEGER FUNCTION IUNIFI( NAME )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)            NAME
C
C     ------------------------------------------------------------------
C     RETOURNE L'UNITE LOGIQUE ATTACHEE AU NOM NAME
C     ------------------------------------------------------------------
C
C IN  NAME   : CH*16 : NOM DONT ON RECHERCHE LE NUMERO D'UNITE LOGIQUE
C OUT IUNIFI : IS   : NUMERO D'UNITE LOGIQUE ASSOCIE A "NAME"
C                     RENVOI 0 SI LE NOM N'EST PAS DANS LES TABLES
C
C     ------------------------------------------------------------------
C     REMARQUE : SUPPOSE QUE LA DEFINITION DU COUPLE (UNITE LOGIQUE,NOM)
C                EST DEJA FAITE (CF DEFUFI)
C     REMARQUE : SI L'INITIALISATION N'A PAS ETE FAITE LA ROUTINE S'EN
C                CHARGERA (APPEL A INITFI)
C     ------------------------------------------------------------------
C MODIF UTILIFOR  DATE 05/12/2001   AUTEUR VABHHTS J.PELLET 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C         INITFI
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN IUNIFI
C
      PARAMETER              (MXFILE=100)
      CHARACTER*16      DDNAME(MXFILE)
      INTEGER          FIRST, UNITES(MXFILE) , NBFILE
      COMMON/ CN01FI / FIRST, UNITES         , NBFILE
      COMMON/ CC01FI / DDNAME
C
      CHARACTER*16  NAME16
C
C     --- INITIALISATION (SI NECESSAIRE) ---
      IF ( FIRST .NE. 15091989 ) CALL INITFI
C
      NAME16 = NAME
      IUNIFI = 0
      DO 1 I = 1, NBFILE
        IF( NAME16 .EQ. DDNAME(I) ) THEN
          IUNIFI = UNITES(I)
          GOTO 2
        ENDIF
   1  CONTINUE
   2  CONTINUE
      END
