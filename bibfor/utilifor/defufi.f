      SUBROUTINE DEFUFI( UNIT , NAME )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 05/12/2001   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            UNIT
      CHARACTER*(*)             NAME
C
C     ------------------------------------------------------------------
C     DEFINITION DE LA CORRESPONDANCE UN NOM UTILISATEUR ET UN NUMERO
C     D'UNITE LOGIQUE
C     ------------------------------------------------------------------
C IN  UNIT   : IS   : NUMERO D'UNITE LOGIQUE
C IN  NAME   : CH*16 : NOM ASSOCIE AU NUMERO D'UNITE LOGIQUE UNIT
C     ------------------------------------------------------------------
C     CONVENTION : SI UNIT <= 0 ALORS ON RETIRE LE NOM "NAME" DES TABLES
C     ------------------------------------------------------------------
C     REMARQUE : AUCUNE ALLOCATION DYNAMIQUE N'EST FAITE,
C        L'OUVERTURE DE L'UNITE LOGIQUE EST LA CHARGE DE L'UTILISATEUR
C     ------------------------------------------------------------------
C     LIMITATION :  ON NE PEUT DEFINIR SIMULTANEMENT QUE (MXFILE=100)
C        CORRESPONDANCE
C     ------------------------------------------------------------------
C     REMARQUE : SI L'INITIALISATION N'A PAS ETE FAITE LA ROUTINE S'EN
C                CHARGERA (APPEL A INITFI)
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         INITFI
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN DEFUFI
C     ------------------------------------------------------------------
C
      PARAMETER              (MXFILE=100)
      CHARACTER*16      DDNAME(MXFILE)
      INTEGER          FIRST, UNITES(MXFILE) , NBFILE
      COMMON/ CN01FI / FIRST, UNITES         , NBFILE
      COMMON/ CC01FI / DDNAME
      CHARACTER*16  NAME16
C
C     --- INITIALISATION (SI NECESSAIRE) ---
      IF ( FIRST .NE. 15091989 ) CALL INITFI
C
      NAME16 = NAME
      IF ( UNIT .LT. 0 )THEN
C        --- ON RETIRE LE DDNAME DE LA TABLE ----
         DO 10 IFILE = 1, MXFILE
            IF ( DDNAME(IFILE) .EQ. NAME16 ) THEN
               DDNAME(IFILE) = ' '
               UNITES(IFILE) = -1
               GOTO 11
            ENDIF
  10     CONTINUE
  11     CONTINUE
      ELSE
C        --- INSERTION DEMANDEE ---
         ILIBRE = 0
         DO 20 IFILE = 1, NBFILE
            IF ( DDNAME(IFILE) .EQ. NAME16 ) THEN
C              --- DEJA PRESENT ---
               UNITES(IFILE) = UNIT
               GOTO 21
            ELSEIF (DDNAME(IFILE) .EQ. '  ' ) THEN
C              --- RECHERCHE DE LA DERNIERE PLACE LIBRE ---
               ILIBRE = IFILE
            ENDIF
  20     CONTINUE
         IF ( ILIBRE .EQ. 0 ) THEN
            NBFILE = NBFILE + 1
            IF (NBFILE.GT. MXFILE) GOTO 21
            ILIBRE = NBFILE
         ENDIF
         DDNAME(ILIBRE) = NAME16
         UNITES(ILIBRE) = UNIT
  21     CONTINUE
      ENDIF
C
      END
