      SUBROUTINE ASOPEN ( UNIT, FICHIE )
      IMPLICIT   NONE
      INTEGER             UNIT
      CHARACTER*(*)       FICHIE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 16/07/2002   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_6
C
C     OUVERTURE DE L'UNITE LOGIQUE
C
C IN  : UNIT   : NUMERO D'UNITE LOGIQUE
C IN  : NAME   : NOM ASSOCIE AU NUMERO D'UNITE LOGIQUE UNIT
C
C     ATTENTION ecriture du NAME en minuscules.
C     ------------------------------------------------------------------
C
      INTEGER          MXFILE
      PARAMETER      ( MXFILE = 100 )
      INTEGER          UNITFI(MXFILE) , NBFILE
      COMMON/ CN02FI / UNITFI         , NBFILE
      INTEGER          I,K
      LOGICAL          V11, FIRST
      CHARACTER*8      K8B
      CHARACTER*64     NAME
      DATA FIRST      /.TRUE./
C     ------------------------------------------------------------------
C
C     --- SI NUL, ON NE FAIT RIEN ---
C
      IF ( FIRST ) THEN
        NBFILE = 0
        DO 1 K = 1,MXFILE
          UNITFI(K) = 0
 1      CONTINUE
        FIRST = .FALSE.
      ENDIF
      IF ( UNIT .GT. 0 ) THEN
C
        DO 10 I = 1 , NBFILE
          IF ( UNITFI(I) .EQ. UNIT ) GOTO 9999
 10     CONTINUE
        IF ( FICHIE(1:1) .EQ. ' ' ) THEN
           CALL CODENT ( UNIT, 'G', K8B )
           NAME = 'fort.'//K8B
        ELSE
           NAME = FICHIE
        ENDIF
C
C     --- VERIFICATION DE L'OUVERTURE DU FICHIER ---
C
        INQUIRE ( UNIT, OPENED=V11 )
C
        IF ( .NOT. V11 ) THEN
          IF (UNIT .NE. 6) OPEN ( UNIT=UNIT, FILE=NAME )
        ELSE
          GOTO 9999
        ENDIF
C
C     --- ON STOCKE DANS LE COMMON ---
C
        DO 15 I = 1 , NBFILE
          IF ( UNITFI(I) .EQ. 0 ) THEN
          UNITFI(I) = UNIT
          GOTO 9999
        ENDIF
 15     CONTINUE
        NBFILE = NBFILE + 1
        IF ( NBFILE .GT. MXFILE ) THEN
          CALL UTMESS('F','ASOPEN','NOMBRE D''UNITES LOGIQUES'
     &         //' OUVERTES SUPERIEUR A 100')
        ENDIF
        UNITFI(NBFILE) = UNIT
      ELSE IF ( UNIT .LT. 0 ) THEN
        DO 20 I = 1 , NBFILE
          IF ( UNITFI(I) .EQ. -UNIT ) THEN
            UNITFI(I) = 0
            CLOSE (UNIT=-UNIT)
          ENDIF
 20     CONTINUE
      ENDIF
C
 9999 CONTINUE
      END
