      SUBROUTINE PRINIT(NBVAL,IUNIT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBVAL,IUNIT(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 10/12/2001   AUTEUR VABHHTS J.PELLET 
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
C     INITIALISATION DES UNITES LOGIQUES OUVERTES POUR LES IMPRESSIONS
C     ------------------------------------------------------------------
C
C IN  NBVAL : IS : NOMBRE D'UNITE LOGIQUE A AFFECTER
C        SI < 0 ALORS ON PREND LES UNITES "VIGILE" "MESSAGES" "RESULTAT"
C
C     --- COMMUN D'IMPRESSIONS SUPERVISEUR -----------------------------
C     UNIT  : IS     : TABLEAU DES UNITES LOGIQUES ACTIVES
C     NBUNIT: CH*(*) : NOMBRE D'UNITES LOGIQUES ACTIVES
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         IUNIFI
C     ROUTINE(S) FORTRAN     :
C         WRITE   LEN
C     ------------------------------------------------------------------
C FIN PRINIT
C     ------------------------------------------------------------------
C
      PARAMETER          ( MXUNIT = 10)
      INTEGER         UNIT(MXUNIT), NBUNIT
      COMMON /PRCN00/ UNIT   , NBUNIT , NBCOLS
C     ------------------------------------------------------------------
C
C     --- AFFICHAGE SUR "MXCOLS" COLONNES ------------------------------
      PARAMETER   (MXCOLS = 132 )
      CHARACTER*(MXCOLS)  BLANC, TIRET
      CHARACTER*6                       DEBUT
      CHARACTER*1                              FIN
      COMMON /PRCC00/     BLANC, TIRET, DEBUT, FIN
C     ------------------------------------------------------------------
C
C     --- VARIABLES LOCALES --------------------------------------------
      PARAMETER          ( MXFICH = 4)
      CHARACTER*8 FILE( MXFICH )
      DATA        FILE/'RESULTAT', 'MESSAGE', 'VIGILE ', 'ERREUR' /
C     ------------------------------------------------------------------
C
C     --- INITIALISATIONS DES UNITES LOGIQUES ACTIVES ---
      IF ( NBVAL .LT. 0 ) THEN
         DO 10 I = 1, MXFICH
            UNIT(I) = IUNIFI(FILE(I))
 10      CONTINUE
C
C        --- RE-ORDONNEMENT ---
         DO 20 I = 1, MXFICH
            DO 30 J = I+1, MXFICH
               IF ( UNIT(I) .EQ. UNIT(J) ) UNIT(J) = 0
 30         CONTINUE
 20      CONTINUE
C
         NBUNIT = 0
         DO 40 I = 1, MXFICH
            IF ( UNIT(I) .GT. 0 ) THEN
               NBUNIT  = NBUNIT + 1
               UNIT(NBUNIT) = UNIT(I)
            ENDIF
 40      CONTINUE
C
      ELSE
C
C        --- CAS CLASSIQUE ---
         NBUNIT =  NBVAL
         DO 50 I = 1, NBUNIT
            UNIT(I) = IUNIT(I)
 50      CONTINUE
      ENDIF
C
C     --- INITIALISATIONS DES BLANCS ET TIRETS --
      DEBUT = '    > '
      FIN   = '<'
      DO 60 I = 1, MXCOLS
         BLANC(I:I) = ' '
         TIRET(I:I) = '-'
 60   CONTINUE
C
      END
