      SUBROUTINE IBIMPR ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 28/03/2001   AUTEUR CIBHHLV L.VIVAN 
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
C     DEFINITION DES UNITES LOGIQUES DES IMPRESSIONS
C     ------------------------------------------------------------------
C OUT IER     : IS  : CODE RETOUR D'EXECUTION
C       = 0 ==> PAS DE PROBLEME
C      /= 0 ==> PROBLEME A LA DEFINITION  DES UNITES LOGIQUES
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         DEFUFI
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C
C     ------- COMMUN DEBUG SUPERVISEUR ---------------------------------
C     ------------------------------------------------------------------
C
C     --- VARIABLES LOCALES --------------------------------------------
      PARAMETER   ( MXIMPR = 13)
      CHARACTER*16    MOTFAC, NOMRES, CONCEP, NOMCMD
      CHARACTER*16    PRNOM (MXIMPR)
      INTEGER         PRUNIT, PLACE, PASSE
      SAVE                           PASSE
C     ------------------------------------------------------------------
      CHARACTER*16  NOMPR (MXIMPR)
      INTEGER       UNITPR (MXIMPR)   , PRESPR(MXIMPR)
      DATA          NOMPR  /'VIGILE'  , 'MESSAGE'   , 'RESULTAT',
     +                      'ERREUR'  , 'COMMANDE'  , 'SEISME'  ,
     +                      'GNUPLOT' , 'POSTSCRIPT', 'AGRAF'   ,
     +                      'ASTER'   , 'IDEAS'     , 'CASTEM'  ,
     +                      'MED'     /
      DATA          UNITPR /    0     ,      6      ,     8     ,
     +                          9     ,     21      ,    22     ,
     +                         23     ,     24      ,    25     ,
     +                         26     ,     30      ,    37     ,
     +                         80     /
      DATA          PASSE  /    0     /
C     ------------------------------------------------------------------
      IER = 0
C
C     --- RECUPERATION DU NOM DE LA COMMANDE UTILISATEUR ---
      CALL GETRES( NOMRES , CONCEP , NOMCMD )
C
C     --- ACTUALISATION DES COMPTEURS DE PRESENCE ---
      PASSE = PASSE + 1
      DO 4 IMPR = 1, MXIMPR
         PRESPR(IMPR) = 0
   4  CONTINUE
C
C     --- DEFINITION EVENTUELLE DES OPTIONS PAR DEFAUTS ---
      IF ( PASSE .EQ. 1) THEN
        DO 5 IMPR = 1, MXIMPR
           CALL DEFUFI( UNITPR(IMPR) , NOMPR(IMPR) )
   5    CONTINUE
      ENDIF
C
C     --- NOMBRE DE CATALOGUES SPECIFIES PAR L'UTILISATEUR ---
      MOTFAC = 'IMPRESSION'
      CALL GETFAC(MOTFAC,NBOCC)
C
      IUN = 1
      DO 20 IOCC = 1, NBOCC
         CALL GETVTX(MOTFAC,'FICHIER' ,IOCC,IUN,MXIMPR,PRNOM,NBNOM)
         IF ( NBNOM.LT.0 ) THEN
            IER = IER + 1
            CALL UTMESS('F',NOMCMD,'TROP DE NOMS DEFINIS DANS LA LISTE'
     +                            //' ARGUMENT DE "FICHIER"')
         ENDIF
         DO 15 INOM = 1, NBNOM
            CALL LXCAPS( PRNOM(INOM) )
            CALL LXCADR( PRNOM(INOM) )
            CALL UTREMT( PRNOM(INOM), NOMPR , MXIMPR, PLACE )
            IF (PLACE.EQ.0) THEN
               IER = IER + 1
               CALL UTDEBM('E',NOMCMD,'NOM SYMBOLIQUE ERRONE '//
     +                         'POUR UN FICHIER DE SORTIE.')
     +
               CALL UTIMPK('L','VALEUR LUE',1,PRNOM(INOM))
               CALL UTIMPK('L','VALEURS ATTENDUES',MXIMPR,NOMPR)
               CALL UTFINM()
            ELSEIF ( PRESPR(PLACE) .NE. 0 ) THEN
               IER = IER + 1
               CALL UTMESS('E',NOMCMD,
     +                          PRNOM(INOM)//' EST DEJA (RE-) DEFINI')
               PRNOM(INOM) = '::'
            ELSE
               PRESPR(PLACE) = 1
            ENDIF
  15     CONTINUE
         CALL GETVIS(MOTFAC,'UNITE',IOCC,IUN,IUN,PRUNIT,NBUNIT)
C
C        --- DEFINITION DES UNITES LOGIQUES DES IMPRESSIONS ---
         DO 10 IMPR = 1, NBNOM
            CALL DEFUFI( PRUNIT , PRNOM(IMPR) )
  10     CONTINUE
  20  CONTINUE
C
      END
