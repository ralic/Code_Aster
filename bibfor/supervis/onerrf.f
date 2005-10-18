      SUBROUTINE ONERRF(SET, GET, LONG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 17/10/2005   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT NONE
C     ----------------------------------------------------------------
C     QUE FAIT-ON EN CAS D'ERREUR <F> ?
C        VEXCF = 0 == ARRET AVEC ABORT
C        VEXCF = 1 == EXCEPTION FATALERROR
C
C     POUR DEFINIR LE COMPORTEMENT EN CAS D'ERREUR : SET EN IN
C     POUR RECUPERER LA VALEUR COURANTE : SET=' ', GET, LONG EN OUT
C
C IN  SET : VALEUR DU NOUVEAU COMPORTEMENT (ABORT OU EXCEPTION)
C OUT GET : VALEUR ACTUELLE DU COMPORTEMENT
C OUT LEN : LONGUEUR DE GET EN SORTIE
C     ----------------------------------------------------------------
      CHARACTER*(*)     SET
      CHARACTER*16           GET
      INTEGER                     LONG
C     ----------------------------------------------------------------
      INTEGER          LXLGUT
      INTEGER          VEXCF
      SAVE             VEXCF
      DATA VEXCF       /0/
C     ----------------------------------------------------------------
      GET = SET
C --- L'INITIALISATION EST FAITE PAR UN PREMIER APPEL AVEC SET<>' '

C --- ON RECUPERE LA VALEUR ACTUELLE
      IF     (SET(1:1) .EQ. ' ') THEN
C        ON RETOURNE LA VALEUR COURANTE
         IF     (VEXCF .EQ. 0) THEN
            GET='ABORT'
         ELSEIF (VEXCF .EQ. 1) THEN
            GET='EXCEPTION'
         ELSE
            WRITE(6,*) 'ERREUR DE PROGRAMMATION : ONERRF NUMERO 1'
            CALL JEFINI('ERREUR')
         ENDIF

C --- ON POSITIONNE LA VALEUR
      ELSEIF (SET(1:5) .EQ. 'ABORT') THEN
C        ON FERA UN "CALL ABORT" EN CAS D'ERREUR <F>
         VEXCF=0

      ELSEIF (SET(1:9) .EQ. 'EXCEPTION') THEN
C        ON LEVERA UNE EXCEPTION "FATALERROR" EN CAS D'ERREUR <F>
         VEXCF=1

      ELSE
         WRITE(6,*) 'ERREUR DE PROGRAMMATION : ONERRF NUMERO 2'
         CALL JEFINI('ERREUR')
      ENDIF
      LONG = LXLGUT(GET)
      END
