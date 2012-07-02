      SUBROUTINE MDGEP4 (NEQ,NBEXCI,PSIDEL,TEMPS,NOMFON,IDDL,REP)
      IMPLICIT NONE
      REAL*8                        PSIDEL(NEQ,*),TEMPS,     REP
      CHARACTER*8                                NOMFON(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C    MULTI-APPUIS :
C    CONVERSION LES DDL GENERALISES EN BASE PHYSIQUE : CONTRIBUTION 
C    DES DEPLACEMENTS DIFFERENTIELS DES ANCRAGES
C-----------------------------------------------------------------------
C IN  : NEQ    : NB D'EQUATIONS DU SYSTEME ASSEMBLE
C IN  : NBEXCI : NOMBRE D'ACCELERO DIFFERENTS
C IN  : PSIDEL : VALEUR DU VECTEUR PSI*DELTA
C IN  : TEMPS  : INSTANT DE CALCUL DES DEPL_IMPO
C IN  : NOMFON : NOM DE LA FONCTION DEPL_IMPO
C IN  : IDDL   : NUMERO DU DDL TRAITE
C OUT : REP    : VALEUR DE PSIDEL*VALE_NOMFOM(TEMPS)
C .________________.____.______________________________________________.
      CHARACTER*8  NOMPAR, BLANC
      CHARACTER*24 VALK
      REAL*8       COEF
C
C-----------------------------------------------------------------------
      INTEGER IDDL ,IER ,IEX ,NBEXCI ,NEQ 
C-----------------------------------------------------------------------
      BLANC  = '        '
      NOMPAR = 'INST'
      REP    = 0.D0
      DO 10 IEX = 1,NBEXCI
         IF ( NOMFON(IEX) .EQ. BLANC ) THEN
            VALK = 'CHARGE EN MONO APPUI'
            CALL U2MESG('A', 'ALGORITH13_44',1,VALK,0,0,0,0.D0)
            GOTO 10
         ENDIF
         CALL FOINTE('F ',NOMFON(IEX),1,NOMPAR,TEMPS,COEF,IER)
         REP = REP + PSIDEL(IDDL,IEX)*COEF
 10   CONTINUE
      END
