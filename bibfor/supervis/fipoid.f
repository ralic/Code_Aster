      SUBROUTINE FIPOID(NTERM,EXPRES , IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NTERM,EXPRES(*),IER
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
C     VERIFICATION DE L'EXPRESSION POLONAISE PAR LA REGLE DES POIDS
C     ------------------------------------------------------------------
C OUT IER : I : CODE RETOUR  OK  SI = 0
C     ------------------------------------------------------------------
      INTEGER        IVAL,ICLASS,IPRIOR,IARITE,IPOIDS
      REAL*8         RVAL
      COMPLEX*16     CVAL
      CHARACTER*72   KVAL
C     ------------------------------------------------------------------
C
C     LECTURE DE L'ITEM SUIVANT
      IER = 0
      IPOIDS = 0
      DO 100 ITERM = NTERM, 2, -1
         CALL FIEXTR(EXPRES(ITERM),ICLASS,IVAL,RVAL,CVAL,
     +                                              KVAL,IPRIOR,IARITE)
         IPOIDS = IPOIDS + ABS(IARITE) - 1
         IF( IPOIDS.EQ. -1) THEN
           CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPOID.01)',
     +                                           'EXPRESSION ERRONEE')
           IER = 1
           GOTO 9999
         ENDIF
 100  CONTINUE
      CALL FIEXTR(EXPRES(1),ICLASS,IVAL,RVAL,CVAL,KVAL,IPRIOR,IARITE)
      IPOIDS = IPOIDS + ABS(IARITE) - 1
      IF( IPOIDS .NE. -1) THEN
        IER = -1
              CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPOID.02)',
     +                                         'EXPRESSION INCOMPLETE')
      ENDIF
 9999 CONTINUE
      END
