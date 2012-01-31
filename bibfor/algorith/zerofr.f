      SUBROUTINE ZEROFR(INTINI,ALGO,FUNC,X1,X2,TOL,ITMAX,
     &                  SOLU,IRET,ITER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/01/2012   AUTEUR GENIAUT S.GENIAUT 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT NONE

      INTEGER       INTINI,ITMAX,ITER,IRET
      CHARACTER*(*) ALGO
      REAL*8        SOLU,TOL,X1,X2,FUNC
      EXTERNAL      FUNC
C
C ----------------------------------------------------------------------
C TOLE CRP_7
C     BUT : TROUVER LE ZERO D'UNE FONCTION SCALAIRE REELLE 
C     
C
C IN  INTINI : RECHERCHE DE L'INTERVALLE INITIAL 
C              TEL QUE F CHANGE DE SIGNE
C              = 0 : PAS DE RECHERCHE
C              = 1 : RECHERCHE PAR BRACKETING CROISSANT 
C              = 2 : RECHERCHE PAR BRACKETING CROISSANT A DROITE
C IN  ALGO   : ALGORITHME DE RECHERCHE DU ZERO : 'AUTO', 'SECANTE', 
C              'DEKKER', 'DEKKER2', 'BRENT'
C              SI ALGO VAUT 'AUTO', ON PREND 'BRENT'
C IN  FUNC   : FONCTION F
C IN  X1, X2 : INTERVELLE DE RECHERCHE
C IN  TOL    : PRECISION ABSOLUE : LA SOLUTION X EST TELLE QUE F(X)<TOL
C IN  ITMAX  : NOMBRE D'ITERATIONS MAXIMUM
C OUT SOLU   : ZERO DE F
C OUT IRET   : CODE RETOUR : IRET = 0 : OK
C            :               SINON    : PROBLEME
C OUT ITER   : NOMBRE D'ITERATIONS EFFECTUEES
C ----------------------------------------------------------------------

      CHARACTER*8  ALGOZ
      REAL*8       A,B,FA,FB
      
      ALGOZ = ALGO
      A = X1
      B = X2

C     ------------------------------------------------------------------
C     RECHERCHE DE L'INTERVALLE INITIAL [A,B]
C     ------------------------------------------------------------------
C
      CALL ASSERT(INTINI.EQ.0.OR.
     &            INTINI.EQ.1.OR.
     &            INTINI.EQ.2)

      IF (INTINI.EQ.1) THEN

C       BRACKETING CROISSANT A GAUCHE ET A DROITE
        CALL ENCADR (FUNC,A,B,FA,FB,ITMAX,1.6D0,IRET)
        IF (IRET.NE.0) GOTO 9999

      ELSEIF (INTINI.EQ.2) THEN
          
C       BRACKETING CROISSANT UNIQUEMNT A DROITE :
C       SOUVENT LE CAS POUR LES LOIS DE COMPORTEMENT
C       (SI F EST CROISSANTE ET F(A)<0, OU L'INVERSE),
C       CE QUI PERMET DE PRENDRE UN COEF MULT GRAND (10)
        CALL ENCADR (FUNC,A,B,FA,FB,ITMAX,10.D0,IRET)
        IF (IRET.NE.0) GOTO 9999

      ENDIF

C     ------------------------------------------------------------------
C     RECHERCHE DU ZERO DE F ENTRE X1 ET X2
C     ------------------------------------------------------------------

      IF (ALGOZ.EQ.'AUTO') ALGOZ = 'BRENT'
      

      IF (ALGOZ.EQ.'BRENT') THEN

        CALL ZEROFB(FUNC,A,B,TOL,ITMAX,SOLU,IRET,ITER)

      ELSEIF (ALGOZ.EQ.'SECANTE') THEN
      
        CALL ZEROFC(FUNC,A,B,TOL,ITMAX,SOLU,IRET,ITER)

      ELSEIF (ALGOZ.EQ.'DEKKER') THEN

        CALL ZEROFO(FUNC,A,B,TOL,ITMAX,SOLU,IRET,ITER)

      ELSEIF (ALGOZ.EQ.'DEKKER2') THEN

        CALL ZEROF2(FUNC,A,B,TOL,ITMAX,SOLU,IRET,ITER)

      ELSE

        CALL ASSERT(.FALSE.)

      ENDIF


 9999 CONTINUE
      END
