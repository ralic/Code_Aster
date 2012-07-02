      SUBROUTINE FUN2(XI1, XI2, P, XKK, Q, VT,  N  )
      IMPLICIT NONE
      INTEGER                                   N
      REAL*8          XI1, XI2, P, XKK, Q, VT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C         CALCULE LE MOMENT D'INERTIE EQUIVALENT D'UNE POUTRE DROITE A
C      SECTION VARIABLE SOUS L'HYPOTHESE DE VARIATION LINEAIRE DES
C      COORDONNEES
C     ------------------------------------------------------------------
C                        LISTE DES ARGUMENTS
C    -------------------------------------------------------------------
C    TYPE  !   NOM  !  TABLEAU  !             SIGNIFICATION
C    -------------------------------------------------------------------
C IN R*8   !  XI1   !     -     ! MOMENT D INERTIE SECTION INITIALE
C IN R*8   !  XI2   !     -     ! MOMENT D INERTIE SECTION FINALE
C IN R*8   !   P    !     -     ! COEFF. DE CISAILLEMENT DEPENDANT DU
C IN       !        !           !  MATERIAU (P = E/(G*AS*L*L) )
C IN  I    !   N    !     -     ! TYPE DE CALCUL  (N =1  3 OU 4)
C IN       !        !           !  DEGRE DU POLYNOME DU DENOMINATEUR
C OUT R*8  !  XKK   !     -     ! L'ELEMENT DIAGONAL A E/L3 PRES
C OUT R*8  !   Q    !     -     !
C OUT R*8  !   VT   !     -     !
C    -------------------------------------------------------------------
      REAL*8    ALG,DI,PHI,A,A2,A3,RHO,C
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IF (XI1.EQ.XI2) THEN
          Q   = 0.5D0
          XKK = 12.D0*XI1/(1.D0+P*(12.D0*XI1))
          VT  = XI1
      ELSE
          IF     ( N.LE. 1 ) THEN
             ALG = LOG(XI2/XI1)
             DI  = XI2-XI1
             PHI = P*DI
             Q   = 1.D0/ALG - XI1/DI
             XKK = DI /( (XI2+XI1)/(2.0D0*DI)-1.D0/ALG+PHI)
             VT  = DI/ALG
          ELSEIF ( N.EQ. 2 ) THEN
C            ERREUR
          ELSEIF ( N.EQ. 3 ) THEN
             A   = (XI2/XI1)**(1.D0/3)
             Q   = 1.D0/(A+1.D0)
             A3  = (A-1.D0)**3
             PHI = P*XI1*A3
             RHO = LOG(A)-2.D0*Q*(A-1.D0)+PHI
             XKK = XI1*A3/RHO
             VT  = 2.D0*XI1*A*A/(A+1.D0)
          ELSEIF ( N.GE. 4 ) THEN
             A   = (XI2/XI1)**0.25D0
             A3  = A*A*A
             A2  = (A-1.D0)**2
             PHI = P*XI1*A2
             C   = A3-3.D0*A+2.D0
             Q   = C/(2.D0*(A-1.D0)*(A3-1.D0))
             RHO = A2/(3.D0*A3)-C*Q/(6.D0*A3)+PHI
             XKK = A2*XI1/RHO
             VT  = 3.D0*XI1*A3/(A*A + A + 1.D0)
          ENDIF
      ENDIF
      END
