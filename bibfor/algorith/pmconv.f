      SUBROUTINE PMCONV(R,RINI,R1,INST,SIGP,COEF,ITER,INDIMP,
     &                  PARCRI,IRET,ITEMAX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/09/2010   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
C-----------------------------------------------------------------------
C           OPERATEUR    CALC_POINT_MAT CALCUL D'ERREUR ET CONVERGENCE
C-----------------------------------------------------------------------
C IN   R      : RESIDU ACTUEL
C IN   RINI   : RESIDU INITIAL
C IN/OUT R1   : RESIDU PREMIERE ITERATION
C IN   INST   : INSTANT ACTUEL
C IN   SIGP   : CONTRAINTES ACTUELLES (POUR CONSTRUIRE LE DENOMINATEUR)
C IN   COEF   : COEF POUR ADIMENSIONNALISER LE PB
C IN   ITER   : NUMERO D'ITERATION
C IN   PARCRI : PARAMETRES DE CONVERGENCE GLOBAUX
C OUT  IRET   : CODE RETOUR = 0 SI CONVERGENCE 
C OUT  IRET   : CODE RETOUR = 1 SI NON CONVERGENCE ITERATION SUIVANTE
C OUT  IRET   : CODE RETOUR = 2 SI NOMBRE D'ITERATIONS MAXI ATTEINT
C OUT  ITEMAX : NOMBRE D'ITERATIONS MAXI ATTEINT

C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER      IER,NIV,IFM,IND,INDIMP(6)
      INTEGER      IRET,I
      INTEGER      ITMAX,ITER,IRELA
      REAL*8       R8PREM,INST,PARCRI(12)
      REAL*8       R(12),RINI(12),R1(12),SIGP(6),COEF,R8B(12)
      REAL*8       EE,E1,E2,TOLER,E1INI,E2INI,ER1,EINI,R8VIDE
      LOGICAL      ITEMAX
      CHARACTER*8  FONIMP(6)
C-----------------------------------------------------------------------

      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
      
C-----------------------------------------------------------------------
C     VERIFICATION DE LA CONVERGENCE EN DY  ET RE-INTEGRATION ?
C-----------------------------------------------------------------------
      IRET=0            
      E1=0.D0
      E2=0.D0
      E1INI=0.D0
      E2INI=0.D0
      ER1=0.D0
      ITEMAX=.FALSE.
      IF (ITER.EQ.1) THEN
C        SAUVEGARDE DE R(DY0) POUR TEST DE CONVERGENCE
         CALL DCOPY(6,SIGP,1,R1(1),1)
         CALL DSCAL(6,1.D0/COEF,R1(1),1) 
         CALL DCOPY(6,R(7),1,R1(7),1)
         DO 11 I = 1,12
            ER1 = MAX(ER1, ABS(R1(I)))
 11      CONTINUE
         IF (ER1.LE.R8PREM()) THEN
            EE=ER1
            IND=4
            IRET=0
            GOTO 9999
         ENDIF         
      ENDIF
      
      DO 101 I = 1,6
         E1 = MAX(E1, ABS(R(I)))
         E1INI = MAX(E1INI, ABS(RINI(I)))
         E1INI = MAX(E1INI, ABS(R1(I)))
 101  CONTINUE
      DO 102 I = 7,12
         E2 = MAX(E2, ABS(R(I)))
         E2INI = MAX(E2INI, ABS(RINI(I)))
         E2INI = MAX(E2INI, ABS(R1(I)))
 102  CONTINUE
      EINI=MAX(E1INI,E2INI)
      
C     TEST RELATIF OU ABSOLU 
      IF (PARCRI(2).NE.R8VIDE()) THEN
          IRELA=1
      ELSE
          IRELA=0
      ENDIF
      IF (IRELA.EQ.1) THEN
         TOLER=PARCRI(2)
         IF (EINI.GT.R8PREM()) THEN
            E1=E1/EINI
            E2=E2/EINI
            EE=MAX(E1,E2)
            IND=3
         ENDIF
      ELSE
         TOLER=PARCRI(3)
         EE=MAX(E1,E2)
         IND=4
      ENDIF
      ITEMAX=.FALSE.
      ITMAX=NINT(PARCRI(1))

      IF ( ITER .LT. ITMAX ) THEN
C -      NON CONVERGENCE ITERATION SUIVANTE
         IF ( EE .GT. TOLER ) THEN
            IRET=1
         ENDIF
      ELSE
C -      NB ITERATION MAXIMUM ATTEINT SANS CONVERGENCE
         IRET=2
         ITEMAX=.TRUE.
         CALL U2MESS('I','COMPOR2_5')
      ENDIF
 9999 CONTINUE
      IF (NIV.EQ.2) THEN
         CALL PMIMPR(IND,INST,INDIMP,FONIMP,R8B,ITER,
     &               R8B,R8B,R8B,1,R8B,EE,EINI)
      ENDIF
      END
