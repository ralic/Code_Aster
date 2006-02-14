      SUBROUTINE HYPCPC(C11,C22,C33,C12,
     &                  K,C10,C01,C20,NITMAX,EPSI,
     &                  SIG,CODRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/02/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE   
      REAL*8      C11
      REAL*8      C22
      REAL*8      C33
      REAL*8      C12
      REAL*8      K
      REAL*8      C10
      REAL*8      C01
      REAL*8      C20
      INTEGER     NITMAX
      REAL*8      EPSI
      REAL*8      SIG(6)
      INTEGER     CODRET
C      
C-----------------------------------------------------------------------
C
C     LOI DE COMPORTEMENT HYPERELASTIQUE - CONTRAINTES PLANES
C     CALCUL DES CONTRAINTES 
C
C IN  C11,C22,C33,C12: ELONGATIONS
C IN  C10,C01,C20:     CARACTERISTIQUES MATERIAUX
C IN  K     :          MODULE DE COMPRESSIBILITE
C IN  NITMAX:          NOMBRE MAXI D'ITERATIONS 
C IN  EPSI  :          CRITERE DE CONVERGENCE
C OUT SIG   :          CONTRAINTES 
C OUT CODRET:          CODE RETOUR CONVERGENCE COMPORTEMENT
C-----------------------------------------------------------------------
C           
      REAL*8      T1,T3,T5,T6,T8,T12,T13
      REAL*8      T17,T20,T15,T24,T61
      REAL*8      T19,T38,T7,T37,T58,T9,T41,T46
      REAL*8      SN,DC33,DSN,TEST
      INTEGER     ITER,FACMUL
C
C-----------------------------------------------------------------------
C
      FACMUL = 10
      ITER   = 1
  10  CONTINUE    
C
C --- DEBUT DE BOUCLE
C
      T1  = C11*C22
      T3  = C12**2
      T5  = T1*C33-T3*C33
      IF ((T5.LE.0.D0)) THEN
        CALL UTMESS('F','HYPCPC',
     &  'T1*C33-T3*C33 IS ZERO FOR HYPERELASTIC MATERIAL')
      ENDIF   
      T6  = T5**(1.D0/3.D0)
      T7  = 1.D0/T6
      T8  = C11+C22+C33
      T12 = T1-T3
      T15 = T7-T8/T6/T5*T12/3.D0
      T19 = T6**2
      T38 = SQRT(T5)
      SN  = 2.D0*C10*T15+
     &     2.D0*C01*((C11+C22)/T19
     &     -2.D0/3.D0*(T1+C11*C33+C22*C33-T3)/T19/T5*T12)+
     &     4.D0*C20*(T8*T7-3.D0)*T15+K*(T38-1.D0)/T38*T12
      T1  = C11*C22
      T3  = C12**2
      T5  = T1*C33-T3*C33
      T6  = T5**(1.D0/3.D0)
      T8  = 1.D0/T6/T5
      T9  = T1-T3
      T12 = C11+C22+C33
      T13 = T5**2
      T17 = T9**2
      T20 = -2.D0/3.D0*T8*T9+4.D0/9.D0*T12/T6/T13*T17
      T24 = T6**2
      T41 = 1.D0/T6
      T46 = (T41-T12*T8*T9/3.D0)**2.D0
      T58 = SQRT(T5)
      T61 = T58**2
      DSN = 2.D0*C10*T20+
     &      2.D0*C01*(-4.D0/3.D0*(C11+C22)/T24/T5*T9+
     &      10.D0/9.D0*(T1+C11*C33+C22*C33-T3)/T24/T13*T17)+
     &      4.D0*C20*T46+
     &      4.D0*C20*(T41*T12-3.D0)*T20+
     &      K/T5*T17/2.D0-K*(T58-1.D0)/T61/T58*T17/2.D0
     
      IF (DSN.EQ.0.D0) THEN
        CODRET = 1
        GOTO 999
      ELSE
        DC33 = -SN/DSN        
      ENDIF

      C33 = C33+DC33

      ITER = ITER + 1
      IF (ITER.LT.NITMAX*FACMUL) THEN
        TEST = ABS(SN)
        IF (TEST .LT. EPSI) THEN
          CODRET = 0
          GOTO 200
        ELSE
          GOTO 10
        ENDIF  
      ELSE
        CODRET = 1
        GOTO 999
      ENDIF
C
C --- FIN DE BOUCLE
C       
 200  CONTINUE

      T1  = C11*C22
      T3  = C12**2
      T5  = T1*C33-T3*C33
      T6  = T5**(1.D0/3.D0)
      
      IF ((T5.LE.0.D0)) THEN
        CALL UTMESS('F','HYPCPC',
     &  'ZERO ELONGATION FOR HYPERELASTIC MATERIAL')
      ENDIF      
      
      T7  = 1.D0/T6
      T8  = C11+C22+C33
      T12 = C22*C33
      T15 = T7-T8/T6/T5*T12/3.D0
      T19 = T6**2
      T37 = SQRT(T5)
      SIG(1) = 2.D0*C10*T15+
     &         2.D0*C01*((C22+C33)/T19-
     &         2.D0/3.D0*(T1+C11*C33+T12-T3)/T19/T5*T12)+
     &         4.D0*C20*(T8*T7-3.D0)*T15+
     &         K*(T37-1.D0)/T37*C22*C33

      T1  = C11*C22
      T3  = C12**2
      T5  = T1*C33-T3*C33
      T6  = T5**(1.D0/3.D0)
      T7  = 1.D0/T6
      T8  = C11+C22+C33
      T12 = C11*C33
      T15 = T7-T8/T6/T5*T12/3.D0
      T19 = T6**2
      T37 = SQRT(T5)
      
      SIG(2) = 2.D0*C10*T15+
     &         2.D0*C01*((C11+C33)/T19-
     &         2.D0/3.D0*(T1+C22*C33+T12-T3)/T19/T5*T12)+
     &         4.D0*C20*(T8*T7-3.D0)*T15+
     &         K*(T37-1.D0)/T37*C11*C33

      T1  = C11+C22+C33
      T3  = C11*C22
      T5  = C12**2
      T7  = T3*C33-T5*C33
      T8  = T7**(1.D0/3.D0)
      T12 = 1.D0/T8/T7*C12*C33
      T15 = T8**2
      T38 = SQRT(T7)
      SIG(4) = 4.D0/3.D0*C10*T1*T12+
     &         2.D0*C01*(-2.D0*C12/T15+4.D0/3.D0*
     &         (T3+C11*C33+C22*C33-T5)/T15/T7*C12*C33)+
     &         8.D0/3.D0*C20*(T1/T8-3.D0)*T1*T12-
     &         2.D0*K*(T38-1.D0)/T38*C12*C33
      SIG(3) = 0.D0
      SIG(5) = 0.D0
      SIG(6) = 0.D0
      
  999 CONTINUE    
      END
