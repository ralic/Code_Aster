      SUBROUTINE BRE1BL(K0,K1,K2,ETA1,ETA2,E1I,E2I,A,T,B,PW,E1F)

C    ROUTINE ANCIENNEMENT NOMMEE FLE1BL

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2009   AUTEUR DEBONNIERES P.DE-BONNIERES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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

C     CALCUL DE E2F AVEC HYPOTHESE D ECOULEMENT ETAGE 2   

      IMPLICIT REAL*8(A-Z)


      T2 = K1 + K0
      T5 = T / ETA1 * T2
      T6 = EXP(-T5)
      T7 = PW * K1
      T8 = PW * K0
      T10 = K0 * E2I * K1
      T11 = K0 ** 2
      T12 = E2I * T11
      T13 = K0 * A
      T14 = ETA1 * T13
      T16 = K0 * B * K1
      T17 = B * T11
      T18 = K1 ** 2
      T31 = EXP(T5)
      T38 = T2 ** 2
      E1F = -0.1D1 / (T18 + 0.2D1 * K0 * K1 + T11) * (T7 +
     # T8 - T10 - T12 - T14 + T16 + T17 - T18 * E1I - 0.2D1 * E1I * K0 *
     # K1 - T11 * E1I) * T6 + 0.1D1 / T38 * T6 * (T7 + T8 - T10 - T12 + 
     #T * K1 * T13 + T * A * T11 - T14 + T16 + T17) * T31

      END
