      SUBROUTINE HYP3CI(C11   ,C22   ,C33   ,C12   ,C13   ,
     &                  C23   ,C10   ,C01   ,C20   ,SISO  ,
     &                  CODRET)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/03/2010   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      REAL*8  C11,C22,C33
      REAL*8  C12,C13,C23
      REAL*8  C10,C01,C20
      REAL*8  SISO(6)
      INTEGER CODRET
C
C ----------------------------------------------------------------------
C
C LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI    
C 
C 3D - CALCUL DES CONTRAINTES - PARTIE ISOTROPIQUE
C
C ----------------------------------------------------------------------
C
C
C IN  C11,C22,C33,C12,C13,C23 : ELONGATIONS
C IN  C10,C01,C20             : CARACTERISTIQUES MATERIAUX
C OUT SISO   : CONTRAINTES ISOTROPIQUES
C OUT CODRET : CODE RETOUR ERREUR INTEGRATION (1 SI PROBLEME, 0 SINON)
C
C ----------------------------------------------------------------------
C
      REAL*8 GRD(6)
      REAL*8 T1,T3,T5,T7
      REAL*8 T10,T12,T13,T14,T15,T17,T18,T19
      REAL*8 T20,T23,T26,T27,T29
      REAL*8 T33,T40,T43,T46
      REAL*8 T56,T59,T69,T73,T89,T104
C
C ----------------------------------------------------------------------
C
      T1   = C11*C22
      T3   = C23**2
      T5   = C12**2
      T7   = C12*C13
      T10  = C13**2
      T12  = T1*C33-C11*T3-T5*C33+2*T7*C23-T10*C22
      T13  = T12**(1.D0/3.D0)

      IF ((T12.EQ.0.D0).OR.(T13.EQ.0.D0)) THEN
        CODRET = 1
        GOTO 99
      ENDIF

      T14  = 1.D0/T13
      T15  = C11+C22+C33
      T17  = 1.D0/T13/T12
      T18  = T15*T17
      T19  = C22*C33
      T20  = T19-T3
      T23  = T14-T18*T20/3.D0
      T26  = T13**2

      IF ((T15.EQ.0.D0).OR.(T26.EQ.0.D0)) THEN
        CODRET=1
        GOTO 99
      ENDIF

      T27  = 1.D0/T26
      T29  = C11*C33
      T33  = (T1+T29+T19-T5-T10-T3)/T26/T12
      T40  = C20*(T15*T14-3.D0)
      T43  = T29-T10
      T46  = T14-T18*T43/3.D0
      T56  = T1-T5
      T59  = T14-T18*T56/3.D0
      T69  = C10*T15
      T73  = -2.D0*C12*C33+2*C13*C23
      T89  = 2.D0*C12*C23-2*C13*C22
      T104 = -2.D0*C11*C23+2*T7

      GRD(1) = C10*T23+
     &         C01*((C22+C33)*T27-2.D0/3.D0*T33*T20)+
     &         2.D0*T40*T23
      GRD(2) = C10*T46+
     &         C01*((C11+C33)*T27-2.D0/3.D0*T33*T43)+
     &         2.D0*T40*T46
      GRD(3) = C10*T59+
     &         C01*((C11+C22)*T27-2.D0/3.D0*T33*T56)+
     &         2.D0*T40*T59
      GRD(4) = -T69*T17*T73/3.D0+
     &         C01*(-2.D0*C12*T27-2.D0/3.D0*T33*T73)-
     &         2.D0/3.D0*T40*T18*T73
      GRD(5) = -T69*T17*T89/3.D0+
     &         C01*(-2.D0*C13*T27-2.D0/3.D0*T33*T89)-
     &         2.D0/3.D0*T40*T18*T89
      GRD(6) = -T69*T17*T104/3.D0+
     &         C01*(-2.D0*C23*T27-2.D0/3.D0*T33*T104)-
     &         2.D0/3.D0*T40*T18*T104
      SISO(1) = 2.D0*GRD(1)
      SISO(2) = 2.D0*GRD(2)
      SISO(3) = 2.D0*GRD(3)
      SISO(4) = 2.D0*GRD(4)
      SISO(5) = 2.D0*GRD(5)
      SISO(6) = 2.D0*GRD(6)
 99   CONTINUE
      END
