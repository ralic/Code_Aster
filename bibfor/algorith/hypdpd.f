      SUBROUTINE HYPDPD(C11,C22,C12,K,C10,C01,C20,DSIDEP,CODRET)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
      REAL*8 C11,C22,C12
      REAL*8 K
      REAL*8 C10,C01,C20
      REAL*8 DSIDEP(6,6)
      INTEGER CODRET
C
C ----------------------------------------------------------------------
C
C LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI
C
C D_PLAN - CALCUL DE LA MATRICE TANGENTE
C
C ----------------------------------------------------------------------
C
C IN  C11,C22,C12: ELONGATIONS
C IN  C10,C01,C20: CARACTERISTIQUES MATERIAUX
C IN  K      : MODULE DE COMPRESSIBILITE
C OUT DSIDEP : MATRICE TANGENTE
C OUT CODRET : CODE RETOUR ERREUR INTEGRATION (1 SI PROBLEME, 0 SINON)
C
C ----------------------------------------------------------------------
C
      REAL*8 T1,T2,T3,T5,T6,T8
      REAL*8 T12,T13,T15,T16,T22,T28
      REAL*8 T34,T51
      REAL*8 II1,II2,II3
      REAL*8 T10,T17,T31,T36,T47,T50,T20,T24,T42,T60,T62,T63,T18
      REAL*8 T35,T52,T55,T45,T56,T59,T11,T14,T39,T53,T54
C
C ----------------------------------------------------------------------
C
C
C --- INVARIANTS
C
      II1=C11+C22+1
      II2=C11*C22+C11+C22-C12**2
      II3=C11*C22-C12**2
      IF ((II3.LE.0.D0)) THEN
        CODRET=1
        GOTO 10

      ENDIF
      T1=II3**(1.D0/3.D0)
      T3=1.D0/T1/II3
      T6=II3**2
      T10=C22**2
      T13=-2.D0/3.D0*T3*C22+4.D0/9.D0*II1/T1/T6*T10
      T17=T1**2
      T31=1.D0/T1
      T36=(T31-II1*T3*C22/3.D0)**2.D0
      T47=SQRT(II3)
      T50=T47**2
C
      DSIDEP(1,1)=4.D0*C10*T13+4.D0*C01*
     &            (-4.D0/3.D0*(C22+1.D0)/T17/II3*C22+
     &            10.D0/9.D0*II2/T17/T6*T10)+8.D0*C20*T36+
     &            8.D0*C20*(II1*T31-3.D0)*T13+K/II3*T10-
     &            K*(T47-1.D0)/T50/T47*T10

      T1=II3**(1.D0/3.D0)
      T3=1.D0/T1/II3
      T8=II3**2
      T12=C11*C22
      T15=II1*T3
      T17=-T3*C22/3.D0-T3*C11/3.D0+4.D0/9.D0*II1/T1/T8*T12-T15/3.D0
      T20=T1**2
      T24=1.D0/T20/II3
      T42=1.D0/T1
      T60=SQRT(II3)
      T62=K*(T60-1.D0)
      T63=T60**2
C
      DSIDEP(1,2)=4.D0*C10*T17+4.D0*C01*
     &            (1.D0/T20-2.D0/3.D0*(C11+1.D0)*T24*C22-
     &            2.D0/3.D0*(C22+1.D0)*T24*C11+
     &            10.D0/9.D0*II2/T20/T8*T12-2.D0/3.D0*II2*T24)+
     &            8.D0*C20*(T42-T15*C22/3.D0)*(T42-T15*C11/3.D0)+
     &            8.D0*C20*(II1*T42-3.D0)*T17+K/II3*T12-
     &            T62/T63/T60*C11*C22+2.D0*T62/T60

      T1=II3**(1.D0/3.D0)
      T3=1.D0/T1/II3
      T6=II3**2
      T10=C22*C12
      T13=2.D0/3.D0*T3*C12-8.D0/9.D0*II1/T1/T6*T10
      T16=T1**2
      T18=1.D0/T16/II3
      T34=1.D0/T1
      T35=II1*T3
      T52=SQRT(II3)
      T55=T52**2
C
      DSIDEP(1,4)=4.D0*C10*T13+4.D0*C01*
     &            (4.D0/3.D0*C12*T18*C22+4.D0/3.D0*(C22+1.D0)*T18*C12-
     &            20.D0/9.D0*II2/T16/T6*T10)+
     &            16.D0/3.D0*C20*(T34-T35*C22/3.D0)*T35*C12+
     &            8.D0*C20*(II1*T34-3.D0)*T13-2.D0*K/II3*T10+
     &            2.D0*K*(T52-1.D0)/T55/T52*C12*C22

      T1=II3**(1.D0/3.D0)
      T3=1.D0/T1/II3
      T8=II3**2
      T12=C11*C22
      T15=II1*T3
      T17=-T3*C22/3.D0-T3*C11/3.D0+4.D0/9.D0*II1/T1/T8*T12-T15/3.D0
      T20=T1**2
      T24=1.D0/T20/II3
      T42=1.D0/T1
      T60=SQRT(II3)
      T62=K*(T60-1.D0)
      T63=T60**2
C
      DSIDEP(2,1)=4.D0*C10*T17+4.D0*C01*
     &            (1.D0/T20-2.D0/3.D0*(C11+1.D0)*T24*C22-
     &            2.D0/3.D0*(C22+1.D0)*T24*C11+
     &            10.D0/9.D0*II2/T20/T8*T12-2.D0/3.D0*II2*T24)+
     &            8.D0*C20*(T42-T15*C22/3.D0)*(T42-T15*C11/3.D0)+
     &            8.D0*C20*(II1*T42-3.D0)*T17+K/II3*T12-
     &            T62/T63/T60*C11*C22+2.D0*T62/T60

      T1=II3**(1.D0/3.D0)
      T3=1.D0/T1/II3
      T6=II3**2
      T10=C11**2
      T13=-2.D0/3.D0*T3*C11+4.D0/9.D0*II1/T1/T6*T10
      T17=T1**2
      T31=1.D0/T1
      T36=(T31-II1*T3*C11/3.D0)**2.D0
      T47=SQRT(II3)
      T50=T47**2
C
      DSIDEP(2,2)=4.D0*C10*T13+4.D0*C01*
     &            (-4.D0/3.D0*(C11+1.D0)/T17/II3*C11+
     &            10.D0/9.D0*II2/T17/T6*T10)+8.D0*C20*T36+
     &            8.D0*C20*(II1*T31-3.D0)*T13+K/II3*T10-
     &            K*(T47-1.D0)/T50/T47*T10

      T1=II3**(1.D0/3.D0)
      T3=1.D0/T1/II3
      T6=II3**2
      T10=C11*C12
      T13=2.D0/3.D0*T3*C12-8.D0/9.D0*II1/T1/T6*T10
      T16=T1**2
      T18=1.D0/T16/II3
      T34=1.D0/T1
      T35=II1*T3
      T52=SQRT(II3)
      T55=T52**2
C
      DSIDEP(2,4)=4.D0*C10*T13+4.D0*C01*
     &            (4.D0/3.D0*C12*T18*C11+4.D0/3.D0*(C11+1.D0)*T18*C12-
     &            20.D0/9.D0*II2/T16/T6*T10)+
     &            16.D0/3.D0*C20*(T34-T35*C11/3.D0)*T35*C12+
     &            8.D0*C20*(II1*T34-3.D0)*T13-2.D0*K/II3*T10+
     &            2.D0*K*(T52-1.D0)/T55/T52*C12*C11


      T1=II3**(1.D0/3.D0)
      T3=1.D0/T1/II3
      T8=II3**2
      T12=1.D0/T1/T8*C12*C22
      T15=T1**2
      T17=1.D0/T15/II3
      T28=C12*C22
      T34=1.D0/T1
      T35=II1*T3
      T45=C20*(II1*T34-3.D0)
      T56=SQRT(II3)
      T59=T56**2
C
      DSIDEP(4,1)=8.D0/3.D0*C10*T3*C12-32.D0/9.D0*C10*II1*T12+
     &            4.D0*C01*(4.D0/3.D0*C12*T17*C22+
     &            4.D0/3.D0*(C22+1.D0)*T17*C12-
     &            20.D0/9.D0*II2/T15/T8*T28)+
     &            16.D0/3.D0*C20*(T34-T35*C22/3.D0)*T35*C12+
     &            16.D0/3.D0*T45*T3*C12-64.D0/9.D0*T45*II1*T12-
     &            2.D0*K/II3*T28+2.D0*K*(T56-1.D0)/T59/T56*C12*C22

      T1=II3**(1.D0/3.D0)
      T3=1.D0/T1/II3
      T8=II3**2
      T12=1.D0/T1/T8*C12*C11
      T15=T1**2
      T17=1.D0/T15/II3
      T28=C12*C11
      T34=1.D0/T1
      T35=II1*T3
      T45=C20*(II1*T34-3.D0)
      T56=SQRT(II3)
      T59=T56**2
C
      DSIDEP(4,2)=8.D0/3.D0*C10*T3*C12-32.D0/9.D0*C10*II1*T12+
     &            4.D0*C01*(4.D0/3.D0*C12*T17*C11+
     &            4.D0/3.D0*(C11+1.D0)*T17*C12-
     &            20.D0/9.D0*II2/T15/T8*T28)+
     &            16.D0/3.D0*C20*(T34-T35*C11/3.D0)*T35*C12+
     &            16.D0/3.D0*T45*T3*C12-64.D0/9.D0*T45*II1*T12-
     &            2.D0*K/II3*T28+2.D0*K*(T56-1.D0)/T59/T56*C12*C11


      T1=C10*II1
      T2=II3**2
      T3=II3**(1.D0/3.D0)
      T5=1.D0/T3/T2
      T6=C12**2
      T11=1.D0/T3/II3
      T14=T3**2
      T18=1.D0/T14/II3
      T22=1.D0/T14/T2
      T31=II1**2
      T39=C20*(II1/T3-3.D0)
      T51=SQRT(II3)
      T53=K*(T51-1.D0)
      T54=T51**2
C
      DSIDEP(4,4)=64.D0/9.D0*T1*T5*T6+8.D0/3.D0*T1*T11+
     &            4.D0*C01*(-2.D0/T14-16.D0/3.D0*T6*T18+
     &            40.D0/9.D0*II2*T22*T6+4.D0/3.D0*II2*T18)+
     &            32.D0/9.D0*C20*T31*T22*T6+128.D0/9.D0*T39*II1*T5*T6+
     &            16.D0/3.D0*T39*II1*T11+4.D0*K/II3*T6-
     &            4.D0*T53/T54/T51*T6-4.D0*T53/T51
   10 CONTINUE
      END
