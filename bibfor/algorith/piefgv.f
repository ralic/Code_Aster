      SUBROUTINE PIEFGV(NDIM, TYPMOD, TAU, MAT, NONLOC, VIM,
     &                  EPSP, EPSD, A0, A1, A2, A3, ETAS)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/05/2007   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8        TYPMOD(2)
      INTEGER            NDIM, MAT
      REAL*8             NONLOC(4),EPSP(6), EPSD(6), TAU, ETAS
      REAL*8             VIM(2),A0, A1, A2, A3
C ----------------------------------------------------------------------
C     PILOTAGE LOI DE COMPORTEMENT ELASTIQUE FRAGILE ENDO_FRAGILE 
C     EN NON LOCAL GRAD_VARI
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  TAU     : 2ND MEMBRE DE L'EQUATION F(ETA)=TAU
C IN  MAT   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  EPSP    : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
C IN  EPSD    : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
C OUT A0      : LINEARISATION DU CRITERE : FEL = A0 + A1*ETA
C OUT A1      : IDEM A0
C OUT A2      : IDEM A0 POUR LA 2E SOLUTION EVENTUELLE. R8VIDE SINON
C OUT A3      : IDEM A1 POUR LA 2E SOLUTION EVENTUELLE. R8VIDE SINON
C OUT ETAS    : SI PAS DE SOLUTION : LE MINIMUM. R8VIDE SINON
C ----------------------------------------------------------------------

      LOGICAL     CPLAN
      INTEGER     NDIMSI, KL, NRAC
      REAL*8      TREPSP, TREPSD, COPLAN, SIGELP(6), SIGELD(6)
      REAL*8      KRON(6)
      REAL*8      DM,DTAU,GM,GTAU,P0, P1, P2, ETA, RAC(2), WREL, S
      REAL*8      NU, LAMBDA, DEUXMU, GAMMA, SY, KG,RPEN, PHIP, PHID

      CHARACTER*2 K2(4)
      CHARACTER*8 NOM(4)
      REAL*8      VAL(4)

      REAL*8      DDOT

      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ----------------------------------------------------------------------




C -- OPTION ET MODELISATION

      CPLAN = (TYPMOD(1).EQ.'C_PLAN  ')
      NDIMSI = 2*NDIM


C -- CAS DE L'ENDOMMAGEMENT SATURE

      IF (NINT(VIM(2)).EQ.2 .OR. VIM(1)+TAU .GE. 1) THEN
C      IF (NINT(VIM(2)).EQ.2) THEN
        A0 = 0.D0
        A1 = 0.D0
        A2 = 0.D0
        A3 = 0.D0
        GOTO 9999
      END IF


C -- LECTURE DES CARACTERISTIQUES THERMOELASTIQUES

      NOM(1) = 'E'
      NOM(2) = 'NU'
      NOM(3) = 'SY'
      NOM(4) = 'D_SIGM_EPSI'
      CALL RCVALA(MAT,' ','ELAS'     ,0,' ',0.D0,
     &             2,NOM(1),VAL(1),K2,'F ')
      CALL RCVALA(MAT,' ','ECRO_LINE',0,' ',0.D0,
     &             2,NOM(3),VAL(3),K2,'F ')

      NU     = VAL(2)
      LAMBDA = VAL(1)*VAL(2) / (1-2*VAL(2)) / (1+VAL(2))
      DEUXMU = VAL(1) / (1.D0+VAL(2))
      GAMMA  = -VAL(4)/VAL(1)
      KG     = VAL(3)**2 / (2*VAL(1)) * (1+GAMMA)**2

      RPEN=NONLOC(2)
      PHIP=NONLOC(3)
      PHID=NONLOC(4)

C -- CALCUL DES DEFORMATIONS EN PRESENCE DE CONTRAINTES PLANES

      IF (CPLAN) THEN
        COPLAN  = - NU/(1.D0-NU)
        EPSP(3)  = COPLAN * (EPSP(1)+EPSP(2))
        EPSD(3)  = COPLAN * (EPSD(1)+EPSD(2))
      END IF



C ======================================================================
C                CALCUL DES DEFORMATIONS POUR LINEARISATION
C ======================================================================

      DM   = VIM(1)
      DTAU = MIN(1+GAMMA/2, DM+TAU)
      GM   = RPEN*DM   + KG/(1+GAMMA-DM  )**2
      GTAU = RPEN*DTAU + KG/(1+GAMMA-DTAU)**2      
      WREL = (GTAU - GM)/TAU
      S    = GM / WREL
      

C    COEFFICIENTS DE LA FORME QUADRATIQUE DU CRITERE
      TREPSP = EPSP(1)+EPSP(2)+EPSP(3)
      TREPSD = EPSD(1)+EPSD(2)+EPSD(3)
      DO 60 KL=1,NDIMSI
        SIGELP(KL) = LAMBDA*TREPSP*KRON(KL) + DEUXMU*EPSP(KL)
        SIGELD(KL) = LAMBDA*TREPSD*KRON(KL) + DEUXMU*EPSD(KL)
 60   CONTINUE

      P0 = (0.5D0 * DDOT(NDIMSI,EPSP,1,SIGELP,1) + PHIP  ) / WREL
      P1 = (        DDOT(NDIMSI,EPSP,1,SIGELD,1) + PHID  ) / WREL
      P2 = (0.5D0 * DDOT(NDIMSI,EPSD,1,SIGELD,1)            ) / WREL


C    RECHERCHE DES INTERSECTIONS ELLIPSE / DROITE
      CALL ZEROP2(P1/P2, (P0-S-TAU)/P2, RAC, NRAC)

C    PAS DE SOLUTION : ARRET DE L'ALGORITHME
      IF (NRAC .EQ. 0) THEN
        ETAS = 0.D0

C    UNE OU DEUX SOLUTIONS : ON LINEARISE AUTOUR DES DEUX
      ELSE IF (NRAC.EQ.1) THEN
        ETA=RAC(1)
        A1 = 2*P2*ETA+P1
        A0 = TAU - A1*ETA
C       WRITE(6,*)(6,*)
C        WRITE(6,*) 'ETA1=',ETA
C        WRITE(6,*)
      ELSE
        ETA=RAC(1)
        A1 = 2*P2*ETA+P1
        A0 = TAU - A1*ETA
        ETA=RAC(2)
        A3 = 2*P2*ETA+P1
        A2 = TAU - A3*ETA
C        WRITE(6,*)
C        WRITE(6,*) 'ETA1=',RAC(1)
C        WRITE(6,*)
C        WRITE(6,*)
C        WRITE(6,*) 'ETA2=',RAC(2)
C        WRITE(6,*)
      ENDIF

 9999 CONTINUE

C      WRITE(6,*) 'EPSP(1)=',EPSP(1)
C      WRITE(6,*) 'EPSP(2)=',EPSP(2)
C      WRITE(6,*) 'EPSP(3)=',EPSP(3)
C      WRITE(6,*) 'EPSP(4)=',EPSP(4)

C      WRITE(6,*) 'EPSP(7)=',PHIP

C      WRITE(6,*) 'EPSD(1)=',EPSD(1)
C      WRITE(6,*) 'EPSD(2)=',EPSD(2)
C      WRITE(6,*) 'EPSD(3)=',EPSD(3)
C      WRITE(6,*) 'EPSD(4)=',EPSD(4)

C      WRITE(6,*) 'EPSD(7)=',PHID
    
C      WRITE(6,*) 'A0=',A0
C      WRITE(6,*) 'A1=',A1
C      WRITE(6,*) 'A2=',A2
C      WRITE(6,*) 'A3=',A3

      END
