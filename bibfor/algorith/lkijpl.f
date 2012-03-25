        SUBROUTINE LKIJPL(NMAT,MATER,SIGF,NR,DRDY,DSDE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C       ----------------------------------------------------------------
C       MATRICE COHERENTE DE LETK A T+DT
C       IN  NMAT   :  DIMENSION MATER
C           MATER  :  COEFFICIENTS MATERIAU
C           SIGF   :  ETAT DE CONTRAINTES A T+DT
C           NR     :  DIMENSION MATRICE JACOBIENNE
C           DRDY   :  MATRICE JACOBIENNE (NR*NR)
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       ----------------------------------------------------------------
        INTEGER         NMAT,NR
        REAL*8          DSDE(6,6),MATER(NMAT,2)
        REAL*8          DRDY(NR,NR),SIGF(6)
C
        INTEGER         I,J,IRET,IER,NDT,NDI
        REAL*8          JSS(6,6),JSZ(6,3),JZS(3,6),JZZ(3,3)
        REAL*8          HOOK(6,6),HOOKNL(6,6),I1,COEFNL
        REAL*8          PATM,NELAS,INVJZZ(3,3),J3X6(3,6)
        REAL*8          DET,J6X6(6,6),DIJACO(6,6),INVDIJ(6,6)
        REAL*8          MAXI,MINI,R8PREM,MUE,MU

C       --------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       --------------------------------------------------------------
C === =================================================================
C --- INITIALISATION MATRICES A ZERO
C === =================================================================
        CALL LCINMA(0.D0,JSS)
        CALL R8INIR(18,0.D0,JSZ,1)
        CALL R8INIR(18,0.D0,JZS,1)
        CALL R8INIR(9,0.D0,JZZ,1)
C === =================================================================
C --- RECHERCHE DU MAXIMUM DE DRDY
C === =================================================================
        MAXI = 0.D0
        DO 1 I = 1, NR
          DO 2 J = 1, NR
            IF(ABS(DRDY(I,J)).GT.MAXI)MAXI = ABS(DRDY(I,J))
  2     CONTINUE
  1     CONTINUE
C === =================================================================
C --- DIMENSIONNEMENT A R8PREM
C === =================================================================
        MINI = R8PREM()*MAXI
        DO 3 I = 1, NR
          DO 4 J = 1, NR
            IF(ABS(DRDY(I,J)).LT.MINI)DRDY(I,J) = 0.D0
  4     CONTINUE
  3     CONTINUE

C === =================================================================
C --- SEPARATION DES TERMES DU JACOBIEN
C === =================================================================
        DO 5 I = 1, NDT
          DO 6 J = 1, NDT
            JSS(I,J) = DRDY(I,J)
  6       CONTINUE
  5     CONTINUE  

        DO 7 I = 1, 3
          DO 8 J = 1,NDT
            JSZ(J,I)=DRDY(J,NDT+I)
            JZS(I,J)=DRDY(NDT+I,J)
  8       CONTINUE
  7     CONTINUE

        DO 9 I = 1, 3
          DO 10 J = 1, 3
            JZZ(I,J) = DRDY(NDT+I,NDT+J)
 10       CONTINUE 
  9     CONTINUE
C === =================================================================
C --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
C === =================================================================
        CALL LCOPLI ( 'ISOTROPE', '3D      ', MATER, HOOK)
C --- PRISE EN COMPTE DU TERME NON LINEAIRE E(I1+) = E0*(I1+/PA)**NE
C --- AVEC I1 = -TRACE(SIGMA), CAR EQUATIONS DU MODELE LETK
C --- SONT EXPRIMEES EN CONVENTION MECANIQUE DES SOLS 
        I1      = -(SIGF(1)+SIGF(2)+SIGF(3))
        PATM    = MATER(1,2)
        NELAS   = MATER(2,2)
        COEFNL  = (I1/(3.D0*PATM))**NELAS

        MUE = MATER(4,1)
        MU  = -MUE*COEFNL

C === =================================================================
C --- MISE A L'ECHELLE DU NUMERATEUR DR(1:6)/DEPS 
C === =================================================================
        COEFNL = COEFNL/MU

        CALL LCPRSM(COEFNL,HOOK,HOOKNL)
C === =================================================================
C --- CONSTRUCTION TENSEUR CONSTITUTIF TANGENT DSDE 
C === =================================================================
C --- INVERSION DU TERME JZZ
        CALL R8INIR(9,0.D0,INVJZZ,1)
        DO 11 I=1,3
          INVJZZ(I,I) = 1.D0
 11     CONTINUE

        CALL MGAUSS('NCVP',JZZ, INVJZZ, 3, 3, 3, DET, IRET)
        IF(IRET.GT.0)CALL R8INIR(9,0.D0,INVJZZ,1)

C --- PRODUIT DU TERME (JZZ)^-1*JZS = J3X6
        CALL PRMAMA(1,INVJZZ,3,3,3,JZS,3,3,NDT,J3X6,3,3,NDT,IER)
        IF(IER.GT.0)WRITE(6,*)'ECHEC AVEC PRMAMA 1'

C --- PRODUIT DU TERME JSZ*(JZZ)^-1*JZS = JSZ*J3*6 = J6X6
        CALL PRMAMA(1,JSZ,6,NDT,3,J3X6,3,3,NDT,J6X6,6,NDT,NDT,IER)
        IF(IER.GT.0)WRITE(6,*)'ECHEC AVEC PRMAMA 2'

C --- DIFFERENCE DE MATRICE (JSS - J6X6) = DIJACO
        CALL LCDIMA(JSS,J6X6,DIJACO)

C --- INVERSION DU TERME (DIJACO)^-1 = INVDIJ
        CALL LCINMA(0.D0,INVDIJ)
        DO 12 I=1,NDT
          INVDIJ(I,I) = 1.D0
 12     CONTINUE
        CALL MGAUSS('NCVP',DIJACO, INVDIJ, 6, NDT, NDT, DET, IRET)
        IF(IRET.GT.1)CALL LCEQMA(HOOK,DSDE)

C --- CONSTRUCTION DSDE = INVDIJ*HOOKNL
        CALL LCPRMM(INVDIJ,HOOKNL,DSDE)

        END
