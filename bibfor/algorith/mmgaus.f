      SUBROUTINE MMGAUS(ALIAS,TYPI,NORD,XPG,YPG,HPG)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/09/2011   AUTEUR MASSIN P.MASSIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT NONE
      CHARACTER*8 ALIAS
      INTEGER     TYPI
      INTEGER     NORD
      REAL*8      XPG
      REAL*8      YPG
      REAL*8      HPG
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (UTILITAIRE)
C
C RETOURNE LES COORDONNEES ET LE POIDS DU POINT D'INTEGRATION
C      
C ----------------------------------------------------------------------
C
C
C IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
C IN  TYPI   : TYPE D'INTEGRATION
C                1 NOEUDS
C               X2 GAUSS (X est l'ordre de la quadrature)
C               Y3 SIMPSON (Y est nombre de partitions du domaine)
C		        Z4 NEWTON-COTES (Z est dégré du polynôme interpolateur)
C IN  NORD   : NUMERO DU POINT D'INTEGRATION
C OUT XPG    : COORDONNEE X DU POINT D'INTEGRATION
C OUT YPG    : COORDONNEE Y DU POINT D'INTEGRATION
C OUT HPG    : POIDS DU POINT D'INTEGRATION
C
C ----------------------------------------------------------------------
C
       INTEGER      PARAM,I,J,K,H,N
       REAL*8       FPG(10,5,2), PNC(8,6), PNCT(6,10)
       REAL*8       FPGT(10,6,3)
C
C ----------------------------------------------------------------------
C

C
C --- INITIALISATIONS
C
C
      DO 10 J= 1,10
       DO 20 I= 1,6
        DO 30 K= 1,3
         FPGT(J,I,K)=0.D0
   30   CONTINUE
   20  CONTINUE
   10 CONTINUE
       

C Points de Gauss (1D)

      FPG(1,1,1) = 0.D0
      FPG(1,1,2) = 2.D0

      FPG(2,1,1) = 0.577350269189626D0
      FPG(2,1,2) = 1.D0

      FPG(3,1,1) = 0.D0
      FPG(3,1,2) = 0.888888888888889D0
      FPG(3,2,1) = 0.774596669241483D0
      FPG(3,2,2) = 0.555555555555556D0

      FPG(4,1,1) = 0.339981043584856D0
      FPG(4,1,2) = 0.652145154862546D0
      FPG(4,2,1) = 0.861136311594053D0
      FPG(4,2,2) = 0.347854845137454D0

      FPG(5,1,1) = 0.D0
      FPG(5,1,2) = 0.568888888888889D0
      FPG(5,2,1) = 0.538469310105683D0
      FPG(5,2,2) = 0.478628670499366D0
      FPG(5,3,1) = 0.906179845938664D0
      FPG(5,3,2) = 0.236926885056189D0

      FPG(6,1,1) = 0.238619186083197D0
      FPG(6,1,2) = 0.467913934572691D0
      FPG(6,2,1) = 0.661209386466265D0
      FPG(6,2,2) = 0.360761573048139D0
      FPG(6,3,1) = 0.932469514203152D0
      FPG(6,3,2) = 0.171324492379170D0

      FPG(7,1,1) = 0.D0
      FPG(7,1,2) = 0.417959183673469D0
      FPG(7,2,1) = 0.405845151377397D0
      FPG(7,2,2) = 0.381830050505119D0
      FPG(7,3,1) = 0.741531185599394D0
      FPG(7,3,2) = 0.279705391489277D0
      FPG(7,4,1) = 0.949107912342759D0
      FPG(7,4,2) = 0.129484966168870D0

      FPG(8,1,1) = 0.183434642495650D0
      FPG(8,1,2) = 0.362683783378362D0
      FPG(8,2,1) = 0.525532409916329D0
      FPG(8,2,2) = 0.313706645877887D0
      FPG(8,3,1) = 0.796666477413627D0
      FPG(8,3,2) = 0.222381034453374D0
      FPG(8,4,1) = 0.960289856497536D0
      FPG(8,4,2) = 0.101228536290376D0

      FPG(9,1,1) = 0.D0
      FPG(9,1,2) = 0.330239355001260D0
      FPG(9,2,1) = 0.324253423403809D0
      FPG(9,2,2) = 0.312347077040003D0
      FPG(9,3,1) = 0.613371432700590D0
      FPG(9,3,2) = 0.260610696402935D0
      FPG(9,4,1) = 0.836031107326636D0
      FPG(9,4,2) = 0.180648160694857D0
      FPG(9,5,1) = 0.968160239507626D0
      FPG(9,5,2) = 0.081274388361574D0

      FPG(10,1,1) = 0.148874338981631D0
      FPG(10,1,2) = 0.295524224714753D0
      FPG(10,2,1) = 0.433395394129247D0
      FPG(10,2,2) = 0.269266719309996D0
      FPG(10,3,1) = 0.679409568299024D0
      FPG(10,3,2) = 0.219086362515982D0
      FPG(10,4,1) = 0.865063366688985D0
      FPG(10,4,2) = 0.149451349150581D0
      FPG(10,5,1) = 0.973906528517172D0
      FPG(10,5,2) = 0.066671344308688D0

C Points de Gauss (domaine triangulaire)

      FPGT(1,1,3) =  0.5D0


      FPGT(2,1,1) =  0.666666666666667D0
      FPGT(2,1,3) =  0.166666666666667D0


      FPGT(3,1,3) = -0.28125D0

      FPGT(3,2,1) =  0.6D0
      FPGT(3,2,3) =  0.260416666666667D0


      FPGT(4,1,1) =  0.108103018168070D0
      FPGT(4,1,3) =  0.111690794839006D0

      FPGT(4,2,1) =  0.816847572980459D0
      FPGT(4,2,3) =  0.054975871827661D0


      FPGT(5,1,3) =  0.1125D0

      FPGT(5,2,1) =  0.059715871789770D0
      FPGT(5,2,3) =  0.066197076394253D0

      FPGT(5,3,1) =  0.797426985353087D0
      FPGT(5,3,3) =  0.062969590272414D0


      FPGT(6,1,1) =  0.501426509658179D0
      FPGT(6,1,3) =  0.058393137863190D0

      FPGT(6,2,1) =  0.873821971016996D0
      FPGT(6,2,3) =  0.025422453185104D0

      FPGT(6,3,1) =  0.053145049844817D0
      FPGT(6,3,2) =  0.310352451033784D0
      FPGT(6,3,3) =  0.041425537809187D0


      FPGT(7,1,3) = -0.074785022233841D0

      FPGT(7,2,1) =  0.479308067841920D0
      FPGT(7,2,3) =  0.087807628716604D0

      FPGT(7,3,1) =  0.869739794195568D0
      FPGT(7,3,3) =  0.026673617804419D0

      FPGT(7,4,1) =  0.048690315425316D0
      FPGT(7,4,2) =  0.312865496004874D0
      FPGT(7,4,3) =  0.038556880445129D0


      FPGT(8,1,3) =  0.072157803838894D0

      FPGT(8,2,1) =  0.081414823414554D0
      FPGT(8,2,3) =  0.047545817133643D0

      FPGT(8,3,1) =  0.658861384496480D0
      FPGT(8,3,3) =  0.051608685267359D0

      FPGT(8,4,1) =  0.898905543365938D0
      FPGT(8,4,3) =  0.016229248811599D0

      FPGT(8,5,1) =  0.008394777409958D0
      FPGT(8,5,2) =  0.263112829634638D0
      FPGT(8,5,3) =  0.013615157087218D0


      FPGT(9,1,3) =  0.048567898141400D0

      FPGT(9,2,1) =  0.020634961602525D0
      FPGT(9,2,3) =  0.015667350113570D0

      FPGT(9,3,1) =  0.125820817014127D0
      FPGT(9,3,3) =  0.038913770502387D0

      FPGT(9,4,1) =  0.623592928761935D0
      FPGT(9,4,3) =  0.039823869463605D0

      FPGT(9,5,1) =  0.910540973211095D0
      FPGT(9,5,3) =  0.012788837829349D0

      FPGT(9,6,1) =  0.036838412054736D0
      FPGT(9,6,2) =  0.221962989160766D0
      FPGT(9,6,3) =  0.021641769688645D0


      FPGT(10,1,3) = 0.045408995191377D0

      FPGT(10,2,1) = 0.028844733232685D0
      FPGT(10,2,3) = 0.018362978878234D0

      FPGT(10,3,1) = 0.781036849029926D0
      FPGT(10,3,3) = 0.022660529717764D0

      FPGT(10,4,1) = 0.141707219414880D0
      FPGT(10,4,2) = 0.307939838764121D0
      FPGT(10,4,3) = 0.036378958422710D0

      FPGT(10,5,1) = 0.025003534762686D0
      FPGT(10,5,2) = 0.246672560639903D0
      FPGT(10,5,3) = 0.014163621265529D0

      FPGT(10,6,1) = 0.009540815400299D0
      FPGT(10,6,2) = 0.066803251012200D0
      FPGT(10,6,3) = 0.004710833481867D0

C Poids Newton-Cotes (1D)

      PNC(1,1)    =  0.25D0
      PNC(1,2)    =  0.75D0

      PNC(2,1)    =  0.155555555555556D0
      PNC(2,2)    =  0.711111111111111D0
      PNC(2,3)    =  0.266666666666667D0

      PNC(3,1)    =  0.131944444444444D0
      PNC(3,2)    =  0.520833333333333D0
      PNC(3,3)    =  0.347222222222222D0

      PNC(4,1)    =  0.097619047619048D0
      PNC(4,2)    =  0.514285714285714D0
      PNC(4,3)    =  0.064285714285714D0
      PNC(4,4)    =  0.647619047619048D0

      PNC(5,1)    =  0.086921296296296D0
      PNC(5,2)    =  0.414004629629630D0
      PNC(5,3)    =  0.153125D0
      PNC(5,4)    =  0.345949074074074D0

      PNC(6,1)    =  0.069770723104057D0
      PNC(6,2)    =  0.415379188712522D0
      PNC(6,3)    = -0.065467372134039D0
      PNC(6,4)    =  0.740458553791887D0
      PNC(6,5)    = -0.320282186948854D0

      PNC(7,1)    =  0.063772321428572D0
      PNC(7,2)    =  0.351361607142857D0
      PNC(7,3)    =  0.024107142857143D0
      PNC(7,4)    =  0.431785714285714D0
      PNC(7,5)    =  0.128973214285714D0

      PNC(8,1)    =  0.053668296723852D0
      PNC(8,2)    =  0.355071882849661D0
      PNC(8,3)    = -0.162087141253808D0
      PNC(8,4)    =  0.909892576559243D0
      PNC(8,5)    = -0.870310245310245D0
      PNC(8,6)    =  1.427529260862590D0

C Poids Newton-Cotes (domaine triangulaire)

      PNCT(1,1)   = 0.016666666666667D0
      PNCT(1,2)   = 0.0375D0
      PNCT(1,3)   = 0.225D0

      PNCT(2,1)   =  0.D0
      PNCT(2,2)   =  0.044444444444445D0
      PNCT(2,3)   = -0.011111111111111D0
      PNCT(2,4)   =  0.088888888888889D0

      PNCT(3,1)   = 0.005456349206349D0
      PNCT(3,2)   = 0.012400793650794D0
      PNCT(3,3)   = 0.012400793650794D0
      PNCT(3,4)   = 0.099206349206349D0
      PNCT(3,5)   = 0.012400793650794D0

      PNCT(4,1)   =  0.D0
      PNCT(4,2)   =  0.021428571428572D0
      PNCT(4,3)   = -0.016071428571429D0
      PNCT(4,4)   =  0.042857142857143D0
      PNCT(4,5)   =  0.038095238095238D0
      PNCT(4,6)   =  0.042857142857143D0
      PNCT(4,7)   = -0.032142857142857D0

      PNCT(5,1)   =  0.002577160493827D0
      PNCT(5,2)   =  0.005765817901235D0
      PNCT(5,3)   =  0.006900077160494D0
      PNCT(5,4)   =  0.062195216049383D0
      PNCT(5,5)   =  0.005198688271605D0
      PNCT(5,6)   = -0.013233024691358D0
      PNCT(5,7)   =  0.086014660493827D0
      PNCT(5,8)   =  0.006616512345679D0

      PNCT(6,1)   =  0.D0
      PNCT(6,2)   =  0.012980599647266D0
      PNCT(6,3)   = -0.016507936507937D0
      PNCT(6,4)   =  0.024832451499118D0
      PNCT(6,5)   =  0.040070546737213D0
      PNCT(6,6)   =  0.029347442680776D0
      PNCT(6,7)   = -0.038201058201058D0
      PNCT(6,8)   =  0.023703703703704D0
      PNCT(6,9)   = -0.051075837742504D0
      PNCT(6,10)  =  0.051922398589065D0

C_______________________________________________________________________
C
C
C NOEUD
C
      IF (TYPI .EQ. 1) THEN
        IF (ALIAS(1:3) .EQ. 'SE2') THEN
            IF (NORD .EQ. 1) THEN
                XPG = -1.D0
            ELSE IF (NORD .EQ. 2) THEN
                XPG =  1.D0
            ELSE
                CALL ASSERT(.FALSE.)
            END IF
            YPG = 0.D0
            HPG = 1.D0
        ELSE IF (ALIAS(1:3) .EQ. 'SE3') THEN
            IF (NORD .EQ. 1) THEN
                XPG = -1.D0
                HPG =  0.5D0
            ELSE IF (NORD .EQ. 2) THEN
                XPG =  1.D0
                HPG =  0.5D0
            ELSE IF (NORD .EQ. 3) THEN
                XPG =  0.D0
                HPG =  1.D0
            ELSE
                CALL ASSERT(.FALSE.)
            END IF
            YPG = 0.D0
        ELSE IF (ALIAS(1:3) .EQ. 'TR3') THEN
            IF (NORD .EQ. 1) THEN
                XPG = 0.D0
                YPG = 0.D0
            ELSE IF (NORD .EQ. 2) THEN
                XPG = 1.D0
                YPG = 0.D0
            ELSE IF (NORD .EQ. 3) THEN
                XPG = 0.D0
                YPG = 1.D0
            ELSE
                CALL ASSERT(.FALSE.)
            END IF
            HPG = 1.D0/6.D0
        ELSE IF ((ALIAS(1:3).EQ.'TR6').OR.(ALIAS(1:3).EQ.'TR7')) THEN
            IF (NORD .EQ. 1) THEN
                XPG = 0.D0
                YPG = 0.D0
            ELSE IF (NORD .EQ. 2) THEN
                XPG = 1.D0
                YPG = 0.D0
            ELSE IF (NORD .EQ. 3) THEN
                XPG = 0.D0
                YPG = 1.D0
            ELSE IF (NORD .EQ. 4) THEN
                XPG = 0.5D0
                YPG = 0.D0
            ELSE IF (NORD .EQ. 5) THEN
                XPG = 0.D0
                YPG = 0.5D0
            ELSE IF (NORD .EQ. 6) THEN
                XPG = 0.5D0
                YPG = 0.5D0
            ELSE
                CALL ASSERT(.FALSE.)
            END IF
            HPG = 1.D0/12.D0
        ELSE IF ((ALIAS(1:3) .EQ. 'QU4')) THEN
            IF (NORD .EQ. 1) THEN
                XPG = -1.D0
                YPG = -1.D0
            ELSE IF (NORD .EQ. 2) THEN
                XPG =  1.D0
                YPG = -1.D0
            ELSE IF (NORD .EQ. 3) THEN
                XPG =  1.D0
                YPG =  1.D0
            ELSE IF (NORD .EQ. 4) THEN
                XPG = -1.D0
                YPG =  1.D0
            ELSE
                CALL ASSERT(.FALSE.)
            END IF
            HPG = 1.D0
        ELSE IF ((ALIAS(1:3).EQ.'QU8')) THEN
            IF (NORD .EQ. 1) THEN
                XPG = -1.D0
                YPG = -1.D0
            ELSE IF (NORD .EQ. 2) THEN
                XPG =  1.D0
                YPG = -1.D0
            ELSE IF (NORD .EQ. 3) THEN
                XPG =  1.D0
                YPG =  1.D0
            ELSE IF (NORD .EQ. 4) THEN
                XPG = -1.D0
                YPG =  1.D0
            ELSE IF (NORD .EQ. 5) THEN
                XPG =  0.D0
                YPG = -1.D0
            ELSE IF (NORD .EQ. 6) THEN
                XPG =  1.D0
                YPG =  0.D0
            ELSE IF (NORD .EQ. 7) THEN
                XPG =  0.D0
                YPG =  1.D0
            ELSE IF (NORD .EQ. 8) THEN
                XPG = -1.D0
                YPG =  0.D0
            ELSE
                CALL ASSERT(.FALSE.)
            END IF
            HPG =  0.5D0
        ELSE IF ((ALIAS(1:3).EQ.'QU9')) THEN
C ----- ACTIVATION SCHEMA AUX NOEUDS POUR QU9 DIFFERENT DE QU8
C ----- POUR PERMETTRE ELIMINATION DES NOEUDS BASEE SUR LES 
C ----- POINTS D'INTEGRATION ( CONDITIONS LIMITE - BARSOUM )
           IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG = -1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  1.D0
            YPG =  1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -1.D0
            YPG =  1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 5) THEN
            XPG =  0.D0
            YPG = -1.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 6) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 7) THEN
            XPG =  0.D0
            YPG =  1.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 8) THEN
            XPG = -1.D0
            YPG = -0.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 9) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG = 16.D0 / 9.D0
          ENDIF
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
C_______________________________________________________________________
C
C
C GAUSS
C
      ELSE IF (MOD(TYPI,10) .EQ. 2) THEN
        PARAM = TYPI/10
    
        IF (ALIAS(1:2) .EQ. 'SE') THEN
            IF ((ALIAS(3:3) .EQ. '3').AND.(PARAM .LE. 2)) THEN
                K = 3
            ELSE
                K = PARAM
            END IF
        
            H   = (K+1)/2
            I   = MOD(NORD,H)+1        
    
            XPG = SIGN(FPG(K,I,1),(2.D0*(NORD/(H+1))-1.D0))
            YPG = 0.D0
            HPG = FPG(K,I,2)
        ELSE IF (ALIAS(1:2) .EQ. 'TR') THEN
            H=0
            N=NORD
   40      CONTINUE   
           IF (N .GT. 0) THEN
                H=H+1
                IF (FPGT(PARAM,H,1) .EQ. 0) THEN
                    N = N - 1
                    I = 0
                ELSE IF (FPGT(PARAM,H,2) .EQ. 0) THEN
                    N = N - 3
                    I = 1
                ELSE
                    N = N - 6
                    I = 2
                END IF
            GOTO 40 
            END IF
            N = 1-N
            
            HPG = FPGT(PARAM,H,3)
        
            IF (I .EQ. 0) THEN
                XPG = 0.333333333333333D0
                YPG = 0.333333333333333D0
            ELSE IF (I .EQ. 1) THEN
                IF(N .EQ. 1) THEN
                    XPG = FPGT(PARAM,H,1)
                    YPG = 0.5D0-FPGT(PARAM,H,1)/2.D0
                ELSE IF(N .EQ. 2) THEN
                    XPG = 0.5D0-FPGT(PARAM,H,1)/2.D0
                    YPG = FPGT(PARAM,H,1)
                ELSE IF(N .EQ. 3) THEN
                    XPG = 0.5D0-FPGT(PARAM,H,1)/2.D0
                    YPG = XPG
                ELSE
                    CALL ASSERT(.FALSE.)
                END IF
            ELSE IF (I .EQ. 2) THEN
                IF(N .EQ. 1) THEN
                    XPG = FPGT(PARAM,H,1)
                    YPG = FPGT(PARAM,H,2)
                ELSE IF(N .EQ. 2) THEN
                    XPG = FPGT(PARAM,H,2)
                    YPG = FPGT(PARAM,H,1)
                ELSE IF(N .EQ. 3) THEN
                    XPG = FPGT(PARAM,H,1)
                    YPG = 1.D0-FPGT(PARAM,H,1)-FPGT(PARAM,H,2)
                ELSE IF(N .EQ. 4) THEN
                    XPG = 1.D0-FPGT(PARAM,H,1)-FPGT(PARAM,H,2)
                    YPG = FPGT(PARAM,H,1)
                ELSE IF(N .EQ. 5) THEN
                    XPG = FPGT(PARAM,H,2)
                    YPG = 1.D0-FPGT(PARAM,H,1)-FPGT(PARAM,H,2)
                ELSE IF(N .EQ. 6) THEN
                    XPG = 1.D0-FPGT(PARAM,H,1)-FPGT(PARAM,H,2)
                    YPG = FPGT(PARAM,H,2)
                ELSE 
                    CALL ASSERT(.FALSE.)
                END IF
            ELSE
                CALL ASSERT(.FALSE.)
            END IF
        ELSE IF (ALIAS(1:2) .EQ. 'QU') THEN
            H   = (PARAM+1)/2        
            J   = MOD(NORD-1,PARAM)+1
            I   = MOD(J,H)+1
        
            XPG = SIGN(FPG(PARAM,I,1),(2.D0*(J/(H+1))-1.D0))
            HPG = FPG(PARAM,I,2)
        
            J   = (NORD-1)/PARAM+1
            I   = MOD(J,H)+1
        
            YPG = SIGN(FPG(PARAM,I,1),(2.D0*(J/(H+1))-1.D0))
            HPG = HPG*FPG(PARAM,I,2)
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
C_______________________________________________________________________
C
C
C SIMPSON
C
      ELSE IF (MOD(TYPI,10) .EQ. 3) THEN
        PARAM = TYPI/10
C
C Segments: SE2 et SE3
C
C Exemple à 2 blocs         Numérotation         Poids (x6)
C
C                         1---2---3---4---5      1---4---2---4---1
C
C      Blocs     | 1 | 2 | 3 | 4 |
C ---------------+---+---+---+---+
C Nombre de point| 3 | 5 | 7 | 9 |
        IF (ALIAS(1:2) .EQ. 'SE') THEN
C ---- POUR SIMPSON PRISE EN COMPTE ELIMINATION DES NOEUDS        
         IF(PARAM.EQ.1) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  4.D0 / 3.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
         ELSE       
        
            N = NORD-1
            
            XPG = -1.D0 + (N*1.D0)/PARAM
            YPG = 0.D0
        
            IF ((N .EQ. 0) .OR. (N .EQ. 2*PARAM)) THEN
                HPG = 1.D0
            ELSE
                IF (MOD(N,2) .EQ. 0) THEN
                    HPG = 2.D0
                ELSE
                    HPG = 4.D0
                END IF
            END IF 
            HPG = HPG/(PARAM*3.D0)
         ENDIF
C
C Triangles: TR3, TR6 et TR7
C
C Exemple à 2 blocs         Numérotation            Poids (x120)
C
C                           15                      1
C                           | \                     | \
C                           10  14                  4   4
C                           |     \                 |     \
C                           6   9   13              3---8---3
C                           |         \             | \     | \
C                           3   5   8   12          4   8   8   4
C                           |             \         |     \ |     \
C                           1---2---4---7--11       1---4---3---4---1
C
C      Blocs     | 1 | 2 | 3 | 4 |
C ---------------+---+---+---+---+
C Nombre de point| 6 | 15| 28| 45|
C
        ELSE IF (ALIAS(1:2) .EQ. 'TR') THEN
            H=0
            N=NORD
   50       CONTINUE
            IF (N .GT. 0) THEN
                H=H+1
                N=N-H
            GOTO 50
            END IF
            H = H-1
            I = -N
            J = H+N
            H = 2*PARAM
        
            XPG = 0.5D0*I/PARAM
            YPG = 0.5D0*J/PARAM
    
            IF ((I .EQ. 0) .OR. (J .EQ. 0) .OR. (I+J .EQ. H)) THEN
                IF (I .EQ. 0) THEN
                    IF ((J .EQ. 0) .OR. (J .EQ. H)) THEN
                        HPG = 1.D0
                    ELSE
                        IF (MOD(J,2) .EQ. 0) THEN
                            HPG = 3.D0
                        ELSE
                            HPG = 4.D0 
                        END IF
                    END IF
                ELSE IF (J .EQ. 0) THEN
                    IF (I .EQ. H) THEN
                        HPG = 1.D0
                    ELSE
                        IF (MOD(I,2) .EQ. 0) THEN
                            HPG = 3.D0
                        ELSE
                            HPG = 4.D0 
                        END IF
                    END IF
                ELSE
                    IF (MOD(J,2) .EQ. 0) THEN
                        HPG = 3.D0
                    ELSE
                        HPG = 4.D0 
                    END IF
                END IF
            ELSE
                IF ((MOD(I,2) .EQ. 0) .AND. (MOD(J,2) .EQ. 0)) THEN
                    HPG = 6.D0
                ELSE
                    HPG = 8.D0 
                END IF
            END IF
            HPG = HPG/((PARAM**2)*30.D0)
C
C Quadrangles: QU4, QU8 et QU9
C
C Exemple à 2 blocs         Numérotation          Poids (x36)
C
C                          21--22--23--24--25     1---4---2---4---1
C                           |               |     |       |       |
C                          16  17  18  19  20     4  16   8  16   4
C                           |               |     |       |       |
C                          11  12  13  14  15     2---8---4---8---2
C                           |               |     |       |       |
C                           6   7   8   9  10     4  16   8  16   4
C                           |               |     |       |       |
C                           1---2---3---4---5     1---4---2---4---1
C
C      Blocs     | 1 | 2 | 3 | 4 |
C ---------------+---+---+---+---+
C Nombre de point| 9 | 25| 49| 81|
C
        ELSE IF (ALIAS(1:2) .EQ. 'QU') THEN
C----- POUR SIMPSON PRISE EN COMPTE ELIMINATION DES NOEUDS
          IF(PARAM.EQ.1) THEN
           IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG = -1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  1.D0
            YPG =  1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -1.D0
            YPG =  1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 5) THEN
            XPG =  0.D0
            YPG = -1.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 6) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 7) THEN
            XPG =  0.D0
            YPG =  1.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 8) THEN
            XPG = -1.D0
            YPG = -0.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 9) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG = 16.D0 / 9.D0
          ENDIF
         ELSE
        
            H = 2*PARAM+1
            I = MOD(NORD-1,H)
            J = (NORD-1)/H
            H = H-1
        
            XPG = -1.D0 +(I*1.D0)/PARAM
            YPG = -1.D0 +(J*1.D0)/PARAM

            IF ((I.EQ.0).OR.(J.EQ.0).OR.(I.EQ.H).OR.(J.EQ.H)) THEN
                IF ((I .EQ. 0) .OR. (I .EQ. H)) THEN
                    IF ((J .EQ. 0) .OR. (J .EQ. H)) THEN
                        HPG = 1.D0
                    ELSE
                        IF (MOD(J,2) .EQ. 0) THEN
                            HPG = 2.D0
                        ELSE
                            HPG = 4.D0 
                        END IF
                    END IF
                ELSE
                    IF (MOD(I,2) .EQ. 0) THEN
                        HPG = 2.D0
                    ELSE
                        HPG = 4.D0
                    END IF
                END IF
            ELSE
                IF ((MOD(I,2) .EQ. 0) .AND. (MOD(J,2) .EQ. 0)) THEN
                    HPG = 4.D0
                ELSE IF ((MOD(I,2).EQ.1).AND.(MOD(J,2).EQ.1)) THEN
                    HPG = 16.D0
                ELSE
                    HPG = 8.D0
                END IF
            END IF
            HPG = HPG/((PARAM**2)*9.D0)
          ENDIF
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
C_______________________________________________________________________
C
C
C NCOTES
C
      ELSE IF (MOD(TYPI,10) .EQ. 4) THEN
        PARAM = TYPI/10
C
C Segments: SE2 et SE3
C
C Exemple d'ordre 4          Numérotation           Poids (x45)
C
C                         1---2---3---4---5      7---32--12--32--7
C
C      Degré     | 3 | 4 | 5 | 6 | 7 | 8 |
C ---------------+---+---+---+---+---+---+
C Nombre de point| 4 | 5 | 6 | 7 | 8 | 9 |
C
        IF (ALIAS(1:2) .EQ. 'SE') THEN
            H = (PARAM+2)/2
            IF(NORD .LE. H) THEN
                I = NORD
            ELSE
                I = PARAM-NORD+2
            END IF

            XPG = -1.D0 + 2.D0*(NORD-1)/PARAM
            YPG = 0.D0
        
            HPG = PNC(PARAM-2,I)
C
C Triangles: TR3, TR6 et TR7
C
C Exemple d'ordre 4         Numérotation            Poids (x90)
C
C                           15                      0
C                           | \                     | \
C                           10  14                  4   4
C                           |     \                 |     \
C                           6   9   13             -1   8  -1
C                           |         \             |         \
C                           3   5   8   12          4   8   8   4
C                           |             \         |             \
C                           1---2---4---7--11       0---4-(-1)--4---0
C
C      Degré     | 3 | 4 | 5 | 6 | 7 | 8 |
C ---------------+---+---+---+---+---+---+
C Nombre de point| 10| 15| 21| 28| 36| 45|
C
        ELSE IF (ALIAS(1:2) .EQ. 'TR') THEN
            H=0
            N=NORD
C----- Schémas de Newton Cotes non programmés pour 
C ordre supérieur à 8 pour triangles
            IF (PARAM.GT.8) THEN
                PARAM=8
            ENDIF  
   60       CONTINUE
            IF (N .GT. 0) THEN
                H=H+1
                N=N-H
            GOTO 60
            END IF
            I = -N
            J = H-1+N
        
            XPG = (I*1.D0)/PARAM
            YPG = (J*1.D0)/PARAM
        
            H=MIN(I,J,PARAM-I-J)
            I=PARAM-MAX(I,J,PARAM-I-J)
        
            J = PARAM/2
            IF (I .LE. J) THEN
                HPG=PNCT(PARAM-2,((I+1)/2)*(I/2+1)+1+H)
            ELSE
                HPG=PNCT(PARAM-2,((I+1)/2)*(I/2+1)+1+H
     &              -(I-(PARAM/2))*(I-((PARAM-1)/2)))
            END IF
C
C Quadrangles: QU4, QU8 et QU9
C
C Exemple d'ordre 4          Numérotation          Poids (x2025)
C 
C                          21--22--23--24--25    49--224--84-224--49
C                           |               |     |               |
C                          16  17  18  19  20   224	1024 384 1024 224
C                           |               |     |               |
C                          11  12  13  14  15    84	384  144 384  84
C                           |               |     |               |
C                           6   7   8   9  10   224	1024 384 1024 224
C                           |               |     |               |
C                           1---2---3---4---5    49--224--84-224--49
C
C      Degré     | 3 | 4 | 5 | 6 | 7 | 8 |
C ---------------+---+---+---+---+---+---+
C Nombre de point| 16| 25| 36| 49| 64| 81|
C
        ELSE IF (ALIAS(1:2) .EQ. 'QU') THEN
            H = (PARAM+2)/2        
        
            J = MOD(NORD-1,PARAM+1)+1
            IF(J .LE. H) THEN
                I = J
            ELSE
                I = PARAM-J+2
            END IF
            XPG = -1.D0 + 2.D0*(J-1)/PARAM
            HPG = PNC(PARAM-2,I)
        
        
            J   = (NORD-1)/(PARAM+1)+1
            IF(J .LE. H) THEN
                I = J
            ELSE
                I = PARAM-J+2
            END IF
            YPG = -1.D0 + 2.D0*(J-1)/PARAM
            HPG = HPG*PNC(PARAM-2,I)
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
      END
