      SUBROUTINE ALBFG2(NDDL  , MCPL  , ITER  , W0    , S    ,
     &                  Y     , XM    , GM    , X     , G    ,
     &                  D     , A     , YISI  )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER  NDDL, MCPL, ITER
      REAL*8   W0(NDDL), S(NDDL*2,MCPL), Y(NDDL*2,MCPL)
      REAL*8   XM(NDDL), GM(NDDL), X(NDDL), G(NDDL), D(NDDL)
      REAL*8   A(MCPL), YISI(MCPL)

C ----------------------------------------------------------------------
C           PRODUCTION D'UNE DIRECTION DE DESCENTE PAR BFGS
C ----------------------------------------------------------------------
C IN       NDDL    I   NOMBRE DE DEGRES DE LIBERTE
C IN       MCPL    I   NOMBRE DE COUPLES STOCKES (RECOMMANDE : MCPL=20)
C IN       ITER    I   ITERATION COURANTE(0, 1, ...)
C VAR      W0     R8   MATRICE INITIALE DIAGONALE
C VAR      S      R8   DELTA VARIABLES DES MCP DERNIERS COUPLES
C VAR      Y      R8   DELTA GRADIENTS DES MCP DERNIERS COUPLES
C                       S ET Y SONT RESERVES DANS UNE ZONE MEMOIRE
C                       COMMUNE : S1,Y1,S2,Y2,...,SMCP,YMCP
C IN       XM     R8   VARIABLES A L'ITERATION PRECEDENTE (ITER >= 1)
C IN       GM     R8   GRADIENTS A L'ITERATION PRECEDENTE (ITER >= 1)
C IN       X      R8   VARIABLES A L'ITERATION COURANTE
C IN       G      R8   GRADIENTS A L'ITERATION COURANTE
C OUT      D      R8   DIRECTION (DE DESCENTE) -W.G
C MEM      A      R8   TABLEAU DE TRAVAIL (1:MCPL)
C MEM      YISI   R8   TABLEAU DE TRAVAIL (1:MCPL)
C ----------------------------------------------------------------------

      INTEGER DEB, FIN, NCPL, N, I, IND
      REAL*8  YS, YDY, SDM1S,  AIMBI, DELTA0, NOS, NOY
      REAL*8  DDOT



C ======================================================================
C              CONSTRUCTION ET STOCKAGE DU NOUVEAU COUPLE
C ======================================================================


C    GESTION DU STOCKAGE CYCLIQUE SUR MCP COUPLES
      IF (ITER .LE. MCPL) THEN
        DEB  = 1
        FIN  = ITER
        NCPL = ITER
      ELSE
        FIN  = MOD(ITER-1, MCPL)+1
        DEB  = MOD(FIN   , MCPL)+1
        NCPL = MCPL
      END IF

C    CALCUL DU NOUVEAU COUPLE (S,Y)
      CALL DCOPY(NDDL, X,1, S(1,FIN),1)
      CALL DAXPY(NDDL, -1.D0, XM,1, S(1,FIN),1)
      CALL DCOPY(NDDL, G,1, Y(1,FIN),1)
      CALL DAXPY(NDDL, -1.D0, GM,1, Y(1,FIN),1)

      NOS = DDOT(NDDL, S(1,FIN),1, S(1,FIN),1)
      NOY = DDOT(NDDL, Y(1,FIN),1, Y(1,FIN),1)

      IF (NOS.EQ.0) CALL UTMESS('F','ALBFG2','POINTS CONFONDUS')
      IF (NOY.EQ.0) CALL UTMESS('F','ALBFG2','GRADIENTS CONFONDUS')



C ======================================================================
C            CONSTRUCTION DE LA NOUVELLE MATRICE INITIALE
C      DIAGONALISATION DE BFGS DIRECTE AVEC MISE A L'ECHELLE  (4.9)
C ======================================================================

      IF (ITER.EQ.1) THEN

C      INITIALISATION PAR DELTA0.ID
        DELTA0 = DDOT(NDDL, Y(1,FIN),1, S(1,FIN),1)
     &         / DDOT(NDDL, Y(1,FIN),1, Y(1,FIN),1)
        DO 10 N = 1,NDDL
          W0(N) = DELTA0
 10     CONTINUE

      ELSE

C      CONSTRUCTION DES PRODUITS SCALAIRES COMMUNS
        YS    = DDOT(NDDL, Y(1,FIN),1, S(1,FIN),1)
        YDY   = 0
        SDM1S = 0
        DO 20 N = 1,NDDL
          YDY   = YDY   + Y(N,FIN)**2 * W0(N)
          SDM1S = SDM1S + S(N,FIN)**2 / W0(N)
 20     CONTINUE

C      CONSTRUCTION DES COMPOSANTES DE LA NOUVELLE MATRICE DIAGONALE
        DO 25 N = 1,NDDL
          W0(N) = 1.D0 / ( YDY / (YS*W0(N))  +  Y(N,FIN)**2 / YS
     &                 -  YDY * (S(N,FIN)/W0(N))**2 / (YS*SDM1S) )
 25     CONTINUE
      END IF



C ======================================================================
C             CALCUL DE LA NOUVELLE DIRECTION DE DESCENTE -W.G
C ======================================================================

      CALL DCOPY(NDDL, G,1, D,1)

      DO 30 I = NCPL, 1, -1
        IND     = MOD( I-1+DEB - 1, MCPL) + 1
        YISI(I) = DDOT(NDDL, Y(1,IND),1, S(1,IND),1)
        A(I)    = DDOT(NDDL, D,1, S(1,IND),1) / YISI(I)
        CALL DAXPY(NDDL, -A(I), Y(1,IND),1, D,1)
 30   CONTINUE

      DO 40 N = 1,NDDL
        D(N) = W0(N)*D(N)
 40   CONTINUE

      DO 50 I = 1, NCPL
        IND     = MOD( I-1+DEB - 1, MCPL) + 1
        AIMBI = A(I) - DDOT(NDDL, Y(1,IND),1, D,1) / YISI(I)
        CALL DAXPY(NDDL, AIMBI, S(1,IND),1, D,1)
 50   CONTINUE

      DO 60 N = 1, NDDL
        D(N) = -D(N)
 60   CONTINUE


      END
