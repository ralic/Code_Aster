      SUBROUTINE ALBFGS(NDDL  , MCPL  , ITER  , MEM   ,
     &                  X     , G     , D     )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/05/2000   AUTEUR VABHHTS J.PELLET 
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
      REAL*8   X(NDDL), G(NDDL), D(NDDL)
      REAL*8   MEM(*)

C ----------------------------------------------------------------------
C                  INTERFACE D'APPEL A L'ALGORITHME BFGS
C ----------------------------------------------------------------------
C IN       NDDL    I   NOMBRE DE DEGRES DE LIBERTE
C IN       MCPL    I   NOMBRE DE COUPLES STOCKES (RECOMMANDE : MCPL=20)
C IN       ITER    I   ITERATION COURANTE(0, 1, ...)
C VAR      MEM    R8   ZONE MEMOIRE DE TRAVAIL : NDDL * (2*MCPL+3)
C                                              + MCPL * 2
C IN       X      R8   VARIABLES A L'ITERATION COURANTE
C IN       G      R8   GRADIENTS A L'ITERATION COURANTE
C OUT      D      R8   DIRECTION DE DESCENTE -W.G
C ----------------------------------------------------------------------

      INTEGER DEB, FIN, NCPL, N, I, IND
      INTEGER IW0, IXM, IGM, IS, IY, IA, IYISI
      REAL*8  YS, YDY, SDM1S, AIMBI
      REAL*8  R8DOT


      IW0   = 1
      IXM   = IW0 + NDDL
      IGM   = IXM + NDDL
      IS    = IGM + NDDL
      IY    = IS  + NDDL
      IA    = IS  + 2*MCPL*NDDL
      IYISI = IA  + MCPL


C ======================================================================
C                    INITIATLISATION DE L'ALGORITHME
C ======================================================================

      IF (ITER.EQ.0) THEN
        DO 5 N = 1,NDDL
          MEM(IW0-1 + N) =   1.D0
          D(N)           = - G(N)
 5      CONTINUE
      END IF



C ======================================================================
C                             ALGORITHME BFGS
C ======================================================================

      IF (ITER.GE.1) THEN
         CALL ALBFG2(NDDL  , MCPL  , ITER  , MEM(IW0), MEM(IS),
     &               MEM(IY), MEM(IXM), MEM(IGM), X   , G     ,
     &               D, MEM(IA), MEM(IYISI)     )
      END IF


C ======================================================================
C                    STOCKAGE DU POINT ET DU GRADIENT
C ======================================================================

      CALL R8COPY(NDDL, X,1, MEM(IXM),1)
      CALL R8COPY(NDDL, G,1, MEM(IGM),1)

      END
