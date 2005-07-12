      SUBROUTINE DCALPH(X,Y,NBPTS,PE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

C**********************************************************
C              BUT DE CETTE ROUTINE :                     *
C CALCULER L ORDRE DE LA SINGULARITE PAR LA METHODE       *
C DES MOINDRES CARRES                                     *
C RECHERCHE DE LA PENTE ET DE LA CONSTANTE PAR LA METHODE *
C DES MOINDRES CARRES D UNE DROITE Y = A * X + B          *
C**********************************************************

C IN  X     : COORDONNEES DES POINTS (RAYON)
C IN  Y     : COORDONNEES DES POINTS (ERREUR MOYENNE)
C IN  NBPTS : NOMBRE DE POINTS
C OUT PE    : DEGRE DE LA SINGULARITE

      IMPLICIT NONE 

C DECLARATION GLOBALE

      INTEGER NBPTS
      REAL*8  X(NBPTS),Y(NBPTS),PE

C DECLARATION LOCALE

      INTEGER IPT, ITER 
      REAL*8  XMOY,YMOY,VX,VXY,A,B,C,C1,F,YMIN

C 1 - INITIALISATION

      C=0.D+0   
      ITER=0

      YMIN=Y(1)
      DO 5 IPT=2,NBPTS
        YMIN=MIN(YMIN,Y(IPT))
 5    CONTINUE

 100  CONTINUE

      XMOY=0.D0
      YMOY=0.D0
      VX=0.D0
      VXY=0.D0

C 2 - RECHERCHE DU X MOYEN ET DU Y MOYEN

      DO 10 IPT=1,NBPTS
        XMOY=XMOY+LOG(X(IPT))
        YMOY=YMOY+LOG(Y(IPT)-C)
 10   CONTINUE

      XMOY=XMOY/NBPTS
      YMOY=YMOY/NBPTS

      DO 15 IPT=1,NBPTS 
        VX=VX+(XMOY-LOG(X(IPT)))*(XMOY-LOG(X(IPT)))
        VXY= VXY+(XMOY-LOG(X(IPT)))*(YMOY-LOG(Y(IPT)-C))
 15   CONTINUE

C 3 - PENTE THEORIQUE = VXY/VX
C CONSTANTE = Y - PENTE * X 

      A=VXY/VX 
      B=EXP(YMOY - A*XMOY)
      F=0.D+0

      DO 20 IPT=1,NBPTS
        F=F+(Y(IPT)-C-B*X(IPT)**A)**2 
 20   CONTINUE                                     

      C1=0.D+0

      DO 25 IPT=1,NBPTS
        C1=C1+(Y(IPT)-C-B*X(IPT)**A)
 25   CONTINUE    

      C1=C1/NBPTS                               

      IF (C1.GT.0.AND.C1.LT.YMIN ) THEN
        IF (ABS((C1-C)/C1).GT.1) THEN
          C=C1
          F=0.D+0
          DO 30 IPT=1,NBPTS
            F=F+(Y(IPT)-C-B*X(IPT)**A)**2 
 30       CONTINUE                                     
          ITER=ITER+1
          IF (ITER.LT.5) GOTO 100
        ENDIF
      ENDIF

      PE=(A/2.D0)+1.D0
      IF (PE.LE.0.4D0) PE=0.4D0
      
      END
