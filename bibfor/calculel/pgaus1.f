      SUBROUTINE PGAUS1(FG,PG,G,NG)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C A_UTIL
C ----------------------------------------------------------------------
C             POIDS ET COORDONNEES DES POINTS DE GAUSS EN 1D
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER        FG         : FAMILLE D'INTEGRATION
C
C VARIABLES DE SORTIE
C REAL*8         PG(NG)     : POIDS DE GAUSS (P1,P2,...)
C REAL*8         G(NG)      : POINTS DE GAUSS (X1,X2,...)
C INTEGER        NG         : NOMBRE DE POINTS DE GAUSS
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      REAL*8       PG(*),G(*)
      INTEGER      FG,NG
 
      IF (FG.EQ.1) THEN
C       X**A AVEC A <= 1
        PG(1)  =  2.D0
        G(1)   =  0.D0
      ELSEIF (FG.EQ.2) THEN
C       X**A AVEC A <= 3
        PG(1)  =  1.0D0
        PG(2)  =  PG(1)
        G(1)   = -0.577350269189626D0
        G(2)   = -G(1)
      ELSEIF (FG.EQ.3) THEN
C       X**A AVEC A <= 5
        PG(1)  =  5.D0/9.D0
        PG(2)  =  8.D0/9.D0
        PG(3)  =  PG(1)
        G(1)   = -0.774596669241483D0
        G(2)   =  0.D0
        G(3)   = -G(1)
      ELSEIF (FG.EQ.4) THEN
C       X**A AVEC A <= 7
        PG(1)  =  0.652145154862546D0
        PG(2)  =  PG(1)
        PG(3)  =  0.347854845137454D0
        PG(4)  =  PG(3)
        G(1)   = -0.339981043584856D0
        G(2)   = -G(1)
        G(3)   = -0.861136311594053D0
        G(4)   = -G(3)
      ELSE 
        CALL UTMESS('F','PGAUS1','FAMILLE NON DISPONIBLE')
      ENDIF

      CALL PGAUSN('SEG     ',FG,NG)
           
      END
