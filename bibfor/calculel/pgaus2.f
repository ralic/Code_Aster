      SUBROUTINE PGAUS2(TYPEMA,FG,PG,G,NG)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C            POIDS ET COORDONNEES DES POINTS DE GAUSS EN 2D
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8    TYPEMA     : TYPE DE LA MAILLE
C INTEGER        FG         : FAMILLE D'INTEGRATION
C
C VARIABLES DE SORTIE
C REAL*8         PG(NG)     : POIDS DE GAUSS (P1,P2,...)
C REAL*8         G(2,NG)    : POINTS DE GAUSS (X1,Y1,X2,...)
C INTEGER        NG         : NOMBRE DE POINTS DE GAUSS
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8  TYPEMA
      REAL*8       PG(*),G(*),PG1(4),G1(4)
      INTEGER      FG,NG,I,J,P1,P2,N1,IBID

C --- TRIANGLE

      IF (TYPEMA(1:3).EQ.'TRI') THEN
        IF (FG.EQ.1) THEN
C         X**A * Y**B AVEC A+B <= 1
          PG(1)  =  0.5D0
          G(1)   =  1.D0/3.D0
          G(2)   =  G(1)
        ELSEIF (FG.EQ.2) THEN
C         X**A * Y**B AVEC A+B <= 1
          PG(1)  =  1.D0/6.D0
          PG(2)  =  PG(1)
          PG(3)  =  PG(1)
          CALL NOEUD('TRIA3   ',G,NG,IBID)
        ELSEIF (FG.EQ.3) THEN
C         X**A * Y**B AVEC A+B <= 2
          PG(1)  =  1.D0/6.D0
          PG(2)  =  PG(1)
          PG(3)  =  PG(1)
          G(1)   =  0.5D0
          G(2)   =  0.5D0
          G(3)   =  0.D0
          G(4)   =  0.5D0
          G(5)   =  0.5D0
          G(6)   =  0.D0
        ELSEIF (FG.EQ.4) THEN
C         X**A * Y**B AVEC A+B <= 2
          PG(1)  =  1.D0/6.D0
          PG(2)  =  PG(1)
          PG(3)  =  PG(1)
          G(1)   =  PG(1)
          G(2)   =  G(1)
          G(3)   =  2.D0/3.D0
          G(4)   =  G(1)
          G(5)   =  G(1)
          G(6)   =  G(3)
        ELSEIF (FG.EQ.5) THEN
C         X**A * Y**B AVEC A+B <= 3
          PG(1)  = -27.D0/96.D0
          PG(2)  =  25.D0/96.D0
          PG(3)  =  PG(2)
          PG(4)  =  PG(2)
          G(1)   =  1.D0/3.D0
          G(2)   =  G(1)
          G(3)   =  0.2D0
          G(4)   =  G(3)
          G(5)   =  0.6D0
          G(6)   =  G(3)
          G(7)   =  G(3)
          G(8)   =  G(5)
        ELSEIF (FG.EQ.6) THEN
C         X**A * Y**B AVEC A+B <= 4
          PG(1)  =  0.11169079483905D0
          PG(2)  =  PG(1)
          PG(3)  =  PG(1)
          PG(4)  =  0.0549758718227661D0
          PG(5)  =  PG(4)
          PG(6)  =  PG(4)
          G(1)   =  0.445948490915965D0 
          G(2)   =  G(1) 
          G(3)   =  0.108103018168070D0
          G(4)   =  G(1)
          G(5)   =  G(1)
          G(6)   =  G(3)
          G(7)   =  0.091576213509771D0
          G(8)   =  G(7)
          G(9)   =  0.816847572980458D0
          G(10)  =  G(7)
          G(11)  =  G(7)
          G(12)  =  G(9)
        ELSEIF (FG.EQ.7) THEN
C         X**A * Y**B AVEC A+B <= 5
          PG(1)  =  9.D0/80.D0
          PG(2)  =  0.066197076394253D0 
          PG(3)  =  PG(2)
          PG(4)  =  PG(2)
          PG(5)  =  0.062969590272413D0
          PG(6)  =  PG(5)
          PG(7)  =  PG(5)
          G(1)   =  1.D0/3.D0
          G(2)   =  G(1)
          G(3)   =  0.470142064105115D0
          G(4)   =  G(3)
          G(5)   =  0.059715871789770D0
          G(6)   =  G(3)
          G(7)   =  G(3)
          G(8)   =  G(5)
          G(9)   =  0.1012865073234563D0
          G(10)  =  G(9)
          G(11)  =  0.7974269853530873D0
          G(12)  =  G(9)
          G(13)  =  G(9)
          G(14)  =  G(11)
        ELSEIF (FG.EQ.8) THEN
C         X**A * Y**B AVEC A+B <= 6
          PG(1)  =  0.025422453185103D0
          PG(2)  =  PG(1)
          PG(3)  =  PG(1)
          PG(4)  =  0.058393137863189D0
          PG(5)  =  PG(4) 
          PG(6)  =  PG(4)
          PG(7)  =  0.041425537809187D0
          PG(8)  =  PG(7)
          PG(9)  =  PG(7)
          PG(10) =  PG(7)
          PG(11) =  PG(7)
          PG(12) =  PG(7)
          G(1)   =  0.063089014491502D0
          G(2)   =  G(1)
          G(3)   =  0.873821971016996D0
          G(4)   =  G(1)
          G(5)   =  G(1)
          G(6)   =  G(3)
          G(7)   =  0.249286745170910D0
          G(8)   =  G(7)
          G(9)   =  0.501426509658180D0
          G(10)  =  G(7)
          G(11)  =  G(7)
          G(12)  =  G(9)
          G(13)  =  0.310352451033785D0
          G(14)  =  0.053145049844816D0
          G(15)  =  G(14) 
          G(16)  =  G(13)
          G(17)  =  0.636502499121399D0
          G(18)  =  G(13)
          G(19)  =  G(17)
          G(20)  =  G(14)
          G(21)  =  G(13)
          G(22)  =  G(17)
          G(23)  =  G(14)
          G(24)  =  G(17)
        ELSE
          GOTO 100
        ENDIF

C --- QUADRANGLE

      ELSEIF (TYPEMA(1:3).EQ.'QUA') THEN
        IF (FG.EQ.1) THEN
C         X**A * Y**B AVEC A <= 1 ET B <= 1 
          PG(1)  =  4
          G(1)   =  0.D0
          G(2)   =  0.D0
        ELSEIF (FG.EQ.2) THEN
C         X**A * Y**B AVEC A <= 1 ET B <= 1   
          PG(1)  =  1.D0
          PG(2)  =  PG(1)           
          PG(3)  =  PG(1)           
          PG(4)  =  PG(1)           
          CALL NOEUD('QUAD4   ',G,NG,IBID)
        ELSEIF (FG.EQ.3) THEN
C         X**A * Y**B AVEC A <= 3 ET B <= 3
          CALL PGAUS1(2,PG1,G1,N1)
          P1 = 1
          P2 = 1
          DO 10 I = 1, N1
          DO 10 J = 1, N1
            PG(P1) = PG1(I) * PG1(J)
            G(P2) = G1(I)
            G(P2+1) = G1(J)
            P1 = P1 + 1
            P2 = P2 + 2
 10       CONTINUE
        ELSEIF (FG.EQ.4) THEN
C         X**A * Y**B AVEC A <= 2, B <= 2 ET A+B <= 3
          PG(1)  =  -1.D0/3.D0
          PG(2)  =  PG(1)           
          PG(3)  =  PG(1)           
          PG(4)  =  PG(1) 
          PG(5)  =  4.D0/3.D0
          PG(6)  =  PG(5)           
          PG(7)  =  PG(5)           
          PG(8)  =  PG(5)
          CALL NOEUD('QUAD8   ',G,NG,IBID)
        ELSEIF (FG.EQ.5) THEN
C         X**A * Y**B AVEC A <= 2 ET B <= 2
          PG(1)  =  1.D0/9.D0
          PG(2)  =  PG(1)           
          PG(3)  =  PG(1)           
          PG(4)  =  PG(1) 
          PG(5)  =  4.D0/9.D0
          PG(6)  =  PG(5)           
          PG(7)  =  PG(5)           
          PG(8)  =  PG(5)
          PG(9)  =  16.D0/9.D0
          CALL NOEUD('QUAD9   ',G,NG,IBID)
        ELSEIF (FG.EQ.6) THEN
C         X**A * Y**B AVEC A <= 5 ET B <= 5
          CALL PGAUS1(3,PG1,G1,N1)
          P1 = 1
          P2 = 1
          DO 20 I = 1, N1
          DO 20 J = 1, N1
            PG(P1) = PG1(I) * PG1(J)
            G(P2) = G1(I)
            G(P2+1) = G1(J)
            P1 = P1 + 1
            P2 = P2 + 2             
 20       CONTINUE          
        ELSEIF (FG.EQ.7) THEN
C         X**A * Y**B AVEC A <= 7 ET B <= 7
          CALL PGAUS1(4,PG1,G1,N1)
          P1 = 1
          P2 = 1
          DO 30 I = 1, N1
          DO 30 J = 1, N1
            PG(P1) = PG1(I) * PG1(J)
            G(P2) = G1(I)
            G(P2+1) = G1(J)
            P1 = P1 + 1
            P2 = P2 + 2             
 30       CONTINUE              
        ELSE
          GOTO 100
        ENDIF

      ELSE
        GOTO 100
      ENDIF

      CALL PGAUSN(TYPEMA,FG,NG)
      GOTO 110

 100  CONTINUE

      CALL UTMESS('F','PGAUS2','FAMILLE NON DISPONIBLE')
      
 110  CONTINUE
           
      END
