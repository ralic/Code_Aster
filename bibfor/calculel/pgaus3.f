      SUBROUTINE PGAUS3(TYPEMA,FG,PG,G,NG)

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
C             POIDS ET COORDONNEES DES POINTS DE GAUSS EN 3D
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8    TYPEMA     : TYPE DE LA MAILLE
C INTEGER        FG         : FAMILLE D'INTEGRATION
C
C VARIABLES DE SORTIE
C REAL*8         PG(NG)     : POIDS DE GAUSS (P1,P2,...)
C REAL*8         G(3,NG)    : POINTS DE GAUSS (X1,Y1,Z1,X2,...)
C INTEGER        NG         : NOMBRE DE POINTS DE GAUSS
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8  TYPEMA
      REAL*8       PG(*),G(*),PG1(4),G1(4),PG2(7),G2(14)
      INTEGER      FG,NG,I,J,K,P1,P2,P3,N1,N2

C --- TETRAEDRE

      IF (TYPEMA(1:3).EQ.'TET') THEN        
        IF (FG.EQ.1) THEN
C         X**A * Y**B * Z**C AVEC A+B+C <= 2
          PG(1)  =  1.D0/24.D0
          PG(2)  =  PG(1)
          PG(3)  =  PG(1)
          PG(4)  =  PG(1)
          G(1)   =  0.1381966011250105D0            
          G(2)   =  G(1)
          G(3)   =  G(1)
          G(4)   =  G(1)
          G(5)   =  G(1)
          G(6)   =  0.5854101966249684D0
          G(7)   =  G(1)
          G(8)   =  G(6)
          G(9)   =  G(1)
          G(10)  =  G(6)
          G(11)  =  G(1)
          G(12)  =  G(1)
        ELSEIF (FG.EQ.2) THEN
C         X**A * Y**B * Z**C AVEC A+B+C <= 3
          PG(1)  = -2.D0/15.D0
          PG(2)  =  3.D0/40.D0
          PG(3)  =  PG(2)
          PG(4)  =  PG(2)
          PG(5)  =  PG(2)
          G(1)   =  0.25D0
          G(2)   =  G(1)
          G(3)   =  G(1)
          G(4)   =  1.D0/6.D0
          G(5)   =  G(4)
          G(6)   =  G(4)
          G(7)   =  G(4)
          G(8)   =  G(4)
          G(9)   =  0.5D0
          G(10)  =  G(4)
          G(11)  =  G(9)
          G(12)  =  G(4)
          G(13)  =  G(9)
          G(14)  =  G(4)
          G(15)  =  G(4)
        ELSEIF (FG.EQ.3) THEN
C         X**A * Y**B * Z**C AVEC A+B+C <= 5
          PG(1)  =  8.D0/405.D0
          PG(2)  =  0.0115113678710454D0
          PG(3)  =  PG(2)
          PG(4)  =  PG(2)
          PG(5)  =  PG(2)
          PG(6)  =  0.0119895139631698D0
          PG(7)  =  PG(6)
          PG(8)  =  PG(6)
          PG(9)  =  PG(6)
          PG(10) =  5.D0/567.D0
          PG(11) =  PG(10)
          PG(12) =  PG(10)
          PG(13) =  PG(10)
          PG(14) =  PG(10)
          PG(15) =  PG(10)
          G(1)   =  0.25D0
          G(2)   =  G(1)      
          G(3)   =  G(1)
          G(4)   =  0.3197936278296299D0
          G(5)   =  G(4)
          G(6)   =  G(4)
          G(7)   =  G(4)
          G(8)   =  G(4)
          G(9)   =  0.0406191165111103D0
          G(10)  =  G(4)
          G(11)  =  G(9)
          G(12)  =  G(4)
          G(13)  =  G(9)
          G(14)  =  G(4)
          G(15)  =  G(4)
          G(16)  =  0.09197107805272303D0
          G(17)  =  G(16)
          G(18)  =  G(16)
          G(19)  =  G(16)
          G(20)  =  G(16)
          G(21)  =  0.72408676584183090D0
          G(22)  =  G(16)
          G(23)  =  G(21)
          G(24)  =  G(16)
          G(25)  =  G(21)
          G(26)  =  G(16)
          G(27)  =  G(16)
          G(28)  =  0.056350832689629156D0
          G(29)  =  G(28)
          G(30)  =  0.443649167310370844D0
          G(31)  =  G(28)
          G(32)  =  G(30)
          G(33)  =  G(28)
          G(34)  =  G(30)
          G(35)  =  G(28)
          G(36)  =  G(28)
          G(37)  =  G(28)
          G(38)  =  G(30) 
          G(39)  =  G(30)
          G(40)  =  G(30)
          G(41)  =  G(28)
          G(42)  =  G(30)
          G(43)  =  G(30)
          G(44)  =  G(30)
          G(45)  =  G(28)
        ELSE 
          GOTO 100       
        ENDIF 

C --- PENTAEDRE

      ELSEIF (TYPEMA(1:3).EQ.'PEN') THEN
        IF (FG.EQ.1) THEN
C         X**A * Y**B * Z**C AVEC A+B <= 2 ET C <= 3
          CALL PGAUS1(2,PG1,G1,N1)
          CALL PGAUS2('TRIA    ',3,PG2,G2,N2)
          P1 = 1
          P2 = 1
          DO 10 I = 1, N1
            P3 = 1
            DO 10 J = 1, N2
              PG(P1) = PG1(I) * PG2(J)
              G(P2) = G1(I)
              G(P2+1) = G2(P3)
              G(P2+2) = G2(P3+1)
              P1 = P1 + 1
              P2 = P2 + 3
              P3 = P3 + 2
 10       CONTINUE
        ELSEIF (FG.EQ.2) THEN
C         X**A * Y**B * Z**C AVEC A+B <= 2 ET C <= 3 
          CALL PGAUS1(2,PG1,G1,N1)
          CALL PGAUS2('TRIA    ',4,PG2,G2,N2)
          P1 = 1
          P2 = 1
          DO 20 I = 1, N1
            P3 = 1
            DO 20 J = 1, N2
              PG(P1) = PG1(I) * PG2(J)
              G(P2) = G1(I)
              G(P2+1) = G2(P3)
              G(P2+2) = G2(P3+1)
              P1 = P1 + 1
              P2 = P2 + 3
              P3 = P3 + 2
 20       CONTINUE
        ELSEIF (FG.EQ.3) THEN
C         X**A * Y**B * Z**C AVEC A+B <= 3 ET C <= 3 
          CALL PGAUS1(2,PG1,G1,N1)
          CALL PGAUS2('TRIA    ',5,PG2,G2,N2)
          P1 = 1
          P2 = 1
          DO 30 I = 1, N1
            P3 = 1
            DO 30 J = 1, N2
              PG(P1) = PG1(I) * PG2(J)
              G(P2) = G1(I)
              G(P2+1) = G2(P3)
              G(P2+2) = G2(P3+1)
              P1 = P1 + 1
              P2 = P2 + 3
              P3 = P3 + 2
 30       CONTINUE
        ELSEIF (FG.EQ.4) THEN
C         X**A * Y**B * Z**C AVEC A+B <= 5 ET C <= 5
          CALL PGAUS1(3,PG1,G1,N1)
          CALL PGAUS2('TRIA    ',7,PG2,G2,N2) 
          P1 = 1
          P2 = 1
          DO 40 I = 1, N1
            P3 = 1
            DO 40 J = 1, N2
              PG(P1) = PG1(I) * PG2(J)
              G(P2) = G1(I)
              G(P2+1) = G2(P3)
              G(P2+2) = G2(P3+1)
              P1 = P1 + 1
              P2 = P2 + 3
              P3 = P3 + 2
 40       CONTINUE
        ELSE
          GOTO 100
        ENDIF

C --- HEXAEDRE

      ELSEIF (TYPEMA(1:3).EQ.'HEX') THEN 
        IF (FG.EQ.1) THEN
C         X**A * Y**B * Z**C AVEC A <= 3, B <= 3 ET C <= 3
          CALL PGAUS1(2,PG1,G1,N1)
          P1 = 1
          P2 = 1
          DO 50 I = 1, N1
          DO 50 J = 1, N1
          DO 50 K = 1, N1
            PG(P1) = PG1(I) * PG1(J) * PG1(K)
            G(P2) = G1(I)
            G(P2+1) = G1(J)
            G(P2+2) = G1(K)
            P1 = P1 + 1
            P2 = P2 + 3
 50       CONTINUE   
        ELSEIF (FG.EQ.2) THEN
C         X**A * Y**B * Z**C AVEC A <= 5, B <= 5 ET C <= 5
          CALL PGAUS1(3,PG1,G1,N1)
          P1 = 1
          P2 = 1
          DO 60 I = 1, N1
          DO 60 J = 1, N1
          DO 60 K = 1, N1
            PG(P1) = PG1(I) * PG1(J) * PG1(K)
            G(P2) = G1(I)
            G(P2+1) = G1(J)
            G(P2+2) = G1(K)
            P1 = P1 + 1
            P2 = P2 + 3
 60       CONTINUE   
        ELSEIF (FG.EQ.3) THEN
C         X**A * Y**B * Z**C AVEC A <= 7, B <= 7 ET C <= 7
          CALL PGAUS1(4,PG1,G1,N1)
          P1 = 1
          P2 = 1
          DO 70 I = 1, N1
          DO 70 J = 1, N1
          DO 70 K = 1, N1
            PG(P1) = PG1(I) * PG1(J) * PG1(K)
            G(P2) = G1(I)
            G(P2+1) = G1(J)
            G(P2+2) = G1(K)
            P1 = P1 + 1
            P2 = P2 + 3
 70       CONTINUE    
        ELSE
          GOTO 100
        ENDIF

C --- PYRAMIDE

      ELSEIF (TYPEMA(1:3).EQ.'PYR') THEN 
        IF (FG.EQ.1) THEN
          PG(1)  =  2.D0/15.D0
          PG(2)  =  PG(1)
          PG(3)  =  PG(1)
          PG(4)  =  PG(1)
          PG(5)  =  PG(1)
          G(1)   =  0.5D0
          G(2)   =  0.D0
          G(3)   =  0.1531754163448146D0
          G(4)   =  0.D0
          G(5)   =  G(1)
          G(6)   =  G(3)
          G(7)   = -G(1)
          G(8)   =  0.D0
          G(9)   =  G(3)
          G(10)  =  0.D0
          G(11)  =  G(7)
          G(12)  =  G(3)
          G(13)  =  0.D0
          G(14)  =  0.D0
          G(15)  =  0.6372983346207416D0
        ELSEIF (FG.EQ.2) THEN
          PG(1)  =  0.1024890634400000D0
          PG(2)  =  PG(1)
          PG(3)  =  PG(1)
          PG(4)  =  PG(1)
          PG(5)  =  0.1100000000000000D0
          PG(6)  =  0.1467104129066667D0
          G(1)   =  0.5702963741068025D0
          G(2)   =  0.D0
          G(3)   =  0.1666666666666667D0
          G(4)   =  0.D0
          G(5)   =  G(1)
          G(6)   =  G(3)
          G(7)   = -G(1)
          G(8)   =  0.D0
          G(9)   =  G(3)
          G(10)  =  0.D0
          G(11)  =  G(7)
          G(12)  =  G(3)
          G(13)  =  0.D0
          G(14)  =  0.D0
          G(15)  =  0.8063183038464675D-1
          G(16)  =  0.D0          
          G(17)  =  0.D0
          G(18)  =  0.6098484849057127D0 
        ELSEIF (FG.EQ.3) THEN
          PG(1)  =  0.04925459268750D0
          PG(2)  =  0.03121056262500D0
          PG(3)  =  PG(2)
          PG(4)  =  PG(2)
          PG(5)  =  PG(2)
          PG(6)  =  0.10663554205740D0
          PG(7)  =  7.171281994273545D-4
          PG(8)  =  0.08169940480108D0
          PG(9)  =  PG(8)
          PG(10) =  PG(8)
          PG(11) =  PG(8)
          PG(12) =  0.00360485542649D0
          PG(13) =  PG(12)
          PG(14) =  PG(12)
          PG(15) =  PG(12)
          PG(16) =  0.00895818158664D0
          PG(17) =  PG(16)
          PG(18) =  PG(16)
          PG(19) =  PG(16)
          PG(20) =  0.00201898387500D0
          PG(21) =  PG(20)
          PG(22) =  PG(20)
          PG(23) =  PG(20)
          PG(24) =  2.286237794882217D-5
          PG(25) =  PG(24)
          PG(26) =  PG(24)
          PG(27) =  PG(24)
          G(1)   =  0.D0
          G(2)   =  0.D0
          G(3)   =  0.5D0
          G(4)   =  0.21210450275000D0
          G(5)   =  G(4)
          G(6)   =  0.5D0 
          G(7)   = -G(4)
          G(8)   =  G(4)
          G(9)   =  0.5D0
          G(10)  =  G(7) 
          G(11)  =  G(7)
          G(12)  =  0.5D0
          G(13)  =  G(4)
          G(14)  =  G(7)
          G(15)  =  0.5D0
          G(16)  =  0.D0
          G(17)  =  0.D0
          G(18)  =  0.07579099450000D0
          G(19)  =  0.D0
          G(20)  =  0.D0
          G(21)  =  0.92420900550000D0
          G(22)  =  0.53949290905726D0
          G(23)  =  0.D0
          G(24)  =  0.17359176400000D0
          G(25)  =  0.D0 
          G(26)  =  G(22)
          G(27)  =  G(24)
          G(28)  = -G(22) 
          G(29)  =  0.D0
          G(30)  =  G(24)
          G(31)  =  0.D0
          G(32)  =  G(28)
          G(33)  =  G(24)
          G(34)  =  0.11332356294274D0
          G(35)  =  0.D0
          G(36)  =  0.82640823600000D0
          G(37)  =  0.D0
          G(38)  =  G(34)
          G(39)  =  G(36)
          G(40)  = -G(34) 
          G(41)  =  0.D0
          G(42)  =  G(36)
          G(43)  =  0.D0
          G(44)  =  G(40)
          G(45)  =  G(36)
          G(46)  =  0.58264060051840D0
          G(47)  =  G(46)
          G(48)  = -0.05320644950000D0 
          G(49)  = -G(46)   
          G(50)  =  G(46)
          G(51)  =  G(48)
          G(52)  =  G(49)
          G(53)  =  G(49)
          G(54)  =  G(48)
          G(55)  =  G(46)
          G(56)  =  G(49)
          G(57)  =  G(48)
          G(58)  =  0.55320644950000D0
          G(59)  =  0.D0
          G(60)  =  0.5D0
          G(61)  =  0.D0 
          G(62)  =  G(58)
          G(63)  =  0.5D0
          G(64)  = -G(58)
          G(65)  =  0.D0
          G(66)  =  0.5D0
          G(67)  =  0.D0
          G(68)  =  G(64)
          G(69)  =  0.5D0
          G(70)  = -0.02943415101840D0
          G(71)  =  G(70)
          G(72)  =  1.05320644950000D0
          G(73)  = -G(70) 
          G(74)  =  G(70)
          G(75)  =  G(72)
          G(76)  =  G(73)
          G(77)  =  G(73)
          G(78)  =  G(72)
          G(79)  =  G(70)
          G(80)  =  G(73)
          G(81)  =  G(72)
        ELSE
          GOTO 100
        ENDIF

      ELSE
        GOTO 100
      ENDIF
      
      CALL PGAUSN(TYPEMA,FG,NG)
      GOTO 110

 100  CONTINUE

      CALL UTMESS('F','PGAUS3','FAMILLE NON DISPONIBLE')
      
 110  CONTINUE
           
      END
