      SUBROUTINE I3TSTF ( K, F, DESC, DESCTM, CONEXK, COORDO, GAUCHE,
     +                          EPSI )
      IMPLICIT  NONE
      INTEGER             K, F, DESC(*), DESCTM(*), CONEXK(*)
      REAL*8              COORDO(*), EPSI
      LOGICAL             GAUCHE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 17/12/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     TEST DE GAUCHITUDE POUR QUADRANGLE
C     ------------------------------------------------------------------
C IN  K      : I : -
C IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
C IN  DESCTM : I : -
C IN  F      : I : NUMERO LOCALE DE LA FACE TRAITEE
C IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
C IN  COORDO : R : TABLE GLOBALE DES COORDONEES
C OUT GAUCHE : R : REPONSE
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER     DS1, SM(4), I, J, DECF, ADESCM
      REAL*8      TOLE
      REAL*8      E1(3),E2(3),E3(3),CS(3,4), LCARA, C, T, S, R, ZERO
C
C======================================================================
C
C --- RECUPERATION DES NOEUDS SOMMET DE LA FACE ET DE SES COORDONNEES
C
      DECF   = 8 + F
      ADESCM = DESCTM(DESC(K))
      DO 10, I = 1, 4, 1
         DS1   = CONEXK(ZI(ADESCM-1 + DECF + (I-1)*6))
         SM(I) = CONEXK(ZI(ADESCM-1 + DECF + (I-1)*6))
         DO 11, J = 1, 3, 1
            CS(J,I) = COORDO(3*(DS1-1) + J)
11       CONTINUE
 10   CONTINUE
C
C --- DEFINITION DU REPERE LOCAL DE LA FACE
C     E1 : DEFINIT PAR L'ARETE N1 N2
C     E2 : DEFINIT PAR L'ARETE N1 N4
C     E3 : PERPENDICULAIRE A E1 E2
C
      ZERO  = 0.0D0
      C     = ZERO
      T     = ZERO
      DO 15, I = 1, 3, 1
         S     = CS(I,2) - CS(I,1)
         R     = CS(I,4) - CS(I,1)
         E1(I) = S
         E2(I) = R
         C     = C + S*S
         T     = T + R*R
15    CONTINUE
      C = SQRT(C)
      T = SQRT(T)
      DO 16, I = 1, 3, 1
         E1(I) = E1(I)/C
         E2(I) = E2(I)/T
16    CONTINUE
      E3(1) = E1(2)*E2(3) - E1(3)*E2(2)
      E3(2) = E1(3)*E2(1) - E1(1)*E2(3)
      E3(3) = E1(1)*E2(2) - E1(2)*E2(1)
      C = ZERO
      DO 17, I = 1, 3, 1
         C = C + E3(I)*E3(I)
17    CONTINUE
      E3(1) =  E3(1)/C
      E3(2) =  E3(2)/C
      E3(3) =  E3(3)/C
C
C --- LCARA : LONGUEUR DE LA PLUS PETITE ARETE
C     AFIN DE DEFINIR UNE PRECISION RELATIVE
C
      LCARA = 1.D+50
      DO 20, J = 1, 3, 1
         C = ZERO
         DO 22, I = 1, 3, 1
            S = COORDO(3*(SM(J+1)-1)+I)-COORDO(3*(SM(J)-1)+I)
            C = C + S*S
 22      CONTINUE
         C = SQRT( C ) 
         LCARA = MIN ( C, LCARA ) 
 20   CONTINUE
      C = ZERO
      DO 24, I = 1, 3, 1
         S = COORDO(3*(SM(4)-1)+I)-COORDO(3*(SM(1)-1)+I)
         C = C + S*S
 24   CONTINUE
      C = SQRT( C ) 
      LCARA = MIN ( C, LCARA ) 
C
C --- FACE GAUCHE A UNE TOLERANCE PRES
C
      T = ZERO
      DO 80, I = 1, 3, 1 
        T = T + ( (CS(I,3)-CS(I,1)) * E3(I) )
 80   CONTINUE
      TOLE = LCARA * EPSI
      GAUCHE = ( ABS(T) .GT. TOLE )
C
      END
