      SUBROUTINE I3TSTF ( K, F, DESC, DESCTM, CONEXK, COORDO, GAUCHE,
     +                          EPSI )
      IMPLICIT  NONE
      INTEGER             K, F, DESC(*), DESCTM(*), CONEXK(*)
      REAL*8              COORDO(*)
      LOGICAL             GAUCHE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 19/11/96   AUTEUR CIBHHGB G.BERTRAND 
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
C     TEST DE GAUCHITUDE POUR QADRANGLE
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER     SM(4), I, J, DECF, ADESCM
      REAL*8      A(3,3), D1, A1, A2, A3, AIRE, EPSI
C
C======================================================================
C
      DECF   = 8 + F
      ADESCM = DESCTM(DESC(K))
      DO 10, I = 1, 4, 1
         SM(I) = CONEXK(ZI(ADESCM-1 + DECF + (I-1)*6))
 10   CONTINUE
C
      DO 20, I = 1, 3, 1
         DO 22, J = 1, 3, 1
            A(I,J) = COORDO(3*(SM(J+1)-1)+I)-COORDO(3*(SM(1)-1)+I)
 22      CONTINUE
 20   CONTINUE
C
      D1 = A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))
      D1 = A(2,1)*(A(1,3)*A(3,2)-A(1,2)*A(3,3)) + D1
      D1 = A(3,1)*(A(1,2)*A(2,3)-A(2,2)*A(1,3)) + D1
C
      A1 = A(1,1)*A(2,2) - A(2,1)*A(1,2)
      A2 = A(2,1)*A(3,2) - A(3,1)*A(2,2)
      A3 = A(3,1)*A(1,2) - A(1,1)*A(3,2)
      AIRE = 0.5D0 * SQRT( A1*A1 + A2*A2 + A3*A3 )
      A1 = A(1,2)*A(2,3) - A(2,2)*A(1,3)
      A2 = A(2,2)*A(3,3) - A(3,2)*A(2,3)
      A3 = A(3,2)*A(1,3) - A(1,2)*A(3,3)
      AIRE = AIRE + 0.5D0 * SQRT( A1*A1 + A2*A2 + A3*A3 )
      EPSI = ( AIRE ) ** 1.5D0
      GAUCHE = ( ABS(D1) .GT. EPSI )
C
      END
