      SUBROUTINE I3PDM3 ( EPSI,K,DESC,DESCTM,CONEXK,COORDO,PT,DEDANS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           K, DESC(*), DESCTM(*), CONEXK(*)
      REAL*8            EPSI, PT(*), COORDO(*)
      LOGICAL           DEDANS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C     APPARTENANCE DU POINT PT A LA MAILLE 3D K
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  K      : I : -
C IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
C IN  DESCTM : I : -
C IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
C IN  COORDO : R : TABLE GLOBALE DES COORDONEES
C IN  PT     : R : COORDONNEES DU POINTS
C OUT DEDANS : I : REPONSE
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
      INTEGER F,I,J,NBF,NBS,ADESTM,DECF,COL,IRET
      REAL*8  ZERO,UN,DEUX,CS(3,4),A(10,10),FK(4,4),B(10),VAL1,VAL2
      LOGICAL FINI,GAUCHE,FAUX
C
C======================================================================
C
      ADESTM =  DESCTM(DESC(K))
      DEDANS = .TRUE.
      FINI   = .FALSE.
      FAUX   = .FALSE.
      F      =  0
      IRET   =  0
      ZERO   =  0.0D0
      UN     =  1.0D0
      DEUX   =  2.0D0
      NBF    = ZI(ADESTM)
100   CONTINUE
      IF ( .NOT. FINI ) THEN
         F    = F + 1
         DECF = 8 + F
         NBS  = ZI(ADESTM-1 + 2+F)
         DO 110, I = 1, NBS, 1
            COL   = CONEXK(ZI(ADESTM-1 + DECF + 6*(I-1)))
            DO 111, J = 1, 3, 1
               CS(J,I) = COORDO(3*(COL-1) + J)
111         CONTINUE
110      CONTINUE
         IF ( NBS . EQ. 3 ) THEN
            GAUCHE = .FALSE.
         ELSE
            CALL I3TSTF (K,F,DESC,DESCTM,CONEXK,COORDO,GAUCHE,EPSI)
         ENDIF
         IF ( GAUCHE ) THEN
            DO 10, J = 1, 10, 1
               DO 11, I = 1, 9, 1
                  A(I,J) = ZERO
11             CONTINUE
               A(10,J) = UN
               B(J)    = ZERO
10          CONTINUE
            A(1,1) = UN
            B(10)  = UN
            CALL I3AFK2(CS,FK,IRET)
            IF ( IRET .NE. -1 ) THEN
               DO 120, I = 1, 4, 1
                  DO 121, J = 1, 3, 1
                     A(I,J+1) = DEUX*FK(I,J)
121               CONTINUE
120            CONTINUE
               COL = 5
               DO 130, I = 1, 3, 1
                  A(1,COL) = FK(1,I)*FK(1,I)
                  A(2,COL) = FK(1,I)*FK(2,I)+FK(2,I)*FK(1,I)
                  A(3,COL) = FK(1,I)*FK(3,I)+FK(3,I)*FK(1,I)
                  A(4,COL) = FK(1,I)*FK(4,I)+FK(2,I)*FK(3,I)
                  A(4,COL) = FK(3,I)*FK(2,I)+FK(4,I)*FK(1,I)+A(4,COL)
                  A(5,COL) = FK(2,I)*FK(2,I)
                  A(6,COL) = FK(3,I)*FK(3,I)
                  A(7,COL) = FK(4,I)*FK(2,I)+FK(2,I)*FK(4,I)
                  A(8,COL) = FK(4,I)*FK(3,I)+FK(3,I)*FK(4,I)
                  A(9,COL) = FK(4,I)*FK(4,I)
                  COL = COL + 1
                  DO 131, J = I+1, 3, 1
                     A(1,COL) = DEUX*(FK(1,I)*FK(1,J))
                     A(2,COL) = DEUX*(FK(1,I)*FK(2,J)+FK(2,I)*FK(1,J))
                     A(3,COL) = DEUX*(FK(1,I)*FK(3,J)+FK(3,I)*FK(1,J))
                     A(4,COL) = DEUX*(FK(1,I)*FK(4,J)+FK(2,I)*FK(3,J))
                     A(4,COL) = FK(3,I)*FK(2,J)+FK(4,I)*FK(1,J)+A(4,COL)
                     A(4,COL) = A(4,COL)*DEUX
                     A(5,COL) = DEUX*(FK(2,I)*FK(2,J))
                     A(6,COL) = DEUX*(FK(3,I)*FK(3,J))
                     A(7,COL) = DEUX*(FK(4,I)*FK(2,J)+FK(2,I)*FK(4,J))
                     A(8,COL) = DEUX*(FK(4,I)*FK(3,J)+FK(3,I)*FK(4,J))
                     A(9,COL) = DEUX*(FK(4,I)*FK(4,J))
                     COL = COL + 1
131               CONTINUE
130            CONTINUE
               CALL MGAUSS(A,B,10,10,1,ZERO,FAUX)
            ENDIF
            COL  = 1
            VAL1 = ZERO
            IF ( IRET .NE. -1 ) THEN
               DO 140, I = 1, 4, 1
                  A(I,I) = B(COL)
                  COL = COL + 1
                  DO 141, J = I+1, 4, 1
                     A(I,J) = B(COL)
                     A(J,I) = B(COL)
                     COL = COL + 1
141               CONTINUE
140            CONTINUE
            ENDIF
            B(1) = UN
            DO 150, I = 1, 3, 1
               B(I+1) = PT(I)
150            CONTINUE
            DO 160 , J = 1, 4, 1
               DO 161, I = 1, 4, 1
                  VAL1 = VAL1 + B(I)*A(I,J)*B(J)
161            CONTINUE
160         CONTINUE
            COL = CONEXK(ZI(ADESTM-1 + 32+F))
            DO 170, I = 1, 3, 1
               B(I+1) = COORDO(3*(COL-1)+I)
170         CONTINUE
            VAL2 = ZERO
            DO 180 , J = 1, 4, 1
               DO 181, I = 1, 4, 1
                  VAL2 = VAL2 + B(I)*A(I,J)*B(J)
181            CONTINUE
180         CONTINUE
            VAL1   = -VAL1*VAL2
         ELSE
            B(1) =  (CS(2,2)-CS(2,1))*(CS(3,3)-CS(3,1))
            B(1) = -(CS(2,3)-CS(2,1))*(CS(3,2)-CS(3,1)) + B(1)
            B(2) = -(CS(1,2)-CS(1,1))*(CS(3,3)-CS(3,1))
            B(2) =  (CS(1,3)-CS(1,1))*(CS(3,2)-CS(3,1)) + B(2)
            B(3) =  (CS(1,2)-CS(1,1))*(CS(2,3)-CS(2,1))
            B(3) = -(CS(1,3)-CS(1,1))*(CS(2,2)-CS(2,1)) + B(3)
            VAL1 = SQRT(B(1)*B(1)+B(2)*B(2)+B(3)*B(3))
            IF ( ABS(VAL1) .LE. EPSI ) THEN
               IRET = -1
            ELSE
               VAL1 = UN/VAL1
               DO 132, I = 1, 3, 1
                  B(I) = B(I)*VAL1
132            CONTINUE
            ENDIF
            VAL1 = ZERO
            DO 133,I = 1, 3, 1
               VAL1 = VAL1 + B(I)*(PT(I)-CS(I,1))
133         CONTINUE
         ENDIF
         DEDANS = ( VAL1 .LE. ZERO ) .OR. ( ABS(VAL1) .LE. EPSI*EPSI)
         FINI = ((F .EQ. NBF) .OR. (.NOT. DEDANS) .OR. (IRET .EQ. -1))
         GOTO 100
      ENDIF
      IF ( IRET .EQ. -1 ) THEN
         CALL UTDEBM('F','I3PDM3','FACE DEGENEREE')
         CALL UTIMPI('L','MAILLE : ',1,K)
         CALL UTIMPI('S',' FACE : ',1,F)
         CALL UTFINM()
      ENDIF
      END
