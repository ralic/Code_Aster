      SUBROUTINE FACINT(NBPAS,DIM,LONGH,VEC1,VEC2,LONG,S,R,D,U,V,W)
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C    PREPARATION DES MATRICES INTERSPECTRALES POUR FACTORISATION
C ----------------------------------------------------------------------
C          NBPAS : NOMBRE DA PAS DE DISCRETISATION DE LA MATRICE INTERSP
C          DIM   : DIMENSION DE LA MATRICE
C          DIMH  : NOMBRE DE FONCTIONS DECRIVANT LA MATRICE
C    IN  : VEC1  : VECTEUR DES VALEURS DES FONCTIONS AVANT FACTORISATION
C    OUT : VEC2  : VECTEUR DES VALEURS DES FONCTIONS APRES FACTORISATION
C          LONG  : LONGUEUR DES VECTEURS VEC1 ET VEC2
C             S  : MATRICE DE TRAVAIL DE DIMENSION DIM, A FACTORISER
C             R  : MATRICE DE TRAVAIL, RESULTAT DE LA FACTORISATION
C             D  : VECTEUR DE TRAVAIL
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
      INTEGER          DIM , DIMH
      COMPLEX*16 S(DIM,DIM),R(DIM,DIM),U(*),W(*)
      REAL*8     D(DIM),VEC1(LONG),VEC2(LONGH),V(*)
C     ------------------------------------------------------------------
      NBPT1 = NBPAS
      NBPT2 = NBPT1*2
      DO 10 L=1,NBPT1
      ICOMP = 0
        DO 20 J=1,DIM
          DO 30 I=1,DIM
            IF (I .LE. J) THEN
              ICOMP = ICOMP + 1
              IX = L + (ICOMP-1)*NBPT2 + NBPT1
              IY = IX + NBPT1
              S(I,J) = DCMPLX(VEC1(IX),VEC1(IY))
               IF (I .NE. J) THEN
                 S(J,I) = DCONJG (S(I,J))
               ENDIF
            ENDIF
   30     CONTINUE
   20   CONTINUE
        SR = DBLE(S(1,1))
        SI = DIMAG(S(1,1))
        IF (SR .EQ. 0.D0 .AND. SI .EQ. 0.D0) THEN
          R(1,1) = S(1,1)
          DO 40 I=1,DIM
            DO 40 J=1,DIM
              SR = DBLE(S(I,J))
              SI = DIMAG(S(I,J))
              IF( SR .NE. 0.D0 .OR. SI .NE. 0.D0) THEN
                CALL U2MESS('F','ALGORITH3_60')
              ENDIF
              R(I,J) = S(I,J)
   40     CONTINUE
        ELSE
C
C     --- FACTORISATION ---
C
          CALL DIAGHR(DIM,S,DIM,D,R,DIM,U,V,W)
C
          DO 200 J=1,DIM
            UU = 0.D0
            DO 210 I=1,DIM
              AR = DBLE(R(I,J))
              AI =DIMAG(R(I,J))
              UU = AR*AR + AI*AI + UU
  210       CONTINUE
            UU = SQRT(UU)
            DO 220 K=1,DIM
              AZ = DBLE(R(K,J))/UU
              BZ =DIMAG(R(K,J))/UU
              R(K,J) = DCMPLX(AZ,BZ)
  220       CONTINUE
  200     CONTINUE
          DO 230 I=1,DIM
            DO 240 J=1,DIM
              IF (D(J) .LT. 0.D0) THEN
                D(J) = 0.D0
              ENDIF
              R(I,J)=R(I,J) * SQRT(D(J))
  240       CONTINUE
  230     CONTINUE
        ENDIF
        ICO = 0
        DO 100 J=1,DIM
          DO 110 I=1,DIM
            ICO = ICO +1
            IX = L + (ICO-1)*NBPT2 + NBPT1
            IY = IX + NBPT1
            VEC2(IX) = DBLE(R(I,J))
            VEC2(IY) =DIMAG(R(I,J))
  110     CONTINUE
  100   CONTINUE
   10 CONTINUE
      END
