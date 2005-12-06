      SUBROUTINE FFT(S,N,IFFT)
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
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16       S(N)
C-----------------------------------------------------------------------
C IN,OUT : S    FONCTION A TRANSFORMER
C IN     : N    NOMBRE DE POINTS DE LA FONCTION
C IN     : IFFT > 0 => FFT
C               < 0 => FFT INVERSE
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
      COMPLEX*16       U,W,T
C     ------------------------------------------------------------------
      M= INT(LOG(DBLE(N))/LOG(2.D0))
      IF (M.GT.30) CALL VERI32()
      N2 = 2**M
       IF(N2 .NE. N) THEN
         M = M+1
      IF (M.GT.30) CALL VERI32()
         N2 = 2**M
          IF(N2 .NE. N) THEN
            M = M-2
          ENDIF
       ENDIF
      ISGN = 1
      IF (IFFT .LT. 0) ISGN=-1
      PI= R8PI()*ISGN
      NM1=N-1
      J = 1
      NV2=N/2
      DO 8 I=1,NM1
        IF(I.GE.J) GOTO 5
          T=S(J)
          S(J)=S(I)
          S(I)=T
    5     CONTINUE
          K=NV2
    6   CONTINUE
        IF(K.GE.J) GOTO 7
          J=J-K
          K=K/2
          GOTO 6
    7 CONTINUE
      J=J+K
    8 CONTINUE
      DO 20 L=1,M
      IF (L.GT.30) CALL VERI32()
        LE=2**L
        LE1=LE/2
        U=(1.D0,0.D0)
        W=DCMPLX(COS(-PI/DBLE(LE1)),SIN(-PI/DBLE(LE1)))
        DO 20 J=1,LE1
          DO 10 I=J,N,LE
            IP=I+LE1
            T=S(IP)*U
            S(IP)=S(I)-T
          S(I)=S(I)+T
   10     CONTINUE
        U=U*W
   20   CONTINUE
        IF (IFFT .LT. 0) THEN
          DO 30 I=1,N2
            S(I) = S(I)/N2
   30     CONTINUE
        ENDIF
 9999   CONTINUE
        END
