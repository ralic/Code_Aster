C---------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C TOLE CRP_4
      VOLATILE           ZI4, ZI, ZR, ZC, ZL
      VOLATILE           ZK8, ZK16, ZK24, ZK32, ZK80
C
      INTEGER*4          ZI4
      COMMON  / I4VAJE / ZI4(1)
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16               ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                 ZK32
      CHARACTER*80                                          ZK80
      COMMON  / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C---------- FIN  COMMUNS NORMALISES  JEVEUX ----------------------------
