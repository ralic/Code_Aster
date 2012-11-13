C --------- DEBUT COMMONS INTERNES JEVEUX
C TOLE CRP_4 CRP_18 CRS_508
      VOLATILE         IACCE
      VOLATILE         LTYP,    LONG,    DATE,    IADD,    IADM
      VOLATILE         LONO,    HCOD,    CARA,    LUTI,    IMARQ
      VOLATILE         GENR,    TYPE,    DOCU,    ORIG,    RNOM
      VOLATILE         INDIR
      VOLATILE         IUSADI
      VOLATILE         ISZON
      VOLATILE         I4ZON
      VOLATILE         LSZON
      VOLATILE         R8ZON
      VOLATILE         K1ZON
C
      INTEGER          IACCE
      COMMON /IACCED/  IACCE(1)
      INTEGER          LTYP   , LONG   , DATE   , IADD   , IADM   ,
     &                 LONO   , HCOD   , CARA   , LUTI   , IMARQ
      COMMON /IATRJE/  LTYP(1), LONG(1), DATE(1), IADD(1), IADM(1),
     &                 LONO(1), HCOD(1), CARA(1), LUTI(1), IMARQ(1)
      CHARACTER*1      GENR   , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8), TYPE(8), DOCU(2), ORIG(1), RNOM(1)
      INTEGER          INDIR
      COMMON /KINDIR/  INDIR(1)
      INTEGER          IUSADI
      COMMON /KUSADI/  IUSADI(1)
C
      INTEGER          ISZON(1)
      INTEGER*4        I4ZON(1)
      LOGICAL          LSZON(1)
      REAL*8           R8ZON(1)
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      EQUIVALENCE    ( ISZON(1), K1ZON(1), R8ZON(1), LSZON(1), I4ZON(1))
C---------- FIN COMMONS INTERNES JEVEUX
