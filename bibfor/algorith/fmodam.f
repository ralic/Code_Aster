      SUBROUTINE FMODAM(NEQ,VITE,VALMOD,BASMOD,FORCE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*24 VALMOD,BASMOD
      REAL*8 VITE(NEQ),FORCE(NEQ)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/04/98   AUTEUR GJBHHEL E.LORENTZ 
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
      CHARACTER*32     JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*6  PGC
      CHARACTER*8  K8B
      CHARACTER*8  MODMEC, MAILLA
      CHARACTER*14 NUMDDL
      CHARACTER*24 DEEQ
      CHARACTER*24 KBID, MATRIC, NOMCHA
      REAL*8       AMOR, MASGEN, PULS, SOMME
      DATA PGC/'FMODAM'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL JELIRA(VALMOD,'LONMAX',NBMODE,KBID)
      NBMODE = NBMODE/3
      CALL JEVEUO(VALMOD,'L',JVALMO)
      CALL JEVEUO(BASMOD,'L',JBASMO)
      DO 1 I=1,NEQ
        FORCE(I) = 0.D0
 1    CONTINUE
      DO 10 I=1,NBMODE
        MASGEN = ZR(JVALMO+3*(I-1)+1-1) 
        PULS = ZR(JVALMO+3*(I-1)+2-1) 
        AMOR = ZR(JVALMO+3*(I-1)+3-1)
        SOMME = 0.D0
        DO 11 N1=1,NEQ
            SOMME = SOMME + ZR(JBASMO+(I-1)*NEQ+N1-1)*VITE(N1)
 11     CONTINUE 
        DO 12 N1=1,NEQ
            FORCE(N1) = FORCE(N1) + 2.D0*AMOR/MASGEN/PULS**3
     &      *ZR(JBASMO+(I-1)*NEQ+N1-1)*SOMME
 12     CONTINUE
 10   CONTINUE
C
      CALL JEDETC('V','&&'//PGC,1)
C
      CALL JEDEMA()
      END
