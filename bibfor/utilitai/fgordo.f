      SUBROUTINE FGORDO(NBEXTR,EXT,ORD)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8                   EXT(*),ORD(*)
      INTEGER           NBEXTR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/03/95   AUTEUR D6BHHAM A.M.DONORE 
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
C     ----------------------------------------------------------------
C     RANGE LES EXTREMAS PAR AMPLITUDE DECROISSANTE
C     -----------------------------------------------------------------
C IN  NBEXTR : I   : NOMBRE  D'EXTREMUM DE LA FONCTION
C IN  EXT    : R   : VALEURS DES EXTREMA
C OUT ORD    : R   : VALEURS DES EXTREMA REORDONNES
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      IF (EXT(1).LT.EXT(2)) THEN
        ORD(1)=EXT(1)
        ORD(2)=EXT(2)
      ELSE
        ORD(1)=EXT(2)
        ORD(2)=EXT(1)
      ENDIF
C
      DO 1 I=3,NBEXTR
        IF (EXT(I).LT.ORD(1)) THEN
           DO 31 K=I,2,-1
              ORD(K)=ORD(K-1)
 31        CONTINUE
           ORD(1)=EXT(I)
           GOTO 1
        ENDIF
        IF (EXT(I).GE.ORD(I-1)) THEN
          ORD(I)=EXT(I)
          GOTO1
        ENDIF
        DO 2 J=1,I-2
          IF ((ORD(J).LE.EXT(I)).AND.(EXT(I).LT.ORD(J+1))) THEN
             DO 3 K=I,J+2,-1
               ORD(K)=ORD(K-1)
 3           CONTINUE
             ORD(J+1)=EXT(I)
             GOTO 1
          ENDIF
 2      CONTINUE
 1    CONTINUE
C
      END
