      SUBROUTINE NTFCMA(IMATE,IFON)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 05/01/99   AUTEUR KBBHHDB G.DEBRUYNE 
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
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IMATE,IFON(3)
C ----------------------------------------------------------------------
C     OBTENTION DES ADRESSES DES FONCTIONS BETA ET LAMBDA DANS LE
C     MATERIAU CODE IMATE
C IN  IMATE  : ADRESSE DU MATERIAU CODE
C OUT IFON   : ADRESSE RELATIVE DES PARAMETRES BETA ET LAMBDA
C      IFON(1) : ADRESSE RELATIVE DU PARAMETRE BETA OU -1 SI BETA ABSENT
C      IFON(2) : ADRESSE RELATIVE DU PARAMETRE LAMBDA
C      IFON(3) : ADRESSE DU NOM DE LA FONCTION AFFINITE SI THER_HYDR
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER            IPI,K
C ----------------------------------------------------------------------
C PARAMETER ASSOCIE AU MATERIAU CODE
C
      PARAMETER        ( LMAT = 7 , LFCT = 9 )
C DEB ------------------------------------------------------------------
C
      DO 10 K=1,ZI(IMATE+1)
        IF ('THER_NL '.EQ. ZK16(ZI(IMATE)+K-1)(1:8)) THEN
          IPI = ZI(IMATE+2+K-1)
          GOTO 11
        ENDIF
 10   CONTINUE
      GOTO 35
 11   CONTINUE
      IDF = ZI(IPI)+ZI(IPI+1)
      DO 20 K=1,ZI(IPI+2)
        IF('BETA    ' .EQ. ZK8(ZI(IPI+3)+IDF+K-1) ) THEN
          IFON(1) = IPI+LMAT-1+LFCT*(K-1)
          GOTO 25
        ENDIF
 20   CONTINUE
      CALL UTMESS('F','NTFCMA_02','PARAMETRE BETA NON TROUVE')
 25   CONTINUE
      DO 30 K=1,ZI(IPI+2)
        IF('LAMBDA  ' .EQ. ZK8(ZI(IPI+3)+IDF+K-1) ) THEN
          IFON(2) = IPI+LMAT-1+LFCT*(K-1)
          GOTO 75
        ENDIF
 30   CONTINUE
      CALL UTMESS('F','NTFCMA_03','PARAMETRE LAMBDA NON TROUVE')
 35   CONTINUE
      DO 40 K=1,ZI(IMATE+1)
        IF ('THER_HYDR '.EQ. ZK16(ZI(IMATE)+K-1)(1:9)) THEN
          IPI = ZI(IMATE+2+K-1)
          GOTO 41
        ENDIF
 40   CONTINUE
      CALL UTMESS('F','NTFCMA_01','COMPORTEMENT NON TROUVE')
 41   CONTINUE
      IDF = ZI(IPI)+ZI(IPI+1)
      DO 50 K=1,ZI(IPI+2)
        IF('BETA    ' .EQ. ZK8(ZI(IPI+3)+IDF+K-1) ) THEN
          IFON(1) = IPI+LMAT-1+LFCT*(K-1)
          GOTO 55
        ENDIF
 50   CONTINUE
      CALL UTMESS('F','NTFCMA_02','PARAMETRE BETA NON TROUVE')
 55   CONTINUE
      DO 60 K=1,ZI(IPI+2)
        IF('LAMBDA  ' .EQ. ZK8(ZI(IPI+3)+IDF+K-1) ) THEN
          IFON(2) = IPI+LMAT-1+LFCT*(K-1)
          GOTO 65
        ENDIF
 60   CONTINUE
      CALL UTMESS('F','NTFCMA_03','PARAMETRE LAMBDA NON TROUVE')
 65   CONTINUE
      DO 70 K=1,ZI(IPI+2)
        IF('AFFINITE  ' .EQ. ZK8(ZI(IPI+3)+IDF+K-1) ) THEN
          IFON(3) = IPI+LMAT-1+LFCT*(K-1)
          GOTO 75
        ENDIF
 70   CONTINUE
      CALL UTMESS('F','NTFCMA_03','PARAMETRE AFFINITE NON TROUVE')
 75   CONTINUE

C FIN ------------------------------------------------------------------
      END
