      SUBROUTINE VPSHIF(LMATK,VALSHI,LMATM,LMATSH)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8                  VALSHI
      INTEGER           LMATK,       LMATM,LMATSH
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 30/06/99   AUTEUR CIBHHPD P.DAVID 
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
C     EFFECTUE LE DECCALAGE SPECTRALE :
C                  MSH =  K - W * M  (W ETANT LE SHIFT)
C     ------------------------------------------------------------------
C IN  LMATK  : IS : ADRESSE ATTRIBUT MATRICE K
C IN  VALSHI : R8 : VALEUR DU DECALAGE
C IN  LMATM  : IS : ADRESSE ATTRIBUT MATRICE M
C IN  LMATSH : IS : ADRESSE ATTRIBUT MATRICE SHIFTEE
C     ------------------------------------------------------------------
C
C     ------ DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  
C
      REAL*8       COEF(2)
      INTEGER      MAT (2)
      CHARACTER*1  TYPE(2)
      CHARACTER*8  NOMDDL
      CHARACTER*24 NMAT(2),NMATSH
C     ------------------------------------------------------------------
      DATA TYPE/'R', 'R'/
      DATA NOMDDL /'        '/
C     ------------------------------------------------------------------
C
C     --- DECALAGE SPECTRAL  K - W * M    (W ETANT LE SHIFT) ---
      COEF(1) = 1.D0
      COEF(2) = -VALSHI
      NMAT (1) = ZK24(ZI(LMATK+1))
      NMAT (2) = ZK24(ZI(LMATM+1))
      NMATSH=ZK24(ZI(LMATSH+1))
      NBCMB   = 2
      CALL MTCOMB(NBCMB,TYPE,COEF,TYPE,NMAT,TYPE(1),NMATSH,NOMDDL,'V')
      END
