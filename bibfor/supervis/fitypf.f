      SUBROUTINE FITYPF ( NOMF, ITYPFO )
      IMPLICIT  NONE
      CHARACTER*8         NOMF
      INTEGER             ITYPFO
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 18/09/2002   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C IN  : NOMF   : NOM DE LA FONCTION DONT ON VEUT RECUPERER LE TYPE
C OUT : ITYPFO : TYPE RECUPERE
C                SUIVANT LE TYPE LU (VOIR LA ROUTINE FIPREP)
C                TYPE = 'EVAL'       , ITYPFO = 0
C                TYPE = 'FONCTION_R' , ITYPFO = 32
C                TYPE = 'FONCTION_I' , ITYPFO = 31 
C                TYPE = 'FONCTION_C' , ITYPFO = 35
C                TYPE = 'FONCTION'   , ITYPFO = 0
C
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
C     ------------------------------------------------------------------
      INTEGER  ICSTE , IOPE1, IOPE2, LOPE1, LOPE2, I,
     +         IPARG,IPARD,IVIRG,IEGAL,IPTVI,ILOAD,IPLUS,IFONC,
     +         LNOMOP, LARITE, LPRIOR, LCLASS, LVALIS, LVALR8, LVALC8
      CHARACTER*24
     +         KNOMOP, KARITE, KPRIOR, KCLASS, KVALIS, KVALR8, KVALC8
C     ------------------------------------------------------------------
      COMMON /FICK01/
     +        KNOMOP, KARITE, KPRIOR, KCLASS, KVALIS, KVALR8, KVALC8
      COMMON /FICI01/
     +        LNOMOP, LARITE, LPRIOR, LCLASS, LVALIS, LVALR8, LVALC8
      COMMON /FICL01/ ICSTE , IOPE1, IOPE2, LOPE1, LOPE2
      COMMON /FISY01/ IPARG,IPARD,IVIRG,IEGAL,IPTVI,ILOAD,IPLUS,IFONC
C     ------------------------------------------------------------------
C
      ITYPFO = 0
      DO 10 I = 1, ICSTE
         IF ( ZK8(LNOMOP-1+I) .EQ. NOMF ) THEN
            ITYPFO = ZI(LCLASS-1+I)
         ENDIF
 10   CONTINUE
C
      END
