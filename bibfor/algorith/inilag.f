      SUBROUTINE  INILAG (FMLI,ICAR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/01/98   AUTEUR CIBHHLB L.BOURHRARA 
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
C***********************************************************************
C    P. RICHARD     DATE 13/10/92
C-----------------------------------------------------------------------
C  BUT:      < INITIALISATION DES MATRICE LAGRANGE-LAGRANGE >
      IMPLICIT REAL*8 (A-H,O-Z)
C
C
C-----------------------------------------------------------------------
C
C NOM----- / /:
C
C FMLI     /I/: FAMILLE DES MATRICE DE LIAISON
C ICAR     /I/: CARACTERISTIQUE DE LA LIAISON
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32  JEXNOM,JEXNUM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C   PARAMETER REPRESENTANT LE NOMBRE MAX DE COMPOSANTE DE LA GRANDEUR
C   SOUS-JACENTE TRAITES
C
      CHARACTER*24  FMLI
      REAL*8 MOINUN,ZERO5
      INTEGER ICAR(4)
C
C-----------------------------------------------------------------------
      DATA MOINUN /-1.0D+00/
      DATA ZERO5 /0.5D+00/
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      NBLIG=ICAR(1)
      IBLO=ICAR(3)
C
      CALL JECROC(JEXNUM(FMLI,IBLO))
      CALL JEECRA(JEXNUM(FMLI,IBLO),'LONMAX',NBLIG*2,' ')
      CALL JEVEUO(JEXNUM(FMLI,IBLO),'E',LDMAT)
C
      DO 10 I=1,NBLIG
        ZR(LDMAT+I-1)=MOINUN
        ZR(LDMAT+NBLIG+I-1)=ZERO5
10    CONTINUE
C
C
C
 9999 CONTINUE
      CALL JEDEMA()
      END
