      SUBROUTINE NMVCD2(INDEX,CHMAT,EXI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/04/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE

      CHARACTER*4   INDEX
      CHARACTER*(*) CHMAT
      LOGICAL       EXI


C ------------------------------------------------------------------
C  TEST SI UNE VARIABLE DE COMMANDE NOUVEAU FORMAT EST SIGNIFICATIVE 
C ------------------------------------------------------------------
C IN   INDEX   K4  INDEX DE LA VARIABLE DE COMMANDE
C IN   CHMAT   K*  SD CHMAT
C OUT  EXI      L  TRUE SI LA VARIABLE DE COMMANDE N'EST PAS UN DEFAUT
C ----------------------------------------------------------------------

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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


      INTEGER  NMAX,IVRC,I,IRET
      CHARACTER*1 K1BID

      CALL JEMARQ()
      EXI=.FALSE.
      CALL JEEXIN(CHMAT(1:8)// '.CVRCVARC',IRET)
      IF ( IRET.NE.0) THEN
        CALL JELIRA(CHMAT(1:8)// '.CVRCVARC','LONMAX',NMAX,K1BID)
        CALL JEVEUO(CHMAT(1:8)// '.CVRCVARC','L',IVRC)
        DO 1 I=1,NMAX
          IF (ZK8(IVRC-1+I)(1:4).EQ.INDEX) EXI=.TRUE.
1       CONTINUE
      ENDIF
      CALL JEDEMA
      END
