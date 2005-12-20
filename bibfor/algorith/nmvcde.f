      SUBROUTINE NMVCDE(INDEX,COMZ,EXI)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/08/1999   AUTEUR GJBHHEL E.LORENTZ 
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

      IMPLICIT NONE

      CHARACTER*4   INDEX
      CHARACTER*(*) COMZ
      CHARACTER*14  COM
      LOGICAL       EXI


C ----------------------------------------------------------------------
C  TEST SI UNE VARIABLE DE COMMANDE EST SIGNIFICATIVE (PAS UN DEFAUT)
C ----------------------------------------------------------------------
C IN   INDEX   K4  INDEX DE LA VARIABLE DE COMMANDE
C IN   COM     K14 SD VARI_COM
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


      INTEGER  IEX


      CALL JEMARQ()
      COM = COMZ

      CALL JEVEUO(COM // '.EXISTENCE','L',IEX)

      IF      (INDEX.EQ.'TEMP') THEN
        EXI = ZL(IEX+0)
      ELSE IF (INDEX.EQ.'HYDR') THEN
        EXI = ZL(IEX+1)
      ELSE IF (INDEX.EQ.'SECH') THEN
        EXI = ZL(IEX+2)
      ELSE
        EXI = .TRUE.
      END IF

      CALL JEDEMA
      END
