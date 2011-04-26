      SUBROUTINE CAVEAS (CHARGZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C       CAVEAS -- TRAITEMENT DU MOT CLE VECT_ASSE
C
C      TRAITEMENT DU MOT CLE VECT_ASSE DE AFFE_CHAR_MECA
C      CE MOT CLE PERMET DE SPECIFIER UN VECTEUR ASSEMBLE (UN CHAM_NO)
C      QUI SERVIRA DE SECOND MEMBRE DANS STAT_NON_LINE
C                                   OU   DYNA_NON_LINE
C
C -------------------------------------------------------
C  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
C                - JXVAR -      -   LA  CHARGE EST ENRICHIE
C                                   DU VECTEUR ASSEMBLE DONT LE NOM
C                                   EST STOCKE DANS L'OBJET
C                                   CHAR//'CHME.VEASS'
C -------------------------------------------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ------
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
      COMMON  / KVARJE /ZK8(1),ZK16(1),ZK24(1),ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ------
C
C -----  ARGUMENTS
      CHARACTER*(*) CHARGZ
C ------ VARIABLES LOCALES
      CHARACTER*8   CHARGE, VECASS
      CHARACTER*24  OBJ
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
      CALL GETVID(' ','VECT_ASSE',0,1,1,VECASS,NVECAS)
      IF (NVECAS.EQ.0) GOTO 9999
C
      CALL CHPVER('F',VECASS,'NOEU','DEPL_R',IER)
C
      CHARGE = CHARGZ
      OBJ    = CHARGE//'.CHME.VEASS'
C
      CALL WKVECT(OBJ,'G V K8',1,IDVEAS)
      ZK8(IDVEAS) = VECASS
C
9999  CONTINUE
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
