      SUBROUTINE DEPRCV(PARCRI, RESASS, CONVPR, CONVDE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/05/2000   AUTEUR VABHHTS J.PELLET 
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
      LOGICAL      CONVPR
      REAL*8       PARCRI(*), CONVDE(*)
      CHARACTER*24 RESASS

C ----------------------------------------------------------------------
C        EVALUATION DE LA CONVERGENCE DU PROBLEME PRIMAL (SUR LE RESIDU)
C ----------------------------------------------------------------------
C IN  PARCRI  CRITERES DE CONVERGENCE
C                  10 RESI_PRIM_ABSO
C                  11 RESI_DUAL_ABSO
C IN  RESASS  RESIDU ASSEMBLE
C OUT CONVPR  VRAI SI CONVERGE
C OUT CONVDE  R   VALEURS DE CONVERGENCE DE L'ALGORITHME
C                  1 - RESI_DUAL_ABSO
C                  2 - RESI_PRIM_ABSO
C                  3 - NOMBRE D'ITERATIONS DUAL
C                  4 - NUMERO ITERATION BFGS
C-----------------------------------------------------------------------
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      CHARACTER*32       JEXNUM
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------

      REAL*8       RMAX, R8VIDE
      INTEGER      NDDL, IAD, I
      CHARACTER*1  K1BID


C    INITIALISATION
      CALL JEMARQ()
      RMAX = 0.D0


C    RECUPERATION DU CHAMP RESIDU ASSEMBLE

      CALL JELIRA(RESASS(1:19) // '.VALE','LONMAX',NDDL,K1BID)
      CALL JEVEUO(RESASS(1:19) // '.VALE','L',IAD)

      DO 10 I=1, NDDL
        IF ( ABS(ZR(IAD-1 + I)).GT.RMAX ) THEN
          RMAX = ABS(ZR(IAD-1 + I))
        END IF
10    CONTINUE

      CONVPR = RMAX.LT.PARCRI(10)
      CONVDE(2) = RMAX

      CALL JEDEMA()
      END
