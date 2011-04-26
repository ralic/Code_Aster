        SUBROUTINE BETIMP (NMAT,MATER,SIG,VIND,VINF,ELGEOM,
     &                     NSEUI1,NSEUI2,NSEUI3,NSEUI4,SIGE,SIGD)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C       ----------------------------------------------------------------
C       BETON_DOUBLE_DP: CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,P1,P2)
C            AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
C            SEUILC = FCOMP      = (SIGEQ   + A  SIGH)/B - FC
C            SEUILT = FTRAC      = (SIGEQ   + C  SIGH)/D - FT
C                AVEC SIGEQ      = SQRT(3/2(D) (D)) (CONTR EQUIVALENTE)
C                     D          = SIG - 1/3 TR(SIG) I
C                     SIGH       = 1/3 TR(SIG)    (CONTR HYDROSTATIQUE)
C       IMPRESSION DE VALEURS EN CAS DE NON CONVERGENCE
C       ----------------------------------------------------------------
C       NSEUIL = 1  --> CRITERE  EN COMPRESSION ACTIVE
C       NSEUIL = 2  --> CRITERE  EN TRACTION ACTIVE
C       NSEUIL = 3  --> CRITERES EN COMPRESSION ET EN TRACTION ACTIVE
C       NSEUIL = 11 --> PROJECTION AU SOMMET DU CONE DE COMPRESSION
C       NSEUIL = 22 --> PROJECTION AU SOMMET DU CONE DE TRACTION
C       NSEUIL = 33 --> PROJECTION AU SOMMET DES CONES DE COMPRESSION
C                       ET TRACTION
C       ----------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VIND   :  VARIABLES INTERNES = ( PC PT THETA ) A T
C       IN  VINF   :  VARIABLES INTERNES = ( PC PT THETA ) A T+DT
C       IN  NMAT   :  DIMENSION MATER
C       IN  MATER  :  COEFFICIENTS MATERIAU
C       IN  ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES AUX
C                     LOIS DE COMPORTEMENT
C       IN  NSEUI1 :  CRITERE ACTIVE LORS DE LA PREMIERE RESOLUTION
C       IN  NSEUI2 :  CRITERE ACTIVE LORS DE LA DEUXIEME RESOLUTION
C       IN  NSEUI3 :  CRITERE ACTIVE LORS DE LA TROISIEME RESOLUTION
C       IN  NSEUI4 :  CRITERE ACTIVE LORS DE LA QUATRIEME RESOLUTION
C       IN  SIGE   :  CONTRAINTE ELASTIQUE
C       IN  SIGD   :  CONTRAINTE A L'INSTANT PRECEDENT
C       ----------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
        INTEGER         NMAT , NSEUI4, IFM, NIV
        INTEGER         NSEUI1, NSEUI2, NSEUI3
        REAL*8          PC, PT, SIG(6), SIGE(6), SIGD(6), DEV(6), LC
        REAL*8          MATER(NMAT,2),ELGEOM(*), VIND(*), VINF(*)
        REAL*8          FC , FT , BETA
        REAL*8          RAC2 , ZERO , UN , DEUX , TROIS
        REAL*8           KE , FCOMP , FTRAC
        REAL*8          A, B, C, D
        REAL*8          SIGEQ , SIGH, P, DFCDLC, DFTDLT, KUC, KUT
        REAL*8           D13, DLAMBC, DLAMBT
        INTEGER           IADZI, IAZK24
        CHARACTER*8     NOMAIL
C       ---------------------------------------------------------------
        INTEGER         NDT  , NDI
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
        DATA   D13      /.33333333333333D0 /
        DATA   ZERO     / 0.D0 /
        DATA   UN       / 1.D0 /
        DATA   DEUX     / 2.D0 /
        DATA   TROIS    / 3.D0 /
        RAC2  = SQRT (DEUX)
C
        CALL INFNIV(IFM,NIV)
        IF (NIV.EQ.2) THEN
        ENDIF
C
        CALL TECAEL ( IADZI, IAZK24 )
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
C
C ---   IMPRESSION GENERALE
C
C
C ---   CARACTERISTIQUES MATERIAU
C
        BETA   = MATER(3,2)
C
        A = RAC2 * (BETA - UN) / (DEUX * BETA - UN)
        B = RAC2 / TROIS * BETA / (DEUX * BETA - UN)
        C = RAC2
        D = DEUX * RAC2 / TROIS
C
C
C --- LONGUEUR CARACTERISTIQUE POUR LOI BETON LC
C
      IF(MATER(9,2).LT.ZERO) THEN
         LC = ELGEOM(1)
      ELSE
         LC = MATER(9,2)
      ENDIF
C
 1000   FORMAT(A43)
 1001   FORMAT(A43,A8)
 1002   FORMAT(A43,1PD12.5)
 1003   FORMAT(4(A9,I3))
C
C ---   TRAITEMENT DE LA CONTRAINTE ELASTIQUE
C
        CALL LCDEVI ( SIGE , DEV )
        CALL LCPRSC ( DEV , DEV , P)
        SIGEQ = SQRT (1.5D0 * P)
C
        CALL LCHYDR ( SIGE , SIGH )
C
        PC  = VIND(1)
        PT  = VIND(2)
        CALL BETFPP ( MATER, NMAT, ELGEOM, PC, PT, 3, FC, FT,
     &                DFCDLC, DFTDLT, KUC, KUT, KE)
C
        FCOMP = (RAC2 * D13 * SIGEQ + A * SIGH) / B - FC
        FTRAC = (RAC2 * D13 * SIGEQ + C * SIGH) / D - FT
C
        WRITE(IFM,1000) ' -----------------------------------------  '
        WRITE(IFM,1000) ' NON CONVERGENCE DE LA LOI BETON_DOUBLE_DP '
        WRITE(IFM,1000) ' -----------------------------------------  '
        WRITE(IFM,1001) 'MAILLE :                                   ',
     &                   NOMAIL
        WRITE(IFM,1002) 'LONGUEUR CARACTERISTIQUE :                 ',
     &                   LC
        WRITE(IFM,1002) 'CONTRAINTE ELASTIQUE EQUIVALENTE :         ',
     &                   SIGEQ
        WRITE(IFM,1002) 'CONTRAINTE ELASTIQUE HYDROSTATIQUE :       ',
     &                   SIGH
        WRITE(IFM,1002) 'ECROUISSAGE EN COMPRESSION :               ',
     &                   PC
        WRITE(IFM,1002) 'ECROUISSAGE EN TRACTION :                  ',
     &                   PT
        WRITE(IFM,1002) 'RESISTANCE COMPRESSION AVANT ECROUISSAGE : ',
     &                   FC
        WRITE(IFM,1002) 'RESISTANCE TRACTION AVANT ECROUISSAGE :    ',
     &                   FT
        WRITE(IFM,1002) 'ECROUISSAGE ULTIME EN COMPRESSION :        ',
     &                   KUC
        WRITE(IFM,1002) 'ECROUISSAGE AU PIC EN COMPRESSION :        ',
     &                   KE
        WRITE(IFM,1002) 'ECROUISSAGE ULTIME EN TRACTION :           ',
     &                   KUT
        WRITE(IFM,1002) 'VALEUR DU CRITERE DE COMPRESSION :         ',
     &                   FCOMP
        WRITE(IFM,1002) 'VALEUR DU CRITERE DE TRACTION :            ',
     &                   FTRAC
C
C ---   TRAITEMENT DE LA CONTRAINTE (NON ELASTIQUE) A L'INSTANT MOINS
C
        CALL LCDEVI ( SIGD , DEV )
        CALL LCPRSC ( DEV , DEV , P)
        SIGEQ = SQRT (1.5D0 * P)
C
        CALL LCHYDR ( SIGD , SIGH )
C
        WRITE(IFM,1002) 'CONTRAINTE EQUIVALENTE A L INSTANT MOINS : ',
     &                   SIGEQ
        WRITE(IFM,1002) 'CONTRAINTE HYDROSTATIQUE A L INSTANT MOINS:',
     &                   SIGH
C
C ---   TRAITEMENT DE LA CONTRAINTE ELASTO PLASTIQUE
C
        CALL LCDEVI ( SIG , DEV )
        CALL LCPRSC ( DEV , DEV , P)
        SIGEQ = SQRT (1.5D0 * P)
C
        CALL LCHYDR ( SIG , SIGH )
C
        PC  = VINF(1)
        PT  = VINF(2)
        CALL BETFPP ( MATER, NMAT, ELGEOM, PC, PT, 3, FC, FT,
     &                DFCDLC, DFTDLT, KUC, KUT, KE)
C
        FCOMP = (RAC2 * D13 * SIGEQ + A * SIGH) / B - FC
        FTRAC = (RAC2 * D13 * SIGEQ + C * SIGH) / D - FT
        DLAMBC = VINF(1) - VIND(1)
        DLAMBT = VINF(2) - VIND(2)
C
        WRITE(IFM,1003) 'NSEUI1 : ',NSEUI1,' NSEUI2 : ',NSEUI2,
     &                 ' NSEUI3 : ',NSEUI3,' NSEUI4 : ',NSEUI4
        WRITE(IFM,1002) 'CONTRAINTE EQUIVALENTE A L INSTANT PLUS :  ',
     &                   SIGEQ
        WRITE(IFM,1002) 'CONTRAINTE HYDROSTATIQUE A L INSTANT PLUS :',
     &                   SIGH
        WRITE(IFM,1002) 'ECROUISSAGE EN COMPRESSION A L INST. PLUS :',
     &                   PC
        WRITE(IFM,1002) 'ECROUISSAGE EN TRACTION A L INSTANT PLUS:  ',
     &                   PT
        WRITE(IFM,1002) 'RESISTANCE COMPRESSION APRES ECROUISSAGE : ',
     &                   FC
        WRITE(IFM,1002) 'RESISTANCE TRACTION APRES ECROUISSAGE :    ',
     &                   FT
        WRITE(IFM,1002) 'VALEUR DU CRITERE DE COMPRESSION :         ',
     &                   FCOMP
        WRITE(IFM,1002) 'VALEUR DU CRITERE DE TRACTION :            ',
     &                   FTRAC
        WRITE(IFM,1002) 'INCREMENT DE DEFO. PLAS. EN COMPRESSION :  ',
     &                   DLAMBC
        WRITE(IFM,1002) 'INCREMENT DE DEFO. PLAS. EN TRACTION :     ',
     &                   DLAMBT
        WRITE(IFM,1000) ' -----------------------------------------  '
C
C
        END
