        SUBROUTINE BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, NSEUIL, FC,
     &                      FT, DFCDLC, DFTDLT, KUC, KUT, KE)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C       ----------------------------------------------------------------
C       BETON_DOUBLE_DP: CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,P1,P2)
C                   AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
C       CALCUL DES VALEURS DES COURBES D'ADOUCISSEMENT ET DES DERIVES
C       PAR RAPPORT AUX INCREMENTS DE MULTIPLICATEURS PLASTIQUES
C       IN  MATERF :  COEFFICIENTS MATERIAU
C           NMAT   :  DIMENSION MATERF
C           ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
C                     AUX LOIS DE COMPORTEMENT
C           PC     :  MULTIPLICATEUR PLASTIQUE EN COMPRESSION
C           PT     :  MULTIPLICATEUR PLASTIQUE EN TRACTION
C           NSEUIL :  SEUIL D'ELASTICITE ACTIVE
C                     NSEUIL = 1  -->  SEUIL COMPRESSION ACTIF
C                     NSEUIL = 2  -->  SEUIL TRACTION ACTIF
C                     NSEUIL = 3  -->  SEUIL COMPRESSION ET TRACTION
C                                                             ACTIFS
C       OUT FC     :  ECCROUISSAGE EN COMPRESSION
C           FT     :  ECCROUISSAGE EN TRACTION
C           DFCDLC :  DERIVE DE LA COURBE D'ADOUCISSEMENT EN COMPRESSION
C           DFTDLT :  DERIVE DE LA COURBE D'ADOUCISSEMENT EN TRACTION
C           KUC    :  ECROUISSAGE ULTIME EN COMPRESSION
C           KUT    :  ECROUISSAGE ULTIME EN TRACTION
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
      REAL*8          UN  , ZERO , D13 , DEUX
      PARAMETER       ( UN    = 1.D0   )
      PARAMETER       ( DEUX  = 2.D0   )
      PARAMETER       ( ZERO  = 0.D0   )
      PARAMETER       ( D13   =  .33333333333333D0 )
C
      INTEGER         NMAT, NSEUIL
      REAL*8          MATERF(NMAT,2), ELGEOM(*)
      REAL*8          PC, PT, DFCDLC, DFTDLT, KUC, KUT
C
      REAL*8          LC, FCP , FTP , FC , FT
      REAL*8          GC , GT , CELAS
      REAL*8          E , KU , KE
      REAL*8          LC0, EPSI
      INTEGER         TYPCOM, TYPTRA, IADZI, IAZK24
      CHARACTER*8     NOMAIL
C     ------------------------------------------------------------------
      INTEGER         N , ND
      COMMON /TDIM/   N , ND
      DATA   EPSI     /1.D-6/
C     ------------------------------------------------------------------
C
C --- INITIALISATION
C
      E      = MATERF(1,1)
      FCP    = MATERF(1,2)
      FTP    = MATERF(2,2)
      GC     = MATERF(4,2)
      GT     = MATERF(5,2)
      CELAS  = MATERF(6,2)
      TYPCOM = INT( MATERF(7,2) + 0.5D0 )
      TYPTRA = INT( MATERF(8,2) + 0.5D0 )
C
      KUC = ZERO
      KE  = ZERO
      KUT = ZERO
      FC  = ZERO
      FT  = ZERO
      DFCDLC = ZERO
      DFTDLT = ZERO
C
C --- LONGUEUR CARACTERISTIQUE POUR LOI BETON LC
C
      IF(MATERF(9,2).LT.ZERO) THEN
         LC = ELGEOM(1)
      ELSE
         LC = MATERF(9,2)
      ENDIF
C
C
C --- VALEUR ET DERIVEE DE LA COURBE D'ADOUCISSEMENT EN COMPRESSION
C
      IF(NSEUIL.EQ.1 .OR. NSEUIL.EQ.3 .OR. NSEUIL.EQ.11 .OR.
     &   NSEUIL.EQ.33) THEN
C
C -      COURBE POST PIC EN COMPRESSION LINEAIRE
C
         IF(TYPCOM.EQ.0) THEN
            KE = DEUX * (UN - CELAS) * FCP / E
            KU = DEUX * GC / (LC * FCP)
     &         - (UN + DEUX * CELAS) * KE * D13
            LC0 = (6.D0 * E * GC)
     &          / (FCP * FCP * (11.D0 - 4.D0 * CELAS * (UN + CELAS)))
            IF(LC.GT.LC0) THEN
              IF(MATERF(9,2).LT.ZERO) THEN
                CALL UTMESS('A','BETFPP','INTEGRATION ELASTOPLASTIQUE '
     &          //'DE LOI BETON_DOUBLE_DP : LA CONDITION D APPLICABI'
     &          //'LITE SUR LA TAILLE DES ELEMENTS N EST PAS RESPECTEE '
     &          //'EN COMPRESSION.')
              ELSE
                CALL TECAEL ( IADZI, IAZK24 )
                NOMAIL = ZK24(IAZK24-1+3)(1:8)
                CALL UTMESS('A','BETFPP','INTEGRATION ELASTOPLASTIQUE '
     &          //'DE LOI BETON_DOUBLE_DP : LA CONDITION D APPLICABI'
     &          //'LITE SUR LA TAILLE DES ELEMENTS N EST PAS RESPECTEE '
     &          //'EN COMPRESSION POUR LA MAILLE: '//NOMAIL)
              ENDIF
            ENDIF
            IF(PC.LT.KE) THEN
               FC = FCP * (CELAS + DEUX*(UN - CELAS)*PC/KE
     &                  + (CELAS - UN)*PC*PC/(KE*KE))
               DFCDLC = FCP * DEUX * (UN - CELAS) * (UN - PC/KE) / KE
            ELSE
               IF(PC.LT.KU) THEN
                  FC = FCP
     1               * (PC - KU) / (KE - KU)
                  DFCDLC =  FCP / (KE - KU)
               ELSE
               FC = FCP * EPSI
C               FC = ZERO
               DFCDLC = ZERO
               ENDIF
            ENDIF
C
C -      COURBE POST PIC EN COMPRESSION NON LINEAIRE
C
         ELSE
            KE = DEUX * (UN - CELAS) * FCP / E
            KU = 1.5D0 * GC / (LC * FCP) - 0.5D0 * CELAS * KE
            LC0 = (1.5D0 * E * GC)
     &          / (FCP * FCP * (4.D0 - CELAS * (UN + CELAS)))
            IF(LC.GT.LC0) THEN
              IF(MATERF(9,2).LT.ZERO) THEN
                CALL UTMESS('A','BETFPP','INTEGRATION ELASTOPLASTIQUE '
     &          //'DE LOI BETON_DOUBLE_DP : LA CONDITION D APPLICABI'
     &          //'LITE SUR LA TAILLE DES ELEMENTS N EST PAS RESPECTEE '
     &          //'EN COMPRESSION.')
              ELSE
                CALL TECAEL ( IADZI, IAZK24 )
                NOMAIL = ZK24(IAZK24-1+3)(1:8)
                CALL UTMESS('A','BETFPP','INTEGRATION ELASTOPLASTIQUE '
     &          //'DE LOI BETON_DOUBLE_DP : LA CONDITION D APPLICABI'
     &          //'LITE SUR LA TAILLE DES ELEMENTS N EST PAS RESPECTEE '
     &          //'EN COMPRESSION POUR LA MAILLE: '//NOMAIL)
              ENDIF
            ENDIF
            IF(PC.LT.KE) THEN
C               FC = FCP / TROIS
C     1            * (UN +  4.D0*PC/KE - DEUX*PC*PC/(KE*KE))
C               DFCDLC =  (4.D0* FCP)/(TROIS*KE) * (UN - PC/KE)
               FC = FCP * (CELAS + DEUX*(UN - CELAS)*PC/KE
     &                  + (CELAS - UN)*PC*PC/(KE*KE))
               DFCDLC = FCP * DEUX * (UN - CELAS) * (UN - PC/KE) / KE
            ELSEIF(PC.LT.KU) THEN
               FC = FCP
     1         * (UN - (PC - KU)* (PC - KU) /((KE - KU)*(KE - KU)))
               DFCDLC = - DEUX*FCP*(PC - KU) / ((KE - KU)*(KE - KU))
            ELSE
               FC = FCP * EPSI
C               FC = ZERO
               DFCDLC = ZERO
            ENDIF
         ENDIF
         KUC = KU
      ENDIF
C
C --- VALEUR ET DERIVEE DE LA COURBE D'ADOUCISSEMENT EN TRACTION
C
      IF(NSEUIL.EQ.2 .OR. NSEUIL.EQ.3 .OR. NSEUIL.EQ.22 .OR.
     &   NSEUIL.EQ.33) THEN
C
C -      COURBE POST PIC EN TRACTION LINEAIRE
C
         IF(TYPTRA.EQ.0) THEN
            KU = DEUX * GT / (LC * FTP)
            LC0 = (DEUX * E * GT) / (FTP * FTP)
            IF(LC.GT.LC0) THEN
              IF(MATERF(9,2).LT.ZERO) THEN
                CALL UTMESS('A','BETFPP','INTEGRATION ELASTOPLASTIQUE '
     &          //'DE LOI BETON_DOUBLE_DP : LA CONDITION D APPLICABI'
     &          //'LITE SUR LA TAILLE DES ELEMENTS N EST PAS RESPECTEE '
     &          //'EN TRACTION.')
              ELSE
                CALL TECAEL ( IADZI, IAZK24 )
                NOMAIL = ZK24(IAZK24-1+3)(1:8)
                CALL UTMESS('A','BETFPP','INTEGRATION ELASTOPLASTIQUE '
     &          //'DE LOI BETON_DOUBLE_DP : LA CONDITION D APPLICABI'
     &          //'LITE SUR LA TAILLE DES ELEMENTS N EST PAS RESPECTEE '
     &          //'EN TRACTION POUR LA MAILLE: '//NOMAIL)
              ENDIF
            ENDIF
            IF(PT.LT.KU) THEN
               FT = FTP * (UN - PT / KU)
               DFTDLT = - FTP / KU
            ELSE
               FT = FTP * EPSI
C               FT = ZERO
               DFTDLT = ZERO
            ENDIF
C
C -      COURBE POST PIC EN TRACTION NON LINEAIRE
C
         ELSE
            KU = 1.D06
            LC0 = (E * GT) / (FTP * FTP)
            IF(LC.GT.LC0) THEN
              IF(MATERF(9,2).LT.ZERO) THEN
                CALL UTMESS('A','BETFPP','INTEGRATION ELASTOPLASTIQUE '
     &          //'DE LOI BETON_DOUBLE_DP : LA CONDITION D APPLICABI '
     &          //'LITE SUR LA TAILLE DES ELEMENTS N EST PAS RESPECTEE '
     &          //'EN TRACTION.')
              ELSE
                CALL TECAEL ( IADZI, IAZK24 )
                NOMAIL = ZK24(IAZK24-1+3)(1:8)
                CALL UTMESS('A','BETFPP','INTEGRATION ELASTOPLASTIQUE '
     &          //'DE LOI BETON_DOUBLE_DP : LA CONDITION D APPLICABI'
     &          //'LITE SUR LA TAILLE DES ELEMENTS N EST PAS RESPECTEE '
     &          //'EN TRACTION POUR LA MAILLE: '//NOMAIL)
              ENDIF
            ENDIF
            FT = FTP * EXP( - LC * FTP * PT / GT)
            DFTDLT = - FTP * FTP * LC / GT * EXP( - LC * FTP * PT / GT)
         ENDIF
         KUT = KU
C
      ENDIF
C
      END
