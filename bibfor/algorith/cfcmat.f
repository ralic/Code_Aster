        SUBROUTINE CFCMAT ( IMAT,   NMAT,   TEMPD, TEMPF,
     &                      MATERD, MATERF, MATCST, NDT,   NDI,
     &                      NR , NVI , BZ )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C       POLY_CFC  : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                    NB DE CMP DIRECTES/CISAILLEMENT, NB VAR. INTERNES
C                    MATER(*,1) = E, NU , ALPHA
C                    MATER(*,2) = BZ, G, DL, DA, N, K, TAU_0, Q1,
C                                 B1, HL, Q2, B2, C1, D1, C2
C                    VARIABLES INTERNES : EVI, EVCUM,
C                                         BETGR, ALMIC, GVCUM
C       ----------------------------------------------------------------
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION  DE MATER
C           TEMPD  :  TEMPERATURE  A T
C           TEMPF  :  TEMPERATURE  A T+DT
C       OUT MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                     'NON' SINON
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES TENSEURS
C           NR     :  NB DE COMPOSANTES SYSTEME NL
C           NVI    :  NB DE VARIABLES INTERNES
C           BZ     :  VARIABLE LOGIQUE :
C                    'VRAI' POUR CALCULER AVEC LE MODELE POLY PILVIN
C                    'FAUX' POUR CALCULER AVEC LE MODELE POLY B.Z.
C       ----------------------------------------------------------------
        INTEGER         NMAT, NDT, NDI, NR, NVI
        REAL*8          MATERD(NMAT,2), MATERF(NMAT,2), TEMPD, TEMPF
        REAL*8          EPSI
        CHARACTER*8     NOM, NOMC(18)
        CHARACTER*2     BL2, FB2, CERR(18)
        CHARACTER*3     MATCST
        LOGICAL         BZ
        DATA EPSI       /1.D-15/
C       ----------------------------------------------------------------
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C -     POUR INTEGRATION PAR METHODE EXPLICITE
C -     ON RESTE DIMENSIONNE EN 3D
C
C - 3D
C
         NDT = 6
         NDI = 3
         NR  = 1
         NVI = 1688
C
         BL2 = '  '
C
C -     RECUPERATION MATERIAU -----------------------------------------
C
          NOMC(1) = 'E       '
          NOMC(2) = 'NU      '
          NOMC(3) = 'ALPHA   '
          NOMC(4) = 'DL      '
          NOMC(5) = 'DA      '
          NOMC(6) = 'N       '
          NOMC(7) = 'K       '
          NOMC(8) = 'TAU_0   '
          NOMC(9) = 'Q1     '
          NOMC(10) = 'B1     '
          NOMC(11) = 'HL     '
          NOMC(12) = 'Q2     '
          NOMC(13) = 'B2     '
          NOMC(14) = 'C1     '
          NOMC(15) = 'D1     '
          NOMC(16) = 'C2     '
C
C -     RECUPERATION MATERIAU A TEMPD (T)
C
          CALL RCVALA(IMAT,' ',    'ELAS',  1,  'TEMP', TEMPD, 3,
     &                   NOMC(1),  MATERD(1,1),  CERR(1), BL2 )
          CALL RCVALA(IMAT,' ',    'POLY_CFC', 1, 'TEMP', TEMPD, 13,
     &                   NOMC(4),  MATERD(1,2),  CERR(4), BL2 )
          IF (CERR(4).EQ.'OK'.AND.CERR(5).EQ.'OK') THEN
          BZ=.TRUE.
          DO 10 I = 1 , 16
            IF ( CERR(I) . NE. 'OK' .AND. I .NE. 2 .AND. I .NE. 3 ) THEN
            CALL CODREE ( TEMPD , 'E' , NOM )
            CALL UTMESS ('F','POLY_CFC','VALEUR DE '//NOMC(I)//
     &      ' A LA TEMPERATURE '//NOM//' NON TROUVEE')
C        CALL U2MESK('F','ALGORITH_80', 2 ,VALK)
            ENDIF
            IF ( CERR(3) .NE. 'OK' ) MATERD(3,1) = 0.D0
 10       CONTINUE
          ELSE
          BZ=.FALSE.
          ENDIF
C
C -     RECUPERATION MATERIAU A TEMPF (T+DT)
C
          CALL RCVALA(IMAT,' ', 'ELAS',  1,  'TEMP', TEMPF, 3,
     &                   NOMC(1),  MATERF(1,1),  CERR(1), BL2 )
          CALL RCVALA(IMAT,' ', 'POLY_CFC',1,  'TEMP', TEMPF, 13,
     &                   NOMC(4),  MATERF(1,2),  CERR(4), BL2 )
          IF (CERR(4).EQ.'OK'.AND.CERR(5).EQ.'OK') THEN
          BZ=.TRUE.
          DO 20 I = 1 , 16
            IF ( CERR(I) . NE. 'OK' .AND. I .NE. 2 .AND. I .NE. 3 ) THEN
            CALL CODREE ( TEMPF , 'E' , NOM )
            CALL UTMESS ('F','POLY_CFC','VALEUR DE '//NOMC(I)//
     &      ' A LA TEMPERATURE '//NOM//' NON TROUVEE')
C        CALL U2MESK('F','ALGORITH_80', 2 ,VALK)
            ENDIF
            IF ( CERR(3) .NE. 'OK' ) MATERF(3,1) = 0.D0
 20       CONTINUE
          ELSE
          BZ=.FALSE.
          ENDIF
C
C -     MATERIAU CONSTANT ?
C
        MATCST = 'OUI'
        DO 30 I = 1,2
          IF ( ABS ( MATERD(I,1) - MATERF(I,1) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 30     CONTINUE
        DO 40 I = 1,13
          IF ( ABS ( MATERD(I,2) - MATERF(I,2) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 40     CONTINUE
C
 9999   CONTINUE
        END
