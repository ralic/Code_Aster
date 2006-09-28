        SUBROUTINE CJSMAT ( MOD, IMAT, TEMPF, MATERF,
     &                      NDT, NDI, NVI, NIVCJS )
        IMPLICIT NONE

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
C       CJS        : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                    MATER(*,1) = E , NU , ALPHA
C                    MATER(*,2) = BETA_CJS, RM, N_CJS, KP, RC, A_CJS,
C                                 B_CJS, C_CJS , GAMMA_CJS, MU_CJS,
C                                 PCO, PA
C                    VARIABLES INTERNES : Q, R, X, SIGNE, ETAT
C               ( SIGNE = SIGNE(S:DEPSDP) )
C                (ETAT: ELASTIC = 0, ISOTRO = 1, DEVIAT = 2, ISODEV = 3)
C       ----------------------------------------------------------------
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C           MOD    :  TYPE DE MODELISATION
C           TEMPF  :  TEMPERATURE  A T+DT
C       OUT MATERF :  COEFFICIENTS MATERIAU A T+DT
C                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NVI    :  NB DE VARIABLES INTERNES
C           NIVCJS :  NIVEAU 1, 2 OU 3 DE LA LOI CJS
C       ----------------------------------------------------------------
        INTEGER         NDT, NDI, NVI, IMAT
        REAL*8          MATERF(14,2) , TEMPF
        CHARACTER*8     MOD, NOMC(17)
        CHARACTER*4     NIVCJS
        CHARACTER*2     BL2, FB2, CERR(17)
C       ----------------------------------------------------------------
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
        CALL CJSNVI( MOD, NDT, NDI, NVI )
C
C -     RECUPERATION MATERIAU -----------------------------------------
C
        BL2 = '  '
        FB2 = 'F '
C
        NOMC(1) = 'E        '
        NOMC(2) = 'NU       '
        NOMC(3) = 'ALPHA    '
        NOMC(4) = 'BETA_CJS '
        NOMC(5) = 'RM       '
        NOMC(6) = 'N_CJS    '
        NOMC(7) = 'KP       '
        NOMC(8) = 'RC       '
        NOMC(9) = 'A_CJS    '
        NOMC(10)= 'B_CJS    '
        NOMC(11)= 'C_CJS    '
        NOMC(12)= 'GAMMA_CJS'
        NOMC(13)= 'MU_CJS   '
        NOMC(14)= 'PCO      '
        NOMC(15)= 'PA       '
        NOMC(16)= 'Q_INIT   '
        NOMC(17)= 'R_INIT   '

C
C -     RECUPERATION MATERIAU A TEMPF (T+DT)
C
        CALL RCVALA(IMAT,' ', 'ELAS', 1, 'TEMP', TEMPF, 3,
     &                 NOMC(1),  MATERF(1,1),  CERR(1), BL2 )
        IF ( CERR(3) .NE. 'OK' ) MATERF(3,1) = 0.D0
        CALL RCVALA(IMAT,' ', 'CJS', 1, 'TEMP', TEMPF, 12,
     &                 NOMC(4),  MATERF(1,2),  CERR(4), FB2 )
        CALL RCVALA(IMAT,' ', 'CJS', 1, 'TEMP', TEMPF, 2,
     &                 NOMC(16),  MATERF(13,2),  CERR(16), BL2 )
        IF ( CERR(16).EQ.'NO') THEN
         MATERF(13,2) = 0.D0
        ENDIF
        IF ( CERR(17).EQ.'NO') THEN
         MATERF(14,2) = 0.D0
        ENDIF

        IF( MATERF(3,2) .EQ. 0.D0 ) THEN
            NIVCJS='CJS1'
C - POUR CJS1, PAR DEFAUT RC=RM/2
C   ET POUR EVITER DES NAN DANS LES LOG ON PREND PC0 = 1.

            MATERF(5,2) = MATERF(2,2) / 2.D0
            MATERF(11,2) = 1.D0

        ELSE IF(MATERF(3,2) .NE. 0.D0 .AND. MATERF(6,2) .NE. 0.D0) THEN
            NIVCJS='CJS2'
C - POUR CJS2  POUR EVITER DES NAN DANS LES LOG
C   ON PREND PC0 = 1
            MATERF(11,2) = 1.D0

        ELSE IF(MATERF(3,2) .NE. 0.D0 .AND. MATERF(6,2) .EQ. 0.D0) THEN
            NIVCJS='CJS3'

        ELSE
            CALL U2MESS('F','ALGORITH2_16')
        ENDIF

        END
