        SUBROUTINE DPMATE ( MOD, IMAT, MATERF, NDT, NDI, NVI, TYPEDP)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/09/2004   AUTEUR F6BHHBO P.DEBONNIERES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
        IMPLICIT     NONE
        INTEGER      NDT, NDI, NVI, IMAT, TYPEDP
        REAL*8       MATERF(4,2)
        CHARACTER*8  MOD
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU POUR LA LOI DE DRUCKER PRAGER --
C ======================================================================
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C       OUT MATERF :  COEFFICIENTS MATERIAU A T+DT
C                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C ======================================================================
        REAL*8       TROIS, DEUX, UN, SIX, ALPHA, SY, SYULT, C , A, PHI
        REAL*8       TYPED,R8VIDE,TABTMP(4),COE
        CHARACTER*2  CODRET(7)
        CHARACTER*8  NOMC(7)
C ======================================================================
        PARAMETER ( SIX    =  6.0D0 )
        PARAMETER ( TROIS  =  3.0D0 )
        PARAMETER ( DEUX   =  2.0D0 )
        PARAMETER ( UN     =  1.0D0 )
C ======================================================================
C --- DEFINITION PARAMETRES MATERIAU ELAS ------------------------------
C ======================================================================
        NOMC(1) = 'E'
        NOMC(2) = 'NU'
        NOMC(3) = 'ALPHA'
C ======================================================================
C --- RECUPERATION DONNEES MATERIAU ELAS -------------------------------
C ======================================================================
        MATERF(3,1) = 0.0D0
        CALL RCVALA(IMAT,' ', 'ELAS', 0, ' ', 0.D0,
     +                            2, NOMC(1), MATERF(1,1), CODRET, 'FM')
        CALL RCVALA(IMAT,' ', 'ELAS', 0, ' ', 0.D0,
     +                            1, NOMC(3), MATERF(3,1), CODRET, '  ')
C ======================================================================
C --- DEFINITION PARAMETRES MATERIAU DRUCKER ---------------------------
C ======================================================================
        NOMC(4) = 'ALPHA'
        NOMC(5) = 'SY'
        NOMC(6) = 'P_ULTM'
C ======================================================================
C --- RECUPERATION MATERIAU SUIVANT LE TYPE D ECROUISSAGE --------------
C ======================================================================
        TYPED = R8VIDE()
        CALL RCVALA(IMAT,' ', 'DRUCK_PRAGER', 0, ' ', 0.D0,
     +                               1, 'TYPE_DP',TYPED, CODRET, ' ')
        IF (TYPED.EQ.1.0D0) THEN
C ======================================================================
C --- CAS LINEAIRE -----------------------------------------------------
C ======================================================================
           TYPEDP = 1
           NOMC(7) = 'H'
           CALL RCVALA(IMAT,' ', 'DRUCK_PRAGER', 0, ' ', 0.D0,
     +                            4, NOMC(4), TABTMP(1), CODRET, 'FM')
C ======================================================================
C --- POUR DES COMMODITES DE PROGRAMMATION ON DEFINIT LES PARAMETRES ---
C --- MATERIAU DE LA FACON SUIVANTE ------------------------------------
C ======================================================================
           MATERF(1,2) = TABTMP(2)
           MATERF(2,2) = TABTMP(4)
           MATERF(3,2) = TABTMP(1)
           MATERF(4,2) = TABTMP(3)
           COE         = MATERF(2,2) + TROIS * MATERF(1,1) *
     +                  ( UN/DEUX/(UN+MATERF(2,1)) +
     +                    MATERF(3,2)*MATERF(3,2)/(UN-DEUX*MATERF(2,1)))
           IF (COE.LE.0.0D0) THEN
              CALL UTMESS('F','DPMATE','INCOHERENCE SUR H, ALPHA, ELAS')
           ENDIF
        ELSE IF (TYPED.EQ.2.0D0) THEN
C ======================================================================
C --- CAS PARABOLIQUE --------------------------------------------------
C ======================================================================
           TYPEDP = 2
           NOMC(7) = 'SY_ULTM'
           CALL RCVALA(IMAT,' ', 'DRUCK_PRAGER', 0, ' ', 0.D0,
     +                            4, NOMC(4), TABTMP(1), CODRET, 'FM')
C ======================================================================
C --- POUR DES COMMODITES DE PROGRAMMATION ON DEFINIT LES PARAMETRES ---
C --- MATERIAU DE LA FACON SUIVANTE ------------------------------------
C ======================================================================
           ALPHA = TABTMP(1)
           SY    = TABTMP(2)
           SYULT = TABTMP(4)
           PHI   = ATAN2 (( TROIS*ALPHA / DEUX /
     +                SQRT(( DEUX*ALPHA + 1.0D0 )*(1.0D0-ALPHA))),1.0D0)
C           PHI   = ASIN ( TROIS * ALPHA / ( DEUX + ALPHA ) )
           C     = ( TROIS - SIN(PHI) ) * SY / SIX / COS(PHI)
           A     = SQRT ( SYULT / SY )
           MATERF(1,2) = A
           MATERF(2,2) = PHI
           MATERF(3,2) = C
           MATERF(4,2) = TABTMP(3)
        ENDIF
C ======================================================================
C --- NOMBRE DE COMPOSANTES --------------------------------------------
C ======================================================================
        IF (MOD(1:2).EQ.'3D') THEN
           NDT = 6
           NDI = 3
        ELSE IF ((MOD(1:6).EQ.'D_PLAN') .OR.
     +           (MOD(1:4).EQ.'AXIS')        ) THEN
           NDT = 4
           NDI = 3
        ENDIF
C ======================================================================
C --- NOMBRE DE VARIABLES INTERNES -------------------------------------
C ======================================================================
        NVI = 3
C ======================================================================
        END
