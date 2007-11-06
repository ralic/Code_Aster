        SUBROUTINE HUJMAT (MOD, IMAT, TEMPF, MATERF,
     &                     NDT, NDI, NVI)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    ------------------------------------------------------------
C HUJEUX  : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C           NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C           MATER(*,1) = E , NU , ALPHA
C           MATER(*,2) = N, BETA, D, M, PCO , PREF,
C                        ACYC, AMON, CCYC, CMON,
C                        RD_ELA, RI_ELA, RHYS, RMOB, XM
C           VARIABLES INTERNES : R1, R2, R3, R4, EPSI_VOLU_P,
C                                IND1, IND2, IND3, IND4
C           1, 2, 3 = DEVIATOIRE ; 4 = ISOTROPE ; 
C           ( SIGNE = SIGNE(S:DEPSDP) )
C           ( IND = 0: MECANISME INACTIF, = 1: MECANISME ACTIF )
C    ------------------------------------------------------------
C    IN  IMAT   :  ADRESSE DU MATERIAU CODE
C        MOD    :  TYPE DE MODELISATION
C        TEMPF  :  TEMPERATURE  A T+DT
C    OUT MATERF :  COEFFICIENTS MATERIAU A T+DT
C                  MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                  MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C        NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C        NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C        NVI    :  NB DE VARIABLES INTERNES
C    ------------------------------------------------------------
        INTEGER      NDT, NDI, NVI, IMAT, I
        REAL*8       MATERF(22,2) , TEMPF
        CHARACTER*8  MOD, NOMC(23)
        CHARACTER*2  BL2, FB2, CERR(23)


C -  NB DE COMPOSANTES / VARIABLES INTERNES ---------------------
        CALL HUJNVI (MOD, NDT, NDI, NVI)   


C -  RECUPERATION MATERIAU --------------------------------------
        BL2 = '  '
        FB2 = 'F '

        NOMC(1) = 'E       '
        NOMC(2) = 'NU      '
        NOMC(3) = 'ALPHA   '
        NOMC(4) = 'N       '
        NOMC(5) = 'BETA    '
        NOMC(6) = 'D       '
        NOMC(7) = 'B       '
        NOMC(8) = 'PHI     '
        NOMC(9) = 'ANGDIL  '
        NOMC(10)= 'PCO     '
        NOMC(11)= 'PREF    '
        NOMC(12)= 'ACYC    '
        NOMC(13)= 'AMON    '
        NOMC(14)= 'CCYC    '
        NOMC(15)= 'CMON    '
        NOMC(16)= 'RD_ELA  '
        NOMC(17)= 'RI_ELA  '
        NOMC(18)= 'RHYS    '
        NOMC(19)= 'RMOB    '
        NOMC(20)= 'XM      '
Caf 30/04/07 debut
        NOMC(21)= 'RD_CYC  '
        NOMC(22)= 'RI_CYC  '
        NOMC(23)= 'DILA    '
Caf fin
        
        DO 10 I = 1, 22
          MATERF(I,1)=0.D0
          MATERF(I,2)=0.D0
 10       CONTINUE


C --- RECUPERATION MATERIAU A TEMPF (T+DT)
        CALL RCVALA(IMAT,' ','ELAS',0,'   ',TEMPF,3,
     &              NOMC(1), MATERF(1,1), CERR(1),BL2)
             
        CALL RCVALA(IMAT,' ','HUJEUX',0,'   ',TEMPF,20,
     &              NOMC(4), MATERF(1,2),CERR(4),FB2)

        END
