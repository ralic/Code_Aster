        SUBROUTINE LMAMAT ( FAMI,KPG,KSP,MOD,IMAT,NMAT,TEMPD,TEMPF,
     1                      MATERD,MATERF, MATCST, TYPMA,NDT,NDI,
     2                      NR,NVI )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/08/2006   AUTEUR CIBHHPD L.SALMONA 
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
C       MODELE VISCOPLASTIQUE DE BESANCON EN VITESSE
C                  RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                  NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                  MATER(16 A 51,1) PREMIERE MATRICE D'ANISOTROPIE 'M'
C                       CONSTRUITE A PARTIR DE M11 , M22 , M33 , M66
C                  MATER(52 A 87,1) DEUXIEME MATRICE D'ANISOTROPIE 'N'
C                       CONSTRUITE A PARTIR DE N11 , N22 , N33 , N66
C                  MATER(16 A 51,2) TROISIEME MATRICE D'ANISOTROPIE 'Q'
C                       CONSTRUITE A PARTIR DE Q11 , Q22 , Q33 , Q66
C                  MATER(52 A 87,2) QUATRIEME MATRICE D'ANISOTROPIE 'R'
C                       CONSTRUITE A PARTIR DE R11 , R22 , R33 , R66
C                  MATER(*,1) = E , NU , ALPHA
C                  MATER(*,2) = DE_0, R_0 , N   , K   , Y_I , Y_0 , B
C                               A_0  ,RM  , M
C                               P   , P1 , P2
C                  VARIABLES INTERNES : X , X1 , X2 , V , E
C       ----------------------------------------------------------------
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION  DE MATER
C           TEMPD  :  TEMPERATURE  A T
C           TEMPF  :  TEMPERATURE  A T+DT
C       OUT MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C                     MATER(16 A 87,*) = MATRICES D'ANISOTROPIE
C           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                     'NON' SINON
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NR     :  NB DE COMPOSANTES SYSTEME NL
C           NVI    :  NB DE VARIABLES INTERNES
C       ----------------------------------------------------------------
        INTEGER         NMAT, IMAT, NDT , NDI  , NR , NVI,KPG,KSP
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          TEMPD , TEMPF , VMTP1(29) , VMTP2(29)
        REAL*8          VALPAD, VALPAF
        REAL*8          EPSI
        CHARACTER*8     MOD,  NOM , NOMC(34) , TYPMA , NOMPAR
        CHARACTER*2     BL2, FB2, CERR(34)
        CHARACTER*3     MATCST
        CHARACTER*(*)   FAMI
        DATA EPSI       /1.D-15/
C       ----------------------------------------------------------------
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
        CALL LMANVI ( MOD , NDT , NDI , NR , NVI )
C
      BL2 = '  '
      FB2 = 'F '
C
C -     VISCO-PLASTICITE --->  CALCUL DE LA MATRICE DE COMPORTEMENT
C -     TANGENT  'COHERENT'
C
          TYPMA = 'COHERENT'
C
C -     RECUPERATION MATERIAU -----------------------------------------
C
          NOMC(1) = 'E       '
          NOMC(2) = 'NU      '
          NOMC(3) = 'ALPHA   '
          NOMC(4) = 'B_ENDOGE'
          NOMC(5) = 'K_DESSIC'
          NOMC(6) = 'DE_0    '
          NOMC(7) = 'R_0     '
          NOMC(8) = 'N       '
          NOMC(9) = 'K       '
          NOMC(10)= 'Y_I     '
          NOMC(11)= 'Y_0     '
          NOMC(12)= 'B       '
          NOMC(13)= 'A_0     '
          NOMC(14)= 'RM      '
          NOMC(15)= 'M       '
          NOMC(16)= 'P       '
          NOMC(17)= 'P1      '
          NOMC(18)= 'P2      '
          NOMC(19)= 'M11     '
          NOMC(20)= 'M22     '
          NOMC(21)= 'M33     '
          NOMC(22)= 'M66     '
          NOMC(23)= 'N11     '
          NOMC(24)= 'N22     '
          NOMC(25)= 'N33     '
          NOMC(26)= 'N66     '
          NOMC(27)= 'Q11     '
          NOMC(28)= 'Q22     '
          NOMC(29)= 'Q33     '
          NOMC(30)= 'Q66     '
          NOMC(31)= 'R11     '
          NOMC(32)= 'R22     '
          NOMC(33)= 'R33     '
          NOMC(34)= 'R66     '
C
         DO  9 J = 1 , 2
         DO  9 I = 1 , NMAT
           MATERD(I,J) = 0.D0
           MATERF(I,J) = 0.D0
 9       CONTINUE
C
          NOMPAR = 'TEMP'
          VALPAD = TEMPD
          VALPAF = TEMPF
C
C -     RECUPERATION MATERIAU A TEMPD (T)
C
          CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','ELAS',1,NOMPAR,VALPAD,
     1                 5,NOMC(1),  MATERD(1,1),  CERR(1), BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERD(3,1) = 0.D0
          IF ( CERR(4) .NE. 'OK' ) MATERD(4,1) = 0.D0
          IF ( CERR(5) .NE. 'OK' ) MATERD(5,1) = 0.D0
          CALL RCVALA(IMAT,' ',    'LMARC',   1,  'TEMP', TEMPD, 29,
     1                   NOMC(6),  VMTP1,        CERR(6) , FB2 )
C
          DO 11 I = 1 , 13
          MATERD(I,2) = VMTP1(I)
 11       CONTINUE
C
C --    CONSTRUCTION DES MATRICES D ANISOTROPIE A TEMPF (T)
          CALL LMAANI( VMTP1(14) , MATERD(16,1) )
          CALL LMAANI( VMTP1(18) , MATERD(52,1) )
          CALL LMAANI( VMTP1(22) , MATERD(16,2) )
          CALL LMAANI( VMTP1(26) , MATERD(52,2) )
C
C -     RECUPERATION MATERIAU A TEMPF (T+DT)
C
          CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','ELAS',1,NOMPAR,
     1                VALPAF,5,NOMC(1),MATERF(1,1),CERR(1),BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERF(3,1) = 0.D0
          IF ( CERR(4) .NE. 'OK' ) MATERF(4,1) = 0.D0
          IF ( CERR(5) .NE. 'OK' ) MATERF(5,1) = 0.D0
          CALL RCVALA(IMAT,' ',    'LMARC',   1,  'TEMP', TEMPF, 29,
     1                   NOMC(6),  VMTP2,         CERR(6) , FB2 )
C
          DO 21 I = 1 , 13
          MATERF(I,2) = VMTP2(I)
 21       CONTINUE
C
C --    CONSTRUCTION DES MATRICES D ANISOTROPIE A TEMPF (T+DT)
          CALL LMAANI( VMTP2(14) , MATERF(16,1) )
          CALL LMAANI( VMTP2(18) , MATERF(52,1) )
          CALL LMAANI( VMTP2(22) , MATERF(16,2) )
          CALL LMAANI( VMTP2(26) , MATERF(52,2) )
C
C -     MATERIAU CONSTANT ?
C
        MATCST = 'OUI'
        DO 30 I = 1,5
          IF ( ABS ( MATERD(I,1) - MATERF(I,1) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 30     CONTINUE
        DO 40 I = 1,29
          IF ( ABS ( VMTP1(I) - VMTP2(I) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 40     CONTINUE
C
 9999   CONTINUE
        END
