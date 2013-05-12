      SUBROUTINE HUJMA2(MOD,IMAT,NMAT,TEMPF,ANGMAS,SIGD,VIND,
     &                  MATERD,MATERF,NDT,NDI,NVI,NR,MATCST)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/04/2013   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
      IMPLICIT   NONE
C       ----------------------------------------------------------------
C       RECUPERATION DU MATERIAU A TEMPF ET AJUSTEMENT SEUILS
C       IN  MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION 1 DE MATER
C           TEMPF  :  TEMPERATURE A T + DT
C          ANGMAS  :  ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM - 3CMP)
C           SIGD   :  ETAT CONTRAINTES A T
C           VIND   :  VARIABLE INTERNE A T
C       OUT MATERF :  COEFFICIENTS MATERIAU A T+DT (TEMPF )
C                     MATER(*,I) = CARACTERISTIQUES MATERIAU
C                                    I = 1  CARACTERISTIQUES ELASTIQUES
C                                    I = 2  CARACTERISTIQUES PLASTIQUES
C           MATERD : PARAMETRES MATERIAU A T
C           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                     'NON' SINON OU 'NAP' SI NAPPE DANS 'VECMAT.F'
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NR     :  NB DE COMPOSANTES SYSTEME NL
C           NVI    :  NB DE VARIABLES INTERNES
C       ----------------------------------------------------------------
      CHARACTER*8   MOD
      CHARACTER*3   MATCST
      INTEGER       IMAT,NMAT,NDT,NDI,NVI,NR       
      REAL*8        TEMPF,MATERD(NMAT,2),MATERF(NMAT,2),VIND(50)
      REAL*8        SIGD(6),ANGMAS(3)
C
      LOGICAL       REORIE
      REAL*8        ZERO,R8VIDE,BID66(6,6),SEUIL,TIN(3),PISO,Q
      REAL*8        PTRAC,B,PHI,M,PC0,DEGR,D,R8PREM,UN,TROIS
      REAL*8        MATERT(22,2)
      INTEGER       I,J
      PARAMETER   ( ZERO  = 0.D0 )
      PARAMETER   ( UN    = 1.D0 )
      PARAMETER   ( TROIS = 3.D0 )
      PARAMETER   ( DEGR  = 0.0174532925199D0 )
C     ------------------------------------------------------------------
C ----------------------------------------------------------------------
C ---  NR     :  NB DE COMPOSANTES MAXIMUM DU SYSTEME NL  
C ----------------------------------------------------------------------
      NR = 18

C ----------------------------------------------------------------------
C ---  RECUPERATION DE MATERF, NDT, NDI, NVI ET MATERD
C ----------------------------------------------------------------------
      MATCST = 'OUI'
      CALL HUJMAT(MOD,IMAT,TEMPF,MATERT,NDT,NDI,NVI)

      DO 10 I = 1, 22
        DO 20 J = 1, 2
          MATERD(I,J) = MATERT(I,J)
          MATERF(I,J) = MATERT(I,J)
  20    CONTINUE
  10  CONTINUE    

C ----------------------------------------------------------------------
C --- CONTROLE DE LA DIMENSION DE LA MODELISATION
C --- AJUSTEMENT NECESSAIRE SI MODELISATION TYPE D_PLAN
C ----------------------------------------------------------------------
      IF(MOD(1:6).EQ.'D_PLAN')THEN
        SIGD(5) = ZERO
        SIGD(6) = ZERO
        NDT = 6
      ENDIF      
C ----------------------------------------------------------------------
C --- CONTROLE DES EQUILIBRES DE SEUILS PLASTIQUES
C ----------------------------------------------------------------------
C --- 1 ORIENTATION DES CONTRAINTES SELON ANGMAS VERS REPERE LOCAL
      IF (ANGMAS(1).EQ.R8VIDE()) CALL U2MESS('F','ALGORITH8_20')
      REORIE =(ANGMAS(1).NE.ZERO) .OR. (ANGMAS(2).NE.ZERO)
     &         .OR. (ANGMAS(3).NE.ZERO)
      CALL HUJORI ('LOCAL', 1, REORIE, ANGMAS, SIGD, BID66)

C --- 2 INITIALISATION SEUIL DEVIATOIRE SI NUL
      PTRAC = MATERF(21,2)
      DO 30 I = 1, NDI
        IF (VIND(I) .EQ. ZERO) THEN
           IF (MATERF(13, 2) .EQ. ZERO) THEN
             VIND(I) = 1.D-3
           ELSE
             VIND(I) = MATERF(13,2)
           ENDIF

           CALL HUJCRD(I, MATERT, SIGD, VIND, SEUIL)

C --- SI LE SEUIL EST DESEQUILIBRE A L'ETAT INITIAL
C     ON EQUILIBRE LE SEUIL EN CALCULANT LA VALEUR DE R
C     APPROPRIEE
           IF (SEUIL.GT.ZERO) THEN
             CALL HUJPRJ(I,SIGD,TIN,PISO,Q)
             PISO       = PISO - PTRAC
             B          = MATERF(4,2)
             PHI        = MATERF(5,2)
             M          = SIN(DEGR*PHI)
             PC0        = MATERF(7,2)
             VIND(I)    = -Q/(M*PISO*(UN-B*LOG(PISO/PC0)))
             VIND(23+I) = UN
           ENDIF
        ENDIF
  30    CONTINUE

C ---> 3 INITIALISATION SEUIL ISOTROPE SI NUL
      IF (VIND(4) .EQ. ZERO) THEN
         IF (MATERF(14, 2) .EQ. ZERO) THEN
           VIND(4) = 1.D-3
         ELSE
           VIND(4) = MATERF(14,2)
         ENDIF

         CALL HUJCRI(MATERT, SIGD, VIND, SEUIL)

C --- SI LE SEUIL EST DESEQUILIBRE A L'ETAT INITIAL
C     ON EQUILIBRE LE SEUIL EN CALCULANT LA VALEUR DE R
C     APPROPRIEE

         IF (SEUIL .GT. ZERO) THEN
           PISO    = (SIGD(1)+SIGD(2)+SIGD(3))/TROIS
           D       = MATERF(3,2)
           PC0     = MATERF(7,2)
           VIND(4) = PISO/(D*PC0)
           VIND(27)= UN
         ENDIF
      ENDIF

C ---> 4 INITIALISATION SEUIL CYCLIQUE SI NUL
      DO 40 I = 1, NDI
        IF (VIND(4+I) .EQ. ZERO) THEN
          IF (MATERF(18, 2) .EQ. ZERO) THEN
            VIND(4+I) = 1.D-3
          ELSE
            VIND(4+I) = MATERF(18,2)
          ENDIF
        ENDIF
 40     CONTINUE

      IF (VIND(8) .EQ. ZERO) THEN
         IF (MATERF(19, 2) .EQ. ZERO) THEN
           VIND(8) = 1.D-3
         ELSE
           VIND(8) = MATERF(19,2)
         ENDIF
      ENDIF

C --- 5 CONTROLE DES INDICATEURS DE PLASTICITE
      DO 50 I = 1, 4
        IF (ABS(VIND(27+I)-UN).LT.R8PREM()) VIND(23+I)=-UN
  50    CONTINUE

C --- 7 ORIENTATION DES CONTRAINTES SELON ANGMAS VERS REPERE GLOBAL
      CALL HUJORI ('GLOBA', 1, REORIE, ANGMAS, SIGD, BID66)
      
C ----------------------------------------------------------------------
      END
