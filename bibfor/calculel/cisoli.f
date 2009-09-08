      SUBROUTINE CISOLI (FAMI,NDIM,IMATE,COMPOR,EPSM,
     &                  T,LAMBDA,DEUXMU,GAMMA,SEUIL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF calculel  DATE 08/09/2009   AUTEUR SFAYOLLE S.FAYOLLE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      CHARACTER*16       COMPOR(*)
      CHARACTER*(*)      FAMI
      INTEGER            IMATE,NDIM, T(3,3)
      REAL*8             EPSM(6), LAMBDA, DEUXMU
      REAL*8             GAMMA, SEUIL
      CHARACTER*2         IDRET(3)
C ----------------------------------------------------------------------
C     INITIALISATION D ENDO_ISOT_BETON POUR METHODE IMPLEX
C
C IN  COMPOR     : NOM DE LA LOI DE COMPORTEMENT
C IN  IMATE      : CODE MATERIAU
C IN  EPSM       : DEFORMATION AU TEMPS MOINS
C OUT T          : TENSEUR DE PLACEMENT (PASSAGE VECT -> MATRICE)
C OUT LAMBDA
C OUT DEUXMU
C OUT ALPHA
C OUT GAMMA
C OUT SEUIL
C ----------------------------------------------------------------------
      CHARACTER*2 COD2(3)
      CHARACTER*8 NOMRES(3)
      INTEGER     I
      REAL*8      VALRES(3), E, NU
      REAL*8      K0, K1, SICR, TREPSM

C
C -- INITIALISATION
C

C --  TENSEUR DE PLACEMENT (PASSAGE VECT -> MATRICE)
      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3

C -- LECTURE DES CARACTERISTIQUES ELASTIQUES
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,IDRET, 'FM')

      E     = VALRES(1)
      NU    = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)

C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'D_SIGM_EPSI'
      NOMRES(2) = 'SYT'
      NOMRES(3) = 'SYC'
      CALL RCVALB(FAMI,1,1,'+',IMATE,' ','BETON_ECRO_LINE',
     &           0,' ',0.D0,3,
     &            NOMRES,VALRES,COD2,' ')
      IF ((COD2(1).NE.'OK').OR.(COD2(2).NE.'OK')) THEN
         CALL U2MESS('F','ALGORITH4_51')
      ENDIF
      GAMMA  = - E/VALRES(1)
      K0=VALRES(2)**2 *(1.D0+GAMMA)/(2.D0*E)
     &               *(1.D0+NU-2.D0*NU**2)/(1.D0+NU)
      IF (NU.EQ.0) THEN
        IF (COD2(3).EQ.'OK') THEN
          CALL U2MESS('F','ALGORITH4_52')
        ELSE
          SEUIL=K0
        ENDIF
      ELSE
        SICR=SQRT((1.D0+NU-2.D0*NU**2)/(2.D0*NU**2))*VALRES(2)
        IF (COD2(3).EQ.'NO') THEN
          SEUIL=K0
        ELSE
          IF (VALRES(3).LT.SICR) THEN
            CALL U2MESS('F','ALGORITH4_53')
          ELSE
            K1=VALRES(3)*(1.D0+GAMMA)*NU**2/(1.D0+NU)/(1.D0-2.D0*NU)
     &        -K0*E/(1.D0-2.D0*NU)/VALRES(3)
C      PASSAGE AUX DEFORMATIONS ELASTIQUES
            TREPSM=0.D0
            DO 1 I=1,NDIM
              TREPSM=TREPSM+EPSM(I)
 1          CONTINUE
            IF (TREPSM.GT.0.D0) THEN
              TREPSM=0.D0
            ENDIF
            SEUIL  = K0-K1*TREPSM
          ENDIF
        ENDIF
      ENDIF

      END
