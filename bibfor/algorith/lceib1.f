      SUBROUTINE LCEIB1 (IMATE, COMPOR, NDIM, EPSM, T, LAMBDA, DEUXMU,
     &                   ALPHA, GAMMA, SEUIL, COUP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
      IMPLICIT NONE
      CHARACTER*16       COMPOR(*)
      LOGICAL            COUP
      INTEGER            IMATE,NDIM, T(3,3)
      REAL*8             EPSM(6), LAMBDA, DEUXMU, ALPHA, GAMMA
      REAL*8             SEUIL
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDO_ISOT_BETON - INITIALISATION
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
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      INTEGER     I
      REAL*8      VALRES(3), E, NU
      REAL*8      K0, K1, SICR, TREPSM

      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3
      
      IF ((.NOT.( COMPOR(1)(1:15) .EQ. 'ENDO_ISOT_BETON')).AND.
     &   (.NOT.( COMPOR(1)(1:6) .EQ. 'KIT_HM')).AND.
     &   (.NOT.( COMPOR(1)(1:7) .EQ. 'KIT_HHM')).AND.
     &   (.NOT.( COMPOR(1)(1:7) .EQ. 'KIT_THM')).AND.
     &   (.NOT.( COMPOR(1)(1:8) .EQ. 'KIT_THHM')).AND.
     &   (.NOT.( COMPOR(1)(1:7) .EQ. 'KIT_DDI'))) THEN
            CALL UTMESS('F','ENDO_ISOT_BETON_01',
     &           ' COMPORTEMENT INATTENDU : '//COMPOR(1))
      ENDIF
C    LECTURE DES CARACTERISTIQUES DU MATERIAU
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
      IF ((((COMPOR(1)(1:6) .EQ. 'KIT_HM') .OR. 
     &     (COMPOR(1)(1:7) .EQ. 'KIT_HHM') .OR.
     &     (COMPOR(1)(1:7) .EQ. 'KIT_THM') .OR.
     &     (COMPOR(1)(1:7) .EQ. 'KIT_DDI') .OR.
     &     (COMPOR(1)(1:8) .EQ. 'KIT_THHM')).AND.
     &     (COMPOR(11)(1:15) .EQ. 'ENDO_ISOT_BETON')).OR.
     &     (COMPOR(1)(1:15) .EQ. 'ENDO_ISOT_BETON')) THEN
      IF (COUP) THEN
      CALL RCVALA(IMATE,' ','ELAS',1,'TEMP',0.D0,2,
     &              NOMRES,VALRES,CODRET, 'FM')
      CALL RCVALA(IMATE,' ','ELAS',1,'TEMP',0.D0,1,
     &              NOMRES(3),VALRES(3),CODRET(3), ' ')            
      ELSE
      CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET, 'FM')
      CALL RCVALA(IMATE,' ','ELAS',3,' ',0.D0,1,
     &              NOMRES(3),VALRES(3),CODRET(3), ' ')      
      ENDIF
      IF ( CODRET(3) .NE. 'OK' ) VALRES(3) = 0.D0
      E     = VALRES(1)
      NU    = VALRES(2)
      ALPHA = VALRES(3)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)
C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'D_SIGM_EPSI'
      NOMRES(2) = 'SYT'
      NOMRES(3) = 'SYC'
      CALL RCVALA(IMATE,' ','BETON_ECRO_LINE',0,' ',0.D0,3,
     &            NOMRES,VALRES,CODRET,' ')
      GAMMA  = - E/VALRES(1)
      K0=VALRES(2)**2 *(1.D0+GAMMA)/(2.D0*E)
     &               *(1.D0+NU-2.D0*NU**2)/(1.D0+NU)
      IF (NU.EQ.0) THEN
        IF (CODRET(3).EQ.'OK') THEN
          CALL UTMESS('F','LCLDSB',' SYC NE DOIT PAS ETRE
     & VALORISE POUR NU NUL DANS DEFI_MATERIAU')
        ELSE
          SEUIL=K0
        ENDIF
      ELSE
        SICR=SQRT((1.D0+NU-2.D0*NU**2)/(2.D0*NU**2))*VALRES(2)
        IF (CODRET(3).EQ.'NO') THEN
          SEUIL=K0
        ELSE
          IF (VALRES(3).LT.SICR) THEN        
            CALL UTMESS('F','LCLDSB',' SYC DOIT ETRE
     &  SUPERIEUR A SQRT((1+NU-2*NU*NU)/(2.D0*NU*NU))*SYT
     &  DANS DEFI_MATERIAU POUR PRENDRE EN COMPTE LE 
     &  CONFINEMENT')
          ELSE
            K1=VALRES(3)*(1.D0+GAMMA)*NU**2/(1.D0+NU)/(1.D0-2.D0*NU)
     &        -K0*E/(1.D0-2.D0*NU)/VALRES(3)
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
      ENDIF
      
      END
