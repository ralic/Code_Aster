      SUBROUTINE MPPSTA (H,LDH,V,LDV,DDLSTA,N,VECTT,
     &                   DDLEXC,INDICO,PROJ)
C-----------------------------------------------------------------------
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C-----------------------------------------------------------------------
C
C METHODE DES PUISSANCES AVEC PROJECTION. OPTIMISATION 
C SOUS CONTRAINTES INEGALITES
C
C ----------------------------------------------------------------------
C
C IN  H        : MATRICE REDUITE
C IN  LDH      : NOMBRE DE COEFFICIENTS DE H
C IN  V        : MATRICE DE CHANGEMENT DE BASE
C IN  LDV      : NOMBRE DE COEFFICIENTS DE V
C IN  DDLSTA   : POSITION DES DDL_STAB
C IN  N        : DIMENSION ESPACE GLOBAL
C IN/OUT VECTT : MODE CONVERGE
C IN  DDLEXC   : POSITION DDL IMPOSES
C IN  INDICO   : SI =1 ALORS VECTEUR INITIAL =VECTT 
C                SINON RANDOM  
C IN  PROJ     : =1 PROJECTION
C                =0 PAS DE PROJECTION DANS LA METHODE
C
C ----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      INCLUDE 'jeveux.h'
      INTEGER DDLSTA(N),DDLEXC(N)
      REAL*8  H(LDH,LDH), V(LDV,LDH)
      REAL*8  VECTT(LDV)

C     %-----------------%
C     | SCALAR ARGUMENTS|
C     %-----------------%

      INTEGER N,LDH,LDV,INDICO,PROJ 
C
C
C
C
C     %------------------------%
C     | LOCAL SCALARS & ARRAYS |
C     %------------------------%

      INTEGER ISEED(4),I,J,NITMAX,NUM,NPRO
      INTEGER X0,BOUNDS,X
      REAL*8  NORM
      REAL*8  TEMP,EPSIL,ONE,ZERO
      REAL*8  CRIT2,EPSF

C     %-----------%
C     | FUNCTIONS |
C     %-----------%

      REAL*8 DLAPY2, DNRM2

C     %-----------%
C     | PARAMETER |
C     %-----------%

      PARAMETER (EPSIL=1.D-15)
      PARAMETER (CRIT2=1.D-12)
      PARAMETER (NITMAX = 40000)
      PARAMETER (ONE = 1.0D+0, ZERO = 0.0D+0)
C
      CALL WKVECT('&&MPPSTA.VECT.TEM1','V V R',
     &             LDH,X0)
      CALL WKVECT('&&MPPSTA.VECT.TEM2','V V R',
     &             LDH,BOUNDS)
      CALL WKVECT('&&MPPSTA.VECT.TEM3','V V R',
     &             LDH,X)
C
      CALL R8INIR(LDH,0.D0,ZR(X0),1)
      CALL R8INIR(LDH,0.D0,ZR(BOUNDS),1)
      CALL R8INIR(LDH,0.D0,ZR(X),1)
C
C     INITIALISATION PAR UN VECT RANDOMDE TAILLE LDH
C
      IF (INDICO.EQ.1) THEN
        CALL DGEMV ('T',LDV,LDH,ONE,V,LDV,VECTT,1,ZERO,ZR(X0),1)
        TEMP = DNRM2(LDH,ZR(X0),1)
        CALL DSCAL ( LDH, ONE / TEMP, ZR(X0), 1 )
        EPSF = CRIT2
        GOTO 100
      ELSE
        EPSF = EPSIL
      ENDIF

      ISEED(1) = 1
      ISEED(2) = 3
      ISEED(3) = 5
      ISEED(4) = 7

      CALL DLARNV (2,ISEED,LDH,ZR(X0))
C
C     PROJECTION DANS LA BASE INITIALE
C
      CALL DGEMV ('N',LDV,LDH,ONE,V,LDV,ZR(X0),1,ZERO,VECTT,1)
C
C     PROJECTION DANS SUR LES DDLS_STAB POSITIFS ET CL
C
      DO 10 I = 1,N
        IF (DDLSTA(I).EQ.0 .AND. PROJ.EQ.1) THEN
          IF (VECTT(I).LT.ZERO) THEN
            VECTT(I) = ZERO
          ENDIF
        ELSE 
          VECTT(I) = VECTT(I)*DDLEXC(I)
        ENDIF    
  10  CONTINUE
C
C     RETOUR DANS LA BASE D'ARNOLDI
C  
      CALL DGEMV ('T',LDV,LDH,ONE,V,LDV,VECTT,1,ZERO,ZR(X0),1)
C
C     ON NORME X0
C
      TEMP = DNRM2(LDH,ZR(X0),1)
      CALL DSCAL ( LDH, ONE / TEMP, ZR(X0), 1 )
C
C     ON APPLIQUE LA METHODE DES PUISSANCES
C
 100  CONTINUE

      NORM = 1.D0
      DO 15 J =1,NITMAX
        NPRO = 0
        NUM = 0
        CALL DGEMV ('N',LDH,LDH,ONE,H,LDH,ZR(X0),
     &               1,ZERO,ZR(X),1)
        CALL DGEMV ('N',LDV,LDH,ONE,V,LDV,ZR(X),
     &               1,ZERO,VECTT,1)
        DO 20 I = 1,N
          IF (DDLSTA(I).EQ.0 .AND. PROJ.EQ.1) THEN
            NUM = NUM+1
            IF (VECTT(I).LE.ZERO) THEN
              VECTT(I) = ZERO
              NPRO = NPRO+1
            ENDIF
          ELSE
            VECTT(I) = VECTT(I)*DDLEXC(I)
          ENDIF  
  20    CONTINUE
        CALL DGEMV ('T',LDV,LDH,ONE,V,LDV,VECTT,
     &               1,ZERO,ZR(X),1)
        TEMP = DNRM2(LDH,ZR(X),1)
        CALL DSCAL ( LDH, ONE / TEMP, ZR(X), 1 )
        DO 25 I = 1,LDH
          ZR(BOUNDS+I-1) = ZR(X0+I-1)-ZR(X+I-1)
  25    CONTINUE
        NORM = DNRM2(LDH,ZR(BOUNDS),1)
C
        DO 50 I = 1,LDH
          ZR(X0+I-1) = ZR(X+I-1)
  50    CONTINUE
C
        IF (NORM.LE.EPSF) THEN
C          WRITE(6,*) 'NORM=', NORM
C          WRITE(6,*) 'ITER=', J
C          WRITE(6,*) 'NUM=', NUM
          GOTO 900
        ELSEIF (J.EQ.NITMAX) THEN
C          WRITE(6,*) 'NORM_2=', NORM
C          WRITE(6,*) 'NUM=', NUM
          GOTO 900
        ENDIF
  15  CONTINUE

 900  CONTINUE

C      WRITE(6,*) 'NPRO=', NPRO

      CALL JEDETC('V','&&MPPSTA',1)

C     %---------------%
C     | END OF ENDSTA |
C     %---------------%

      END
