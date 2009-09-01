      SUBROUTINE CALTRA( NP1,NP4,NBM,NFOUR,TTRANS,TTRAN0,
     &                   VITGTR,DEPGTR,VITG0,DEPG0,
     &                   MASGI,AMOR,PULS,PULSD,MTRANS,
     &                   S0,Z0,SR0,ZA1,ZA2,ZA3,ZA4,ZA5,ZITR,ZIN,
     &                   FEXTT0,FEXTTR,DTTR,OMEGAF,AA,BB,NTRANS)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/09/2009   AUTEUR SELLENET N.SELLENET 
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
C TOLE  CRP_21
C-----------------------------------------------------------------------
C DESCRIPTION : CALCUL DES DDLS GENERALISES A L'INSTANT TTRANS PAR 
C ------------  METHODE INTEGRALE (VERSION MULTI-MODALE) PERMETTANT
C               LE PASSAGE DU TRANSITOIRE
C
C               APPELANT : TRANSI
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER     NP1, NP4, NBM, NFOUR
      REAL*8      TTRANS, TTRAN0,
     &            VITGTR(*), DEPGTR(*), VITG0(*), DEPG0(*),
     &            MASGI(*), AMOR(*), PULS(*), PULSD(*), MTRANS(2,2,*)
      COMPLEX*16  S0(*), Z0(*), SR0(*), ZA1(*), ZA2(*), ZA3(*),
     &            ZA4(NP4,*), ZA5(NP4,*), ZITR(*), ZIN(*)
      REAL*8      FEXTT0(*), FEXTTR(*), DTTR,
     &            OMEGAF(*), AA(NP4,*), BB(NP4,*)
      INTEGER     NTRANS
C
C VARIABLES LOCALES
C -----------------
      INTEGER     I
      REAL*8      DTTRAN
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL    ALGINT, LCINVN, INTFOR, INTFTR, MATRAN, PARMAT, PARMTR
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C-----------------------------------------------------------------------
C  0. INITIALISATIONS
C-----------------------------------------------------------------------
C
      CALL VECINI(NP1,0.D0,DEPGTR)
      CALL VECINI(NP1,0.D0,VITGTR)
C
C-----------------------------------------------------------------------
C  1. PREMIERE ESTIMATION DE LA DUREE DU TRANSITOIRE
C-----------------------------------------------------------------------
C
      IF ( NTRANS.EQ.0 ) THEN
C
C--1.1   CALCUL DES PARAMETRES METHODE INTEGRALE
C
         DTTRAN = TTRANS - TTRAN0
         CALL PARMTR(NP4,NFOUR,NBM,DTTRAN,AMOR,PULS,PULSD,S0,Z0,
     &               OMEGAF,ZA4,ZA5)
C
C--1.2   CALCUL DE LA MATRICE DE TRANSFERT
C
         CALL MATRAN(NBM,S0,Z0,PULS,PULSD,MTRANS)
C
C--1.3   CALCUL DU TERME DE FORCAGE
C
         CALL INTFTR(NP4,NFOUR,NBM,ZA4,ZA5,AA,BB,ZITR)
         DO 13 I = 1, NBM
            ZITR(I) = ZITR(I) / MASGI(I)
  13     CONTINUE
C
C--1.4   CALCUL DES DDLS GENERALISES A L'INSTANT N+1
C
         CALL ALGINT(NBM,VITGTR,VITG0,DEPGTR,DEPG0,ZITR,MTRANS,PULSD,S0)
C
C-----------------------------------------------------------------------
C  2. DEUXIEME ESTIMATION DE LA DUREE DU TRANSITOIRE
C-----------------------------------------------------------------------
C
      ELSE IF ( NTRANS.EQ.1 ) THEN
C
C--2.1   CALCUL DES PARAMETRES METHODE INTEGRALE
C
         CALL PARMAT(NBM,DTTR,AMOR,PULS,PULSD,S0,Z0,SR0,
     &               ZA1,ZA2,ZA3)
C
C--2.2   CALCUL DE LA MATRICE DE TRANSFERT
C
         CALL MATRAN(NBM,S0,Z0,PULS,PULSD,MTRANS)
C
C--2.3   CALCUL DU TERME DE FORCAGE
C
         CALL INTFOR(NBM,FEXTTR,FEXTT0,ZA1,ZA2,ZA3,ZIN)
         DO 23 I = 1, NBM
            ZIN(I) = ZIN(I) / MASGI(I)
  23     CONTINUE
C
C--2.4   CALCUL DES DDLS GENERALISES A L'INSTANT N+1
C
         CALL ALGINT(NBM,VITGTR,VITG0,DEPGTR,DEPG0,ZIN,MTRANS,PULSD,S0)
C
C-----------------------------------------------------------------------
C  3. ESTIMATIONS ULTERIEURES
C-----------------------------------------------------------------------
C
      ELSE
C
C--3.1   CALCUL DU TERME DE FORCAGE
C
         CALL INTFOR(NBM,FEXTTR,FEXTT0,ZA1,ZA2,ZA3,ZIN)
         DO 31 I = 1, NBM
            ZIN(I) = ZIN(I) / MASGI(I)
  31     CONTINUE
C
C--3.2   CALCUL DES DDLS GENERALISES A L'INSTANT N+1
C
         CALL ALGINT(NBM,VITGTR,VITG0,DEPGTR,DEPG0,ZIN,MTRANS,PULSD,S0)
C
      ENDIF
C
C --- FIN DE CALTRA.
      END
