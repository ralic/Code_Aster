      SUBROUTINE CALCMI( NP1,NBM,DT0,DT,
     &                   VITG,DEPG,VITG0,DEPG0,FMOD,FMOD0, 
     &                   MASG0,AMOR,AMOR0,PULS,PULS0,
     &                   TRANS,PULSD,S0,Z0,SR0,ZA1,ZA2,ZA3,ZIN)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2000   AUTEUR KXBADNG T.KESTENS 
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
C DESCRIPTION : CALCUL DES DDLS GENERALISES A L'INSTANT N+1 PAR 
C -----------   METHODE INTEGRALE (VERSION MULTI-MODALE)
C
C               APPELANTS : ALITMI, NEWTON
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER     NP1, NBM
      REAL*8      DT0, DT,
     &            VITG(*), DEPG(*), VITG0(*), DEPG0(*),
     &            FMOD(*), FMOD0(*),
     &            MASG0(*), AMOR(*), AMOR0(*), PULS(*), PULS0(*),
     &            TRANS(2,2,*), PULSD(*)
      COMPLEX*16  S0(*), Z0(*), SR0(*), ZA1(*), ZA2(*), ZA3(*), ZIN(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER     ITESTM
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL    ALGINT, INITVE, INTFOR, MATRAN, PARMAT, TSTPAR
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C  0. INITIALISATIONS
C     ---------------
      ITESTM = 0
      CALL INITVE(NP1,DEPG)
      CALL INITVE(NP1,VITG)
C 
C  1. TEST SUR LES MODIF. DES PARAMETRES DE CALCUL
C     --------------------------------------------
      CALL TSTPAR(ITESTM,NBM,AMOR,AMOR0,PULS,PULS0,DT,DT0)       
C
C  2. CALCUL DE LA MATRICE DE TRANSFERT
C     ---------------------------------
      IF ( ITESTM.EQ.0 ) THEN
         CALL PARMAT(NBM,DT,AMOR,PULS,PULSD,S0,Z0,SR0,ZA1,ZA2,ZA3)
         CALL MATRAN(NBM,S0,Z0,PULS,PULSD,TRANS)
      ENDIF
C
C  3. CALCUL DU TERME DE FORCAGE
C     --------------------------
      CALL INTFOR(NBM,FMOD,FMOD0,ZA1,ZA2,ZA3,ZIN)
C
C  4. CALCUL DES DDLS GENERALISES A L'INSTANT N+1
C     -------------------------------------------
      CALL ALGINT(NBM,VITG,VITG0,DEPG,DEPG0,ZIN,TRANS,PULSD,S0)
C
C --- FIN DE CALCMI.
      END
