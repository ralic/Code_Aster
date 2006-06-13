      SUBROUTINE CALFMN(NP1,NBM,TESTC,
     &                  FMOD0,FMOD00,CMOD,KMOD,VITG0,DEPG0)
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
C-----------------------------------------------------------------------
C DESCRIPTION : CALCUL DE LA FORCE MODALE A L'INSTANT N
C -----------
C               APPELANTS : ALITMI, NEWTON
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER   NP1, NBM, TESTC
      REAL*8    FMOD0(*), FMOD00(*), CMOD(NP1,*), KMOD(NP1,*),
     &          VITG0(*), DEPG0(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER   I, IER
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL  PRMAVE, UTMESS
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IF ( TESTC.EQ.0 ) THEN
C
         DO 10 I = 1, NBM
            FMOD0(I) = FMOD00(I)
  10     CONTINUE
C
      ELSE
C
         IER = 0
         CALL PRMAVE(0,KMOD,NP1,NBM,NBM,DEPG0,NBM,FMOD0,NBM,IER)
         IF ( IER.NE.0 )
     &      CALL UTMESS('F','CALFMN','TAILLES INCOMPATIBLES POUR LE '//
     &                               'PRODUIT MATRICE * VECTEUR')
C
         IER = 0
         CALL PRMAVE(1,CMOD,NP1,NBM,NBM,VITG0,NBM,FMOD0,NBM,IER)
         IF ( IER.NE.0 )
     &      CALL UTMESS('F','CALFMN','TAILLES INCOMPATIBLES POUR LE '//
     &                               'PRODUIT MATRICE * VECTEUR')
C
         DO 20 I = 1, NBM
            FMOD0(I) = FMOD00(I) - FMOD0(I)
  20     CONTINUE
C
      ENDIF
C
C --- FIN DE CALFMN.
      END
