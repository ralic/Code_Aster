      SUBROUTINE INIPAR(NP1,NBM,NBNL,TESTC,
     &                  CMOD0,CMODCA,KMOD0,KMODCA,
     &                  AMOR,AMOR0,PULS,PULS0,
     &                  ACC,VIT,DEP,ACC0,VIT0,DEP0,
     &                  ACCG,VITG,DEPG,ACCG0,VITG0,DEPG0,
     &                  VITGT,DEPGT,FMODT,
     &                  TCONF1,FTEST0,TCONF2,FTEST)
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
C DESCRIPTION : INITIALISATION DES PARAMETRES POUR PAS DE TEMPS SUIVANT 
C -----------   
C               APPELANT : MDITM2
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER  NP1, NBM,  NBNL, TESTC
      REAL*8   CMOD0(NP1,*), CMODCA(NP1,*), KMOD0(NP1,*), KMODCA(NP1,*),
     &         AMOR(*), AMOR0(*), PULS(*), PULS0(*),
     &         ACC(3,*) , VIT(3,*) , DEP(3,*),
     &         ACC0(3,*) , VIT0(3,*) , DEP0(3,*),
     &         ACCG(*) , VITG(*) , DEPG(*),
     &         ACCG0(*), VITG0(*), DEPG0(*),
     &         VITGT(*) , DEPGT(*), FMODT(*),
     &         TCONF1(4,*), FTEST0, TCONF2(4,*), FTEST
C
C VARIABLES LOCALES
C -----------------
      INTEGER  I, IC, J
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C-----------------------------------------------------------------------
C 1.  REINITIALISATION DES VECTEURS DES DEGRES DE LIBERTE GENERALISES
C-----------------------------------------------------------------------
C
C 1.1 SI LE SYSTEME EST EN VOL A L'INSTANT N+1
C
      IF ( TESTC.EQ.0 ) THEN
C
         DO 10 I = 1, NBM
            DEPG0(I) = DEPG(I)
  10     CONTINUE
         DO 20 I = 1, NBM
            VITG0(I) = VITG(I)
  20     CONTINUE
         DO 30 I = 1, NBM
            ACCG0(I) = ACCG(I)
  30     CONTINUE
C
         DO 70 I = 1, NBM
            AMOR0(I) = AMOR(I)
  70     CONTINUE
         DO 80 I = 1, NBM
            PULS0(I) = PULS(I)
  80     CONTINUE
C
C 1.2 SINON (LE SYSTEME EST EN CHOC A L'INSTANT N+1)
C
      ELSE
C
         DO 110 I = 1, NBM
            DEPG0(I) = DEPG(I)
 110     CONTINUE
         DO 120 I = 1, NBM
            VITG0(I) = VITG(I)
 120     CONTINUE
         DO 130 I = 1, NBM
            ACCG0(I) = ACCG(I)
 130     CONTINUE
C
         DO 170 I = 1, NBM
            AMOR0(I) = AMOR(I)
 170     CONTINUE
         DO 180 I = 1, NBM
            PULS0(I) = PULS(I)
 180     CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C 2.  REINITIALISATION DES MATRICES DE RAIDEURS ET D'AMORTISSEMENTS
C     GENERALISES DU SYSTEME
C-----------------------------------------------------------------------
C
      DO 200 J = 1, NBM
         DO 201 I = 1, NBM
            KMOD0(I,J) = KMODCA(I,J)
 201     CONTINUE
 200  CONTINUE
C
      DO 210 J = 1, NBM
         DO 211 I = 1, NBM
            CMOD0(I,J) = CMODCA(I,J)
 211     CONTINUE
 210  CONTINUE
C
C-----------------------------------------------------------------------
C 3.  REINITIALISATION DES VECTEURS ACCELERATIONS, VITESSES ET
C     DEPLACEMENTS PHYSIQUES AUX NOEUDS DE CHOC
C-----------------------------------------------------------------------
C
      DO 300 IC = 1, NBNL
         DO 301 I = 1, 3
            ACC0(I,IC) = ACC(I,IC)
 301     CONTINUE
 300  CONTINUE
C
      DO 310 IC = 1, NBNL
         DO 311 I = 1, 3
            VIT0(I,IC) = VIT(I,IC)
 311     CONTINUE
 310  CONTINUE
C
      DO 320 IC = 1, NBNL
         DO 321 I = 1, 3
            DEP0(I,IC) = DEP(I,IC)
 321     CONTINUE
 320  CONTINUE
C
      DO 330 IC = 1, NBNL
         DO 331 I = 1, 4
            TCONF1(I,IC) = TCONF2(I,IC)
 331     CONTINUE
 330  CONTINUE
      FTEST0 = FTEST
C
C --- FIN DE INIPAR.
      END
