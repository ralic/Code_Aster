      SUBROUTINE CALVOL(NP1,NBM,ICOUPL,INDIC,
     &                  KMOD00,CMOD00,AMOR00,PULS00,PULSI,AMORI,MASGI,
     &                  TPFL,VECI1,VECR1,VECR2,VECR5,VECR3,VGAP,VECR4,
     &                  LOCFL0,AMFLU0,XSI0)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/07/2000   AUTEUR CIBHHAB N.RAHNI 
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
C DESCRIPTION : CALCUL DES CARACTERISTIQUES EN VOL
C -----------
C               APPELANT : MDITM2
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER      NP1, NBM, ICOUPL, INDIC
      REAL*8       KMOD00(NP1,*), CMOD00(NP1,*), AMOR00(*), PULS00(*),
     &             PULSI(*), AMORI(*), MASGI(*)
      CHARACTER*8  TPFL
      INTEGER      VECI1(*)
      REAL*8       VECR1(*), VECR2(*), VECR5(*), VECR3(*),
     &             VGAP, VECR4(*)
      LOGICAL      LOCFL0(*)
      REAL*8       AMFLU0(NP1,*), XSI0(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER      I
      REAL*8       XCF, R8B1, R8B2
      COMPLEX*16   C16B
      LOGICAL      LK
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL     COEFMO, INITMA, INITVE
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL INITMA(NP1,NP1,KMOD00)
      CALL INITMA(NP1,NP1,CMOD00)
      CALL INITMA(NP1,NP1,AMFLU0)
      CALL INITVE(NP1,AMOR00)
      CALL INITVE(NP1,PULS00)
C
      DO 10 I = 1, NBM
         KMOD00(I,I) = PULSI(I)*PULSI(I)
         CMOD00(I,I) = AMORI(I)/MASGI(I)
         AMOR00(I)   = AMORI(I)/MASGI(I)
         PULS00(I)   = PULSI(I)
  10  CONTINUE
C
      IF ( ICOUPL.EQ.1 ) THEN
C....... LK = .FALSE. INDIQUE QU'ON NE CALCULE PAS LES TERMES DE RAIDEUR
         LK = .FALSE.
         DO 20 I = 1, NBM
            IF ( LOCFL0(I) ) THEN
            write(*,*) 'CALVOL ::::TPFL',tpfl
               CALL COEFMO(TPFL,LK,NBM,I,INDIC,R8B1,PULS00(I),VGAP,
     &                     XSI0(I),MASGI,VECI1,VECR1,VECR2,VECR3,VECR4,
     &                     VECR5,R8B2,C16B,XCF)
               AMFLU0(I,I) = XCF/MASGI(I)
            ENDIF
  20     CONTINUE
      ENDIF
C
C --- FIN DE CALVOL.
      END
