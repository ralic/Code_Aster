      SUBROUTINE UTTCPL (DIM,NBMESU,NOMC,NOML,PRPAL)
      IMPLICIT NONE
      INTEGER DIM,NBMESU
      CHARACTER*1 PRPAL(DIM)
      CHARACTER*24 NOMC(DIM)
      CHARACTER*80 NOML(DIM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 08/02/2010   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE PELLET J.PELLET
C ----------------------------------------------------------------------
C  RETOURNE LA LISTE DES DIFFERENTES MESURES DE TEMPS GENERALES
C
C  IN DIM : DIMENSION DES TABLEAUX NOMC, NOML ET PRPAL
C  OUT NBMESU : NOMBRE DE MESURES
C  OUT NOMC(IMES) : NOM COURT DE LA MESURE
C  OUT NOML(IMES) : NOM LONG DE LA MESURE
C  OUT PRPAL(IMES) :  'P' : MESURE PRICIPALE
C                     'S' : MESURE SECONDAIRE
C ----------------------------------------------------------------------
      INTEGER NBMAX,K,I1,I2
      PARAMETER (NBMAX=20)
      CHARACTER*80 D1(NBMAX)

C     -- COMMONS POUR MESURE DE TEMPS :
      INTEGER  MTPNIV ,INDMAX
      PARAMETER (INDMAX=5)
      CHARACTER*80 SNOLON(INDMAX)
      REAL*8 VALMES(INDMAX*7),VALMEI(INDMAX*7)
      COMMON /MESTP1/  MTPNIV
      COMMON /MESTP2/ SNOLON
      COMMON /MESTP3/ VALMES,VALMEI
C     ------------------------------------------------------------------

C     -- SI L'UTILISATEUR NE VEUT PAS DE MESURE, NBMESU=0
      IF (MTPNIV.EQ.0) THEN
        NBMESU=0
        GOTO 9999
      ENDIF

C     ON ECRIT LES DONNEES DANS LE TABLEAU D1 :
C     -----------------------------------------------------
      D1(1)='CPU.RESO.1|P|#1 Resolution des systemes lineaires'
      D1(2)='CPU.RESO.2|S|#1.1 Numerotation, connectivite de la matrice'
      D1(3)='CPU.RESO.3|S|#1.2 Factorisation symbolique'
      D1(4)='CPU.RESO.4|S|#1.3 Factorisation numerique (ou precond.)'
      D1(5)='CPU.RESO.5|S|#1.4 Resolution'

      D1(6) ='CPU.CALC.1|P|#2 Calculs elementaires et assemblages'
      D1(7) ='CPU.CALC.2|S|#2.1 Routine calcul'
      D1(8) ='CPU.CALC.3|S|#2.1.1 Routines te00ij'
      D1(9) ='CPU.ASSE.1|S|#2.2 Assemblages'
      D1(10)='CPU.ASSE.2|S|#2.2.1 Assemblage matrices'
      D1(11)='CPU.ASSE.3|S|#2.2.2 Assemblage seconds membres'

      D1(12)='CPU.COFR.1|P|#3 Contact, frottement'

      D1(13)='CPU.MEMD.1|P|#4 Dechargement de la memoire sur disque'
      D1(14)='CPU.MEMD.2|P|#4 ??? libre pour mesure interne jeveux'

      NBMESU=14
      CALL ASSERT(NBMAX.GE.NBMESU)
      CALL ASSERT(DIM.GE.NBMESU)



C     ON "SPLITE" D1 DANS NOMC, NOML ET PRPAL :
C     -----------------------------------------------------
      DO 1, K=1,NBMESU
        I1= INDEX(D1(K),'|')
        NOMC(K)=D1(K)(1:I1-1)
        I2= INDEX(D1(K)(I1+1:),'|')
        CALL ASSERT (I2.EQ.2)
        PRPAL(K)=D1(K)(I1+1:I1+2)
        CALL ASSERT(PRPAL(K).EQ.'P'.OR.PRPAL(K).EQ.'S')
        NOML(K)=D1(K)(I1+I2+1:)

1     CONTINUE

9999  CONTINUE
      END
