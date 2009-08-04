      SUBROUTINE CALRES ( NP3,IC,TYPCH,NBSEG,CHOC,RC,THETA,
     &                    VLOC,XLOC,VLOC0,XLOC0,EXCLOC,TETAJ,
     &                    JACOBC,JACOBK,FLOC,FLRES,OLD,OLDIA,IFORN,TOLN)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/08/2009   AUTEUR MEUNIER S.MEUNIER 
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
C DESCRIPTION : CALCUL DES FORCES NON-LINEAIRES RESIDUELLES
C -----------
C               APPELANT : MDCHOE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER    NP3, IC, TYPCH(*), NBSEG(*)
      REAL*8     CHOC(5,*), RC(NP3,*), THETA(NP3,*),
     &           VLOC(*), XLOC(*), VLOC0(*), XLOC0(*), EXCLOC(*), TETAJ,
     &           JACOBC(3,*), JACOBK(3,*), FLOC(*), FLRES(*), OLD(9,*)
      INTEGER    OLDIA(*), IFORN
      REAL*8     TOLN
C
C VARIABLES LOCALES
C -----------------
      INTEGER    TYPOBS, NBS, I, IADHER, IER
      REAL*8     YTEMP(3),
     &           XJEU, SINT, COST, DNORM, TOLCH,
     &           FN, VN, CFROT, KN, CN, KT, CT,
     &           OLDVT(2), OLDFT(2), OLDXLO(3), FT(2), VT(2),
     &           XLOCJ(3), ULOCJ(3), VLOCJ(3)
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  ABS
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL   DISBUT, FORNOR, FORTAN, MATINI, INITVE, PRMAVE
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      TOLCH = 10.0D0 * TOLN
C
      DO 10 I = 1, 3
         VLOCJ(I)=(TETAJ*VLOC(I)) + ((1.0D0-TETAJ)*VLOC0(I))
         XLOCJ(I)=(TETAJ*XLOC(I)) + ((1.0D0-TETAJ)*XLOC0(I))
         ULOCJ(I)=XLOCJ(I) - EXCLOC(I)
  10  CONTINUE
C
C 1.  CALCUL DE LA DISTANCE NORMALE
C     -----------------------------
C
      TYPOBS = TYPCH(IC)
      NBS    = NBSEG(IC)
      IF ( (TYPOBS.EQ.0).OR.(TYPOBS.EQ.1).OR.(TYPOBS.EQ.2) )
     &   XJEU = RC(1,IC)
C
      CALL DISBUT(NP3,IC,XLOCJ,TYPOBS,XJEU,RC,THETA,NBS,COST,SINT,DNORM)
C
      IF ( ABS(DNORM).LT.TOLCH ) DNORM = 0.0D0
C
C 2.  SI CHOC CALCUL DES FORCES NORMALES ET TANGENTIELLES
C     ---------------------------------------------------
C
      IF ( DNORM.LT.0.0D0 ) THEN
C
         KN     = CHOC(1,IC)
         CN     = CHOC(2,IC)
         CFROT  = CHOC(5,IC)
C
         CALL FORNOR ( DNORM,VLOCJ,KN,CN,COST,SINT,FN,FLOC,VN,IFORN)
C
         OLD(8,IC) = FN
         OLD(9,IC) = VN
C
         IF ( CFROT.NE.0.0D0 ) THEN
C
            KT = CHOC(3,IC)
            CT = CHOC(4,IC)
            IADHER = OLDIA(IC)
            OLDVT(1)  = OLD(1,IC)
            OLDVT(2)  = OLD(2,IC)
            OLDFT(1)  = OLD(3,IC)
            OLDFT(2)  = OLD(4,IC)
            OLDXLO(1) = OLD(5,IC)
            OLDXLO(2) = OLD(6,IC)
            OLDXLO(3) = OLD(7,IC)
            CALL FORTAN ( FN,XLOCJ,VLOCJ,CFROT,KT,CT,IADHER,
     &                    OLDVT,OLDFT,OLDXLO,COST,SINT,FT,FLOC,VT)
            OLDIA(IC) = IADHER
            OLD(1,IC) = OLDVT(1)
            OLD(2,IC) = OLDVT(2)
            OLD(3,IC) = OLDFT(1)
            OLD(4,IC) = OLDFT(2)
            OLD(5,IC) = OLDXLO(1)
            OLD(6,IC) = OLDXLO(2)
            OLD(7,IC) = OLDXLO(3)
C
         ENDIF
C
         IER = 0
         CALL PRMAVE ( 0,JACOBC,3,3,3,VLOCJ,3,YTEMP,3,IER)
         IF ( IER.NE.0 )
     &      CALL U2MESS('F','ALGORITH_71')
C
         IER = 0
         CALL PRMAVE ( 1,JACOBK,3,3,3,ULOCJ,3,YTEMP,3,IER)
         IF ( IER.NE.0 )
     &      CALL U2MESS('F','ALGORITH_71')
C
         FLRES(1) = FLOC(1) - YTEMP(1)
         FLRES(2) = FLOC(2) - YTEMP(2)
         FLRES(3) = FLOC(3) - YTEMP(3)
C
      ELSE
C
C 3. SINON INITIALISATION DES MATR. JACOB. ET DE FORCES RESIDUELLES.
C    ---------------------------------------------------------------
C
         CALL MATINI(3,3,0.D0,JACOBC)
         CALL MATINI(3,3,0.D0,JACOBK)
         OLD(1,IC) = -SINT*VLOC(2) + COST*VLOC(3)
         OLD(2,IC) = VLOC(1)
         OLD(3,IC) = 0.D0
         OLD(4,IC) = 0.D0
         OLD(5,IC) = XLOC(1)
         OLD(6,IC) = XLOC(2)
         OLD(7,IC) = XLOC(3)
         OLD(8,IC) = 0.D0
         OLD(9,IC) = COST*VLOC(2) + SINT*VLOC(3)
         OLDIA(IC) = 0
         CALL INITVE(3,FLOC)
         CALL INITVE(3,FLRES)
C
      ENDIF
C
C --- FIN DE CALRES.
      END
