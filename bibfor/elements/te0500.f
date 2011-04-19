      SUBROUTINE TE0500(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    - FONCTION REALISEE:  CALCUL DE L'ESTIMATEUR D'ERREUR TEMPORELLE
C      SUR UN ELEMENT ISOPARAMETRIQUE POUR LES MODELISATIONS HM SATUREES
C
C      --> OPTION 'ERRE_TEMPS'
C
C IN OPTION : NOM DE L'OPTION
C IN NOMTE  : NOM DU TYPE D'ELEMENT
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES :
C       MESSAGE              : U2MESS,U2MESK.
C       JEVEUX               : JEMARQ,JEDEMA.
C       CHAMPS LOCAUX        : JEVECH,TECACH,TECAEL.
C       ENVIMA               : R8MIEM.
C       MATERIAUX/CHARGES    : RCVALA,RCCOMA.
C       DEDIEES A TE0500     : CAETHM
C     FONCTIONS INTRINSEQUES : SQRT.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS :
C       25/02/08 (SM) : CREATION POUR CALCUL INDICATEUR D'ERREUR
C                       TEMPORELLE EN INSTATIONNAIRE .
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16 OPTION,NOMTE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION VARIABLES LOCALES
C
      INTEGER NBRE1,NBRR1
      PARAMETER ( NBRE1 = 1 , NBRR1 = 3 )
C
      INTEGER NBRE2
      PARAMETER ( NBRE2 = 2 )
C
      INTEGER NBRE3
      PARAMETER ( NBRE3 = 1 )
C
      INTEGER NDIM,NNO
C
      INTEGER IPI,KPI,IAUX,NPG,IGEOM,JGANO,IMATE,IERRE,IGRDCA,
     &        IRET,ISIGAP,ISIGAM,ITAB(7),NBCMP,TYPVF,IBID
      INTEGER DIMDEP,DIMDEF,DIMCON
      INTEGER IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2
      INTEGER NMEC,NPI,NP1,NP2,NNOS,NNOM,NDDLS,NDDLM
      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMUEL
      INTEGER LXLGUT
C
      REAL*8 POIDS2
      REAL*8 R8MIEM,OVFL
      REAL*8 VALRE1(NBRE1),VALRR1(NBRR1),VALRE2(NBRE2),VALRE3(NBRE3)
      REAL*8 LONGC,PRESC,MYOUNG
      REAL*8 VALPAR(1),TIME,RAUX,RHOLIQ,VISCLI,PERMIN
      REAL*8 FLUHPX,FLUHMX,FLUHPY,FLUHMY,RBID81(9)
      REAL*8 TERTPS
C
      LOGICAL     LAXI,PERMAN,VF
C
      INTEGER CODME1(NBRE1),CODMR1(NBRR1),CODME2(NBRE2),
     &             CODME3(NBRE3)
      CHARACTER*3  MODINT
      CHARACTER*4  NOMPAR(1)
      CHARACTER*8  TYPMOD(2),VALK
      CHARACTER*8  NOMRE1(NBRE1),NOMRR1(NBRR1),
     &             NOMRE2(NBRE2),NOMRE3(NBRE3)
C
      DATA NOMRE1 / 'PERM_IN'    /
      DATA NOMRR1 / 'PERMIN_X','PERMIN_Y','PERMIN_Z' /
      DATA NOMRE2 / 'RHO','VISC' /
      DATA NOMRE3 / 'E'          /
C
C ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      OVFL = R8MIEM()
C
C =====================================================================
C 1. RECUPERATION D'INFORMATIONS SUR L'ELEMENT THM
C =====================================================================
      IBID = 0
      TYPVF = 0
      VF = .FALSE.
      CALL CAETHM( NOMTE , LAXI  , PERMAN,VF,TYPVF,
     &             TYPMOD, MODINT, MECANI, PRESS1, PRESS2, TEMPE,
     &             DIMDEP, DIMDEF, DIMCON, NMEC  ,
     &             NP1   , NP2   , NDIM  , NNO   ,
     &             NNOS  , NNOM  , IBID, NPI   , NPG   ,
     &             NDDLS , NDDLM ,IBID ,IBID, DIMUEL, IPOIDS,
     &             IVF   , IDFDE , IPOID2, IVF2  , IDFDE2, IBID,JGANO )
C =====================================================================
C 2. RECUPERATION DES PARAMETRES TEMPORELS
C =====================================================================
      CALL TECACH('ONN','PTEMPSR',1,ITAB,IRET)
      IF ( IRET.EQ.0 ) THEN
        TIME = ZR(ITAB(1))
      ELSE
        CALL U2MESS('F','INDICATEUR_11')
      ENDIF
C =====================================================================
C 3. INITIALISATIONS/RECUPERATION DE LA GEOMETRIE ET DES CHAMPS LOCAUX
C =====================================================================
C
C 3.1. GEOMETRIE (IGEOM)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C 3.2. MATERIAU (IMATE)
C
      CALL JEVECH('PMATERC','L',IMATE)
C
C 3.3 CONTRAINTES ( T- ET T+ )
C
      CALL JEVECH('PCONTGM','L',ISIGAM)
      CALL TECACH('ONN','PCONTGP',3,ITAB,IRET)
C
      ISIGAP = ITAB(1)
      NBCMP  = ITAB(2)/NPI
C
C 3.4  RECHERCHE DES VALEURS NECESSAIRES AU CALCUL DE L'INDICATEUR
C
C --- A. GRANDEURS CARACTERISTIQUES

      CALL JEVECH('PGRDCA','L',IGRDCA)
      LONGC = ZR(IGRDCA)
      PRESC = ZR(IGRDCA+1)

C --- B. PERMEABILITE INTRINSEQUE DU MILIEU
C
C => PERMIN SI ISOTROPE
C => PERMIN_X,PERMIN_Y ET PERMIN_Z SINON
C
      NOMPAR(1) = 'INST'
      VALPAR(1) = TIME
C
      CALL RCVALA ( ZI(IMATE), ' ', 'THM_DIFFU', 1, NOMPAR, VALPAR,
     &              NBRE1, NOMRE1, VALRE1, CODME1, 0)
C
      IF ( CODME1(1).EQ.0 ) THEN
        PERMIN = VALRE1(1)
      ELSE IF ( CODME1(1).EQ.1 ) THEN
        CALL RCVALA ( ZI(IMATE), ' ', 'THM_DIFFU', 1, NOMPAR, VALPAR,
     &              NBRR1, NOMRR1, VALRR1, CODMR1, 0)
        IF (( CODMR1(1).EQ.0 ).AND.( CODMR1(2).EQ.0 ).AND.
     &      ( CODMR1(3).EQ.0 )) THEN
          PERMIN = SQRT(VALRR1(1)**2+VALRR1(2)**2+VALRR1(3)**2)
        ENDIF
      ELSE
        CALL U2MESK('F','ELEMENTS4_78',1,NOMRE1(1))
      ENDIF

C --- C. MASSE VOLUMIQUE DU LIQUIDE
C        VISCOSITE DYNAMIQUE DU LIQUIDE

      CALL RCVALA ( ZI(IMATE), ' ', 'THM_LIQU', 1, NOMPAR, VALPAR,
     &                NBRE2, NOMRE2, VALRE2, CODME2, 1)
C
      IF ( CODME2(1).EQ.0 .AND. CODME2(2).EQ.0 ) THEN
        RHOLIQ   = VALRE2(1)
        VISCLI   = VALRE2(2)
      ELSE
        CALL U2MESK('F','ELEMENTS4_69',1,NOMRE2(1)//NOMRE2(2))
      ENDIF

C --- D. MODULE DE YOUNG

      CALL RCVALA ( ZI(IMATE), ' ', 'ELAS', 1, NOMPAR, VALPAR,
     &                NBRE3, NOMRE3, VALRE3, CODME3, 1)
C
      IF ( CODME3(1).EQ.0 ) THEN
        MYOUNG = VALRE3(1)
      ELSE
        CALL U2MESK('F','ELEMENTS4_71',1,NOMRE3(1))
      ENDIF
C
C 3.5 CALCUL DU COEFFICIENT D'ADIMENSIONNEMENT
C
      IF ( ABS(LONGC) .GT. OVFL ) THEN
        RAUX = (MYOUNG*VISCLI)/(LONGC**NDIM)
C
        IF (RHOLIQ.GT.OVFL) THEN
          RAUX = RAUX/(RHOLIQ**2)
C
          IF (PRESC.GT.OVFL) THEN
            RAUX = RAUX/(PRESC**2)
C
            IF (PERMIN.GT.OVFL) THEN
              RAUX = RAUX/PERMIN
C
            ELSE
              CALL U2MESS('F','INDICATEUR_20')
            ENDIF
          ELSE
            VALK = 'pression'
            CALL U2MESK('F', 'INDICATEUR_21', 1, VALK )
          ENDIF
        ELSE
          CALL U2MESS('F','INDICATEUR_22')
        ENDIF
      ELSE
        VALK = 'longueur'
        CALL U2MESK('F', 'INDICATEUR_21', 1, VALK )
      ENDIF
C
C =====================================================================
C 4. CALCUL DE L'INDICATEUR TEMPOREL
C =====================================================================
C
C 4.1. INITIALISATION
C
      TERTPS = 0.D0
C
C --- BOUCLE SUR LES POINTS DE GAUSS
C
      DO 10 , IPI = 1,NPG
C
        KPI = IPI
C
        IF ( NDIM .EQ. 2 ) THEN
C =====================================================================
C => EN DIMENSION 2
C =====================================================================
C
C 4.2. ON RECUPERE LES POIDS D'INTEGRATION AUX POINTS DE GAUSS
C
          CALL DFDM2D(NNOS,KPI,IPOID2,IDFDE2,ZR(IGEOM),
     &                RBID81,RBID81,POIDS2)
C
          IAUX = NBCMP*(KPI-1)
C
          FLUHPX = ZR(ISIGAP+IAUX+8)
          FLUHMX = ZR(ISIGAM+IAUX+8)
C
          FLUHPY = ZR(ISIGAP+IAUX+9)
          FLUHMY = ZR(ISIGAM+IAUX+9)
C
          TERTPS = TERTPS +
     &             RAUX*POIDS2*((FLUHPX-FLUHMX)**2+(FLUHPY-FLUHMY)**2)
        ELSE
          IAUX = LXLGUT(OPTION)
          CALL U2MESK ( 'F', 'INDICATEUR_92', 1, OPTION(1:IAUX) )
        ENDIF
C
 10   CONTINUE
C
C --- FIN BOUCLE SUR LES POINTS DE GAUSS
C
C =====================================================================
C 5. STOCKAGE DE L'ERREUR
C =====================================================================
C
      CALL JEVECH('PERREUR','E',IERRE)
C
      ZR(IERRE)   = TERTPS
C
      CALL JEDEMA()
C
      END
