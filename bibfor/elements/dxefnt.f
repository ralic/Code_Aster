      SUBROUTINE DXEFNT(NOMTE,XYZL,PGL,TSUP,TINF,TMOY,SIGT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/02/99   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 NOMTE
      REAL*8 XYZL(3,1),PGL(3,1)
      REAL*8 TSUP(1),TINF(1),TMOY(1)
      REAL*8 SIGT(1)
      LOGICAL GRILLE
C     ------------------------------------------------------------------
C --- EFFORTS GENERALISES D'ORIGINE THERMIQUE AUX NOEUDS
C --- POUR LES ELEMENTS COQUES A FACETTES PLANES :
C --- DST, DKT, DSQ, DKQ, Q4G DUS :
C ---  .A UN CHAMP DE TEMPERATURES SUR LE PLAN MOYEN DONNANT
C ---        DES EFFORTS DE MEMBRANE
C ---  .A UN GRADIENT DE TEMPERATURES DANS L'EPAISSEUR DE LA COQUE
C     ------------------------------------------------------------------
C     IN  NOMTE        : NOM DU TYPE D'ELEMENT
C     IN  XYZL(3,NNO)  : COORDONNEES DES CONNECTIVITES DE L'ELEMENT
C                        DANS LE REPERE LOCAL DE L'ELEMENT
C     IN  PGL(3,3)     : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
C                        LOCAL
C     IN  TSUP(4)      : TEMPERATURES AUX NOEUDS DU PLAN SUPERIEUR
C                        DE LA COQUE
C     IN  TINF(4)      : TEMPERATURES AUX NOEUDS DU PLAN INFERIEUR
C                        DE LA COQUE
C     IN  TMOY(4)      : TEMPERATURES AUX NOEUDS DU PLAN MOYEN
C                        DE LA COQUE
C     OUT SIGT(1)      : EFFORTS  GENERALISES D'ORIGINE THERMIQUE
C                        AUX NOEUDS
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      CHARACTER*2 CODRET(56)
      CHARACTER*10 PHENOM
      CHARACTER*24 DESR
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2)
      REAL*8 N(4)
      INTEGER MULTIC
C     ------------------------------------------------------------------

C --- INITIALISATIONS :
C     -----------------
      CALL JEMARQ()
      ZERO = 0.0D0

      DO 10 I = 1,32
        SIGT(I) = ZERO
   10 CONTINUE

      CALL JEVECH('PMATERC','L',JMATE)
      CALL RCCOMA(ZI(JMATE),'ELAS',PHENOM,CODRET)

      IF ((PHENOM.EQ.'ELAS') .OR. (PHENOM.EQ.'ELAS_COQUE') .OR.
     +    (PHENOM.EQ.'ELAS_COQMU')) THEN

C --- RECUPERATION DE LA TEMPERATURE DE REFERENCE ET
C --- DE L'EPAISSEUR DE LA COQUE
C     --------------------------

        IF (NOMTE(1:8).EQ.'MEGRDKT ') THEN
          GRILLE = .TRUE.
        ELSE
          GRILLE = .FALSE.
        END IF

        CALL JEVECH('PCACOQU','L',JCARA)
        CALL JEVECH('PTEREF','L',JTREF)
        EPAIS = ZR(JCARA)
        TREF = ZR(JTREF)

        DESR = '&INEL.'//NOMTE(1:8)//'.DESR'
        CALL JEVETE(DESR,' ',LZR)


        IF (NOMTE(1:8).EQ.'MEDKTR3 ' .OR. NOMTE(1:8).EQ.'MEDSTR3 ' .OR.
     +      NOMTE(1:8).EQ.'MEGRDKT') THEN

          NNO = 3

C ---- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE
C      -------------------------------------------------
          CALL GTRIA3(XYZL,ZR(LZR))

        ELSE IF (NOMTE(1:8).EQ.'MEDKQU4 ' .OR.
     +           NOMTE(1:8).EQ.'MEDSQU4 ' .OR.
     +           NOMTE(1:8).EQ.'MEQ4QU4 ') THEN
          NNO = 4

C ---- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE
C      ---------------------------------------------------
          CALL GQUAD4(XYZL,ZR(LZR))

        ELSE
          CALL UTMESS('F','DXEFNT','LE TYPE D''ELEMENT : '//NOMTE(1:8)//
     +                'N''EST PAS PREVU.')
        END IF

C --- CALCUL DES MATRICES DE HOOKE DE FLEXION, MEMBRANE,
C --- MEMBRANE-FLEXION, CISAILLEMENT, CISAILLEMENT INVERSE
C     ----------------------------------------------------


        CALL DXMATH(EPAIS,DF,DM,DMF,NNO,PGL,ZR(LZR),MULTIC,INDITH,
     +              GRILLE)
        IF (INDITH.EQ.-1) GO TO 30

C --- BOUCLE SUR LES NOEUDS
C     ---------------------
        DO 20 INO = 1,NNO

C  --      LES COEFFICIENTS SUIVANTS RESULTENT DE L'HYPOTHESE SELON
C  --      LAQUELLE LA TEMPERATURE EST PARABOLIQUE DANS L'EPAISSEUR.
C  --      ON NE PREJUGE EN RIEN DE LA NATURE DU MATERIAU.
C  --      CETTE INFORMATION EST CONTENUE DANS LES MATRICES QUI
C  --      SONT LES RESULTATS DE LA ROUTINE DXMATH.
C          ----------------------------------------
          COE1 = (TSUP(INO)+TINF(INO)+4.D0*TMOY(INO))/6.D0 - TREF
          COE2 = (TSUP(INO)-TINF(INO))/EPAIS

          SIGT(1+8* (INO-1)) = COE1* (DM(1,1)+DM(1,2)) +
     +                         COE2* (DMF(1,1)+DMF(1,2))
          SIGT(2+8* (INO-1)) = COE1* (DM(2,1)+DM(2,2)) +
     +                         COE2* (DMF(2,1)+DMF(2,2))
          SIGT(3+8* (INO-1)) = COE1* (DM(3,1)+DM(3,2)) +
     +                         COE2* (DMF(3,1)+DMF(3,2))
          SIGT(4+8* (INO-1)) = COE2* (DF(1,1)+DF(1,2)) +
     +                         COE1* (DMF(1,1)+DMF(1,2))
          SIGT(5+8* (INO-1)) = COE2* (DF(2,1)+DF(2,2)) +
     +                         COE1* (DMF(2,1)+DMF(2,2))
          SIGT(6+8* (INO-1)) = COE2* (DF(3,1)+DF(3,2)) +
     +                         COE1* (DMF(3,1)+DMF(3,2))
   20   CONTINUE

      END IF

   30 CONTINUE

      CALL JEDEMA()
      END
