      SUBROUTINE TE0469(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C.......................................................................
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     BUT: CONSTRUCTION DU VECTEUR DES FORCES CORRESPONDANT A UN
C          CHARGEMENT FORCE_ARETE POUR LES ELEMENTS ISOPARAMETRIQUES 3D.
C          (I.E. CALCUL DU VECTEUR DES FORCES NODALES EQUIVALENTES
C                A UNE FORCE LINEIQUE LE LONG D'UNE ARETE POUR
C                LES ELEMENTS 3D).
C
C          OPTIONS : 'CHAR_MECA_FR1D3D'
C                    'CHAR_MECA_FF1D3D'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      PARAMETER         ( NBNOMX = 27 )
      PARAMETER         ( NPG1   = 4 )
      CHARACTER*8        NOMPAR(4), ELREFE
      CHARACTER*16       NOMTE, OPTION
      CHARACTER*24       CHVAL, CHCTE
      REAL*8             FX(NBNOMX), FY(NBNOMX), FZ(NBNOMX), VALPAR(4)
      REAL*8             XYZGAU(3), FORLIN(3), INSTAN, JACOB
      REAL*8             FXLIN(NPG1), FYLIN(NPG1), FZLIN(NPG1)
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C
      CALL ELREF1(ELREFE)
C
C
C --- CARACTERISTIQUES DU TYPE D'ELEMENT :
C --- INTEGRATION ET INTERPOLATION
C      ----------------------------
      CHCTE = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CHCTE,'L',JIN)
      NNO   = ZI(JIN+1-1)
      NPG   = ZI(JIN+3-1)
      CHVAL = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(CHVAL,'L',JVAL)
      IPOIDS = JVAL
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
C
      DO 10 I = 1, NBNOMX
         FX(I) = ZERO
         FY(I) = ZERO
         FZ(I) = ZERO
 10   CONTINUE
C
      DO 20 I = 1, NPG1
         FXLIN(I) = ZERO
         FYLIN(I) = ZERO
         FZLIN(I) = ZERO
 20   CONTINUE
C
C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C     ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C --- OPTION 'CHAR_MECA_FR1D3D'
C --- CAS OU LES DONNEES DES FORCES LINEIQUES SONT DES REELS :
C     ------------------------------------------------------
      IF (OPTION.EQ.'CHAR_MECA_FR1D3D') THEN
C
C ---  RECUPERATION DES VALEURS DE LA FORCE LINEIQUE A APPLIQUER
C ---  SUR L'ELEMENT D'ARETE :
C      ---------------------
        CALL JEVECH('PFR1D3D','L',IDFLIN)
C
C ---  BOUCLE SUR LES POINTS D'INTEGRATION
C      -----------------------------------
         DO 30 IGAU = 1, NPG
C
            IDECPG = NNO*(IGAU-1)

C
C ---    CALCUL DE LA FORCE LINEIQUE AUX POINTS D'INTEGRATION :
C        -----------------------------------------------------
            DO 40 INO = 1, NNO
C
               FXLIN(IGAU) = FXLIN(IGAU) + ZR(IVF+IDECPG+INO-1)*
     +                                     ZR(IDFLIN+1-1)
               FYLIN(IGAU) = FYLIN(IGAU) + ZR(IVF+IDECPG+INO-1)*
     +                                     ZR(IDFLIN+2-1)
               FZLIN(IGAU) = FZLIN(IGAU) + ZR(IVF+IDECPG+INO-1)*
     +                                     ZR(IDFLIN+3-1)
  40        CONTINUE
  30     CONTINUE
C
C ---    BOUCLE SUR LES POINTS D'INTEGRATION
C        -----------------------------------
         DO 50 IGAU = 1, NPG
C
            IDECPG = NNO*(IGAU-1)
C
C ---    CALCUL DU PRODUIT JACOBIEN*POIDS AU POINT D'INTEGRATION
C ---    COURANT :
C        -------
            CALL VFF3D(NNO, ZR(IPOIDS+IGAU-1), ZR(IDFDE+IDECPG),
     +                 ZR(IGEOM), JACOB)
C
C ---    CALCUL DE LA CONTRIBUTION AU VECTEUR DES FORCES NODALES
C ---    DU CHARGEMENT LINEIQUE AU POINT D'INTEGRATION COURANT :
C        -----------------------------------------------------
            DO 60 INO = 1, NNO
C
               FX(INO) = FX(INO) + ZR(IVF+IDECPG+INO-1)*FXLIN(IGAU)
     +                             *JACOB
               FY(INO) = FY(INO) + ZR(IVF+IDECPG+INO-1)*FYLIN(IGAU)
     +                             *JACOB
               FZ(INO) = FZ(INO) + ZR(IVF+IDECPG+INO-1)*FZLIN(IGAU)
     +                             *JACOB
  60        CONTINUE
C
  50     CONTINUE
C
C --- OPTION 'CHAR_MECA_FF1D3D'
C --- CAS OU LES DONNEES DES FORCES LINEIQUES SONT DES FONCTIONS
C --- DES COORDONNEES ET DU TEMPS :
C     ---------------------------
      ELSEIF (OPTION.EQ.'CHAR_MECA_FR1D3D') THEN
C
C ---  RECUPERATION DES NOMS DES FONCTIONS REPRESENTANT LA FORCE
C ---  LINEIQUE A APPLIQUER SUR L'ELEMENT D'ARETE :
C      ------------------------------------------
        CALL JEVECH('PFF1D3D','L',IDFLIN)
C
C ---  RECUPERATION DE L'INSTANT D'INTERPOLATION :
C      -----------------------------------------
        CALL JEVECH('PTEMPSR','L',ITEMPS)
C
C ---  AFFECTATION DES VARIABLES POUR L'INTERPOLATION :
C      ----------------------------------------------
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
C
        VALPAR(4) = ZR(ITEMPS)
C
C ---  BOUCLE SUR LES POINTS D'INTEGRATION
C      -----------------------------------
         DO 70 IGAU = 1, NPG
C
            IDECPG = NNO*(IGAU-1)
C
            DO 80 I = 1, 3
               XYZGAU(I) = ZERO
               FORLIN(I) = ZERO
 80         CONTINUE
C
C ---    CALCUL DES COORDONNEES DU POINT D'INTEGRATION COURANT :
C        -----------------------------------------------------
          DO 90 INO = 1, NNO
C
             IDECNO = 3*(INO-1) - 1
C
             XYZGAU(1) = XYZGAU(1) + ZR(IVF+IDECPG+INO-1)*
     +                               ZR(IGEOM+1+IDECNO)
             XYZGAU(2) = XYZGAU(2) + ZR(IVF+IDECPG+INO-1)*
     +                               ZR(IGEOM+2+IDECNO)
             XYZGAU(3) = XYZGAU(3) + ZR(IVF+IDECPG+INO-1)*
     +                               ZR(IGEOM+3+IDECNO)
  90      CONTINUE
C
C ---    INTERPOLATION DES FORCES LINEIQUES EN FONCTION DES
C ---    COORDONNEES ET DU TEMPS :
C        -----------------------
            VALPAR(1) = XYZGAU(1)
            VALPAR(2) = XYZGAU(2)
            VALPAR(3) = XYZGAU(3)
C
         CALL FOINTE('FM',ZK8(IDFLIN+1-1),4,NOMPAR,VALPAR,FORLIN(1),IER)
         CALL FOINTE('FM',ZK8(IDFLIN+2-1),4,NOMPAR,VALPAR,FORLIN(2),IER)
         CALL FOINTE('FM',ZK8(IDFLIN+3-1),4,NOMPAR,VALPAR,FORLIN(3),IER)
C
C ---    CALCUL DU PRODUIT JACOBIEN*POIDS AU POINT D'INTEGRATION
C ---    COURANT :
C        -------
            CALL VFF3D(NNO, ZR(IPOIDS+IGAU-1), ZR(IDFDE+IDECPG),
     +                 ZR(IGEOM), JACOB)
C
C ---    CALCUL DE LA CONTRIBUTION AU VECTEUR DES FORCES NODALES
C ---    DU CHARGEMENT LINEIQUE AU POINT D'INTEGRATION COURANT :
C        -----------------------------------------------------
            DO 100 INO = 1, NNO
C
               FX(INO) = FX(INO) + ZR(IVF+IDECPG+INO-1)*FORLIN(1)
     +                             *JACOB
               FY(INO) = FY(INO) + ZR(IVF+IDECPG+INO-1)*FORLIN(2)
     +                             *JACOB
               FZ(INO) = FZ(INO) + ZR(IVF+IDECPG+INO-1)*FORLIN(3)
     +                             *JACOB
 100        CONTINUE
C
 70      CONTINUE
C
      ENDIF
C
C ---- RECUPERATION ET AFFECTATION DU VECTEUR FORCES NODALES EN SORTIE :
C      ---------------------------------------------------------------
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      DO 110 INO = 1, NNO
         ZR(IVECTU+3*(INO-1)+1-1) = FX(INO)
         ZR(IVECTU+3*(INO-1)+2-1) = FY(INO)
         ZR(IVECTU+3*(INO-1)+3-1) = FZ(INO)
 110  CONTINUE
C
      END
