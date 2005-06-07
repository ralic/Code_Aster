      SUBROUTINE  PK2CAU(NOMTE,NCMP,PK2,SIGMA)
C MODIF ELEMENTS  DATE 06/05/2003   AUTEUR CIBHHPD D.NUNEZ 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      PK2CAU  -- CALCUL DES CONTAINTES DE CAUCHY A PARTIR DES
C                 CONTRAINTES DE PIOLA-KIRCHHOFF DE SECONDE ESPECE
C                 A PARTIR DE LA FORMULE :
C
C            SIGMA = (1/DET[F])*([F]*[PK2]*[F]T)
C             OU [F] EST LA MATRICE DU GRADIENT DES DEFORMATIONS
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMTE          IN     K16      NOM DU TYPE D'ELEMENT
C    NCMP           IN     I        NOMBRE DE COMPOSANTES DU TENSEUR
C                                   DES CONTRAINTES
C    PK2(NCMP,1)    IN     R        TENSEUR DES CONTRAINTES
C                                   DE PIOLA-KIRCHHOFF DE SECONDE ESPECE
C    SIGMA(NCMP,1)  VAR    R        TENSEUR DES CONTRAINTES DE CAUCHY
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*16  NOMTE
           REAL*8        PK2(NCMP,1), SIGMA(NCMP,1)
           INTEGER       NCMP,JNBSPI
C -----  VARIABLES LOCALES
           PARAMETER (NBINCO=51)
           PARAMETER (NPGE=3)
C
           REAL*8        VECU(8,3), VECTHE(9,3), VECTA(9,2,3)
           REAL*8        VECTPT(9,2,3), VECTN(9,3), VECNPH(9,3)
           REAL*8        VECTG(2,3), VECTT(3,3), VECTTT(3,3), JM1(3,3)
           REAL*8        VECPE(NBINCO), BLAM(9,3,3), BID33(3,3)
           REAL*8        XAB(3,3), DUDX(3), DUDY(3), DUDZ(3)
           REAL*8        JDN1NC(9,NBINCO), DUDXNC(9), SIGMAG(3,3)
           REAL*8        FT(3,3), SIGMAT(3,3), PK2T(3,3),PK2G(3,3)
           REAL*8        KSI3S2
C
           LOGICAL       LGREEN
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      UN     = 1.0D0
      DEUX   = 2.0D0
C
      LGREEN = .FALSE.
C
C --- RECUPERATION DE LA CARTE DE COMPORTEMENT :
C     ----------------------------------------
      CALL JEVECH('PCOMPOR','L',ICOMPO)
C
        IF (ZK16(ICOMPO+2)(1:8).EQ.'GREEN_GR') THEN
          LGREEN = .TRUE.
        ENDIF
C
C --- RECUPERATION DU CHAMP DE DEPLACEMENT DANS LE CAS GREEN_GR :
C     ---------------------------------------------------------
      IF (LGREEN) THEN
        CALL TECACH('OON','PDEPLAR',1,IDEPL,IRET)
      ELSE
        DO 10 I = 1, 6
          DO 10 J = 1, 270
            SIGMA(I,J) = PK2(I,J)
  10    CONTINUE
C
        GOTO 9999
      ENDIF
C
C --- RECUPERATION DES COORDONNEES DES NOEUDS DANS LA GEOMETRIE
C --- INITIALE :
C     --------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C --- CARACTERISTIQUES DE COQUES :
C     --------------------------
      CALL JEVECH('PCACOQU','L',ICARA)
C ---   EPAISSEUR TOTALE :
      EPTOT = ZR(ICARA)
C ---   COORDONNEE MINIMALE SUIVANT L'EPAISSEUR
      ZMIN = -EPTOT/DEUX
C
C --- NOMBRE DE COUCHES :
C     -----------------
      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU=ZI(JNBSPI-1+1)
C
      IF (NBCOU.LE.0) THEN
        CALL UTMESS('F','PK2CAU','NOMBRE DE COUCHES OBLIGATOIREMENT '
     +            //'SUPERIEUR A 0 ')
      ENDIF
C
      IF (NBCOU.GT.10) THEN
        CALL UTMESS('F','PK2CAU','NOMBRE DE COUCHES LIMITE A 10 '
     +            //'POUR LES COQUES 3D ')
      ENDIF
C
C --- EPAISSEUR D'UNE COUCHE :
C     ----------------------
      EPAIS = EPTOT/NBCOU
C
C --- RECUPERATION DES OBJETS INITIALISES :
C     -----------------------------------
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESI',' ',LZI)
C
C --- NOMBRE DE NOEUDS (NB1 : SERENDIP, NB2 : LAGRANGE) :
C     -------------------------------------------------
      NB1 = ZI(LZI+1-1)
      NB2 = ZI(LZI+2-1)
C
C --- NOMBRE DE POINTS D'INTEGRATION DANS LE PLAN MOYEN
C --- (INTEGRATION NORMALE) :
C     ---------------------
      NPGSN = ZI(LZI+4-1)
C
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ',LZR)
C
      IF (ZR(LZR+1550-1).LE.SQRT(R8PREM())) THEN
        ZR(LZR+1550-1) = UN
      ENDIF
C
C --- AFFECTATION DES VECTEURS DE TRANSLATION ET DE ROTATION :
C     ------------------------------------------------------
      DO 20 IN = 1, NB1
        DO 30 II = 1, 3
          VECU(IN,II)   = ZR(IDEPL+6*(IN-1)+II-1)
          VECTHE(IN,II) = ZR(IDEPL+6*(IN-1)+II+3-1)
  30    CONTINUE
  20  CONTINUE
C
      DO 40 II = 1, 3
        VECTHE(NB2,II) = ZR(IDEPL+6*NB1+II-1)
  40  CONTINUE
C
C --- DETERMINATION DES REPERES LOCAUX AUX NOEUDS DANS LA
C --- CONFIGURATION INITIALE
C --- VECTA DESIGNE LES VECTEURS COVARIANTS DANS LE PLAN MOYEN A
C ---       CHAQUE NOEUD
C --- VECTN DESIGNE LES VECTEURS NORMAUX AU PLAN MOYEN
C --- VECTPT DESIGNE LES REPERES LOCAUX ORTHORNORMES EN CHAQUE
C --- NOEUD DANS LA CONFIGURATION INITIALE :
C     ------------------------------------
      CALL VECTAN(NB1,NB2,ZR(IGEOM),ZR(LZR),VECTA,VECTN,VECTPT)
C
C --- DETERMINATION AUX NOEUDS DES VECTEURS VECNPH QUI SONT LA
C --- TRANSFORMEE APRES DEFORMATION DES VECTEURS VECTN NORMAUX
C --- AU PLAN MOYEN INITIAL ET DES MATRICES DE ROTATION BLAM FAISANT
C --- PASSER DES VECTEURS VECTN AUX VECTEURS VECNPH :
C     ---------------------------------------------
      CALL VECTRN(NB2,VECTPT,VECTN,VECTHE,VECNPH,BLAM)
C
C --- DETERMINATION DU VECTEUR DE DEPLACEMENT AUX NOEUDS VECPE
C --- DEFINI PAR VECPE = <U V W (NPHI-N)_X (NPHI-N)_Y (NPHI-N)_Z>
C --- OU U, V, W SONT LES 3 DDLS DE TRANSLATION
C --- NPHI EST LE VECTEUR VECNPH ET N LE VECTEUR VECTN :
C     ------------------------------------------------
      CALL VECTPE(NB1,NB2,VECU,VECTN,VECNPH,VECPE)
C
C --- COMPTEUR DES POINTS D'INTEGRATION :
C     ---------------------------------
      KPGS = 0
C
C --- BOUCLE SUR LES COUCHES :
C     ----------------------
      DO 50 ICOU = 1, NBCOU
C
C ---   BOUCLE SUR LES POINTS D'INTEGRATION DANS L'EPAISSEUR :
C       ----------------------------------------------------
        DO 60 INTE = 1, NPGE
C
C ---      POSITION DANS L'EPAISSEUR :
           IF (INTE.EQ.1) THEN
              ZIC = ZMIN + (ICOU-1)*EPAIS
           ELSEIF (INTE.EQ.2) THEN
              ZIC = ZMIN + EPAIS/DEUX + (ICOU-1)*EPAIS
           ELSEIF (INTE.EQ.3) THEN
              ZIC = ZMIN + EPAIS + (ICOU-1)*EPAIS
           ENDIF
C ---      COORDONNEE ISOPARAMETRIQUE DANS L'EPAISSEUR DIVISEE PAR 2
           KSI3S2 = ZIC/EPAIS
C
C ---      BOUCLE SUR LES POINTS D'INTEGRATION DANS LE PLAN MOYEN :
C          ------------------------------------------------------
           DO 70 INTSN = 1, NPGSN
C
             KPGS = KPGS + 1
C
C ---        DETERMINATION DES REPERES LOCAUX AUX POINTS D'INTEGRATION
C ---        DANS LA CONFIGURATION INITIALE
C ---        VECTG DESIGNE LES VECTEURS COVARIANTS DANS LE PLAN MOYEN
C ---              EN CHAQUE POINT D'INTEGRATION
C ---        VECTT DESIGNE LES REPERES LOCAUX ORTHORNORMES EN CHAQUE
C ---        POINT D'INTEGRATION DANS LA CONFIGURATION INITIALE :
C            --------------------------------------------------
             CALL VECTGT(1,NB1,ZR(IGEOM),KSI3S2,INTSN,ZR(LZR),EPAIS,
     +                   VECTN,VECTG,VECTT)
C
C ---        CALCUL DE L'INVERSE DE LA MATRICE JACOBIENNE JM1:
C            ------------------------------------------------
             CALL JACBM1(EPAIS,VECTG,VECTT,BID33,JM1,DETJ)
C
C ---        CALCUL DU VECTEUR JDN1NC QUI EST < DU/DQSI> (I.E.
C ---        <DU/DQSI1,DU/DQSI2,DU/DQSI3,DV/DQSI1,DV/DQSI2,DV/DQSI3,
C ---         DW/DQSI1,DW/DQSI2,DW/DQSI3> ) :
C             -----------------------------
              CALL JM1DN1(1,1,NB1,NB2,ZR(LZR),EPAIS,KSI3S2,INTSN,JM1,
     +                    JDN1NC)
C
C ---        CALCUL DU VECTEUR DUDXNC QUI EST < DU/DX> (I.E.
C ---        <DU/DX,DU/DY,DU/DZ,DV/DX,DV/DY,DV/DZ,DW/DX,DW/DY,DW/DZ> ) :
C             ------------------------------------------------------
              CALL PROMAT(JDN1NC,9,9,6*NB1+3,
     +                    VECPE,6*NB1+3,6*NB1+3,1,
     +                    DUDXNC)
C
              DO 80 I = 1, 3
                DUDX(I) = DUDXNC(1+3*(I-1))
                DUDY(I) = DUDXNC(2+3*(I-1))
                DUDZ(I) = DUDXNC(3+3*(I-1))
  80          CONTINUE
C
C ---         CONSTRUCTION DE LA MATRICE [F] DU GRADIENT DES
C ---         DEFORMATIONS AU POINT D'INTEGRATION COURANT.
C ---         PAR DEFINITION :
C ---                 | 1 0 0 |   | DU/DX DU/DY DU/DZ |
C ---           [F] = | 0 1 0 | + | DV/DX DV/DY DV/DZ |
C ---                 | 0 0 1 |   | DW/DX DW/DY DW/DZ |
C ---         PAR COMMODITE, ON UTILISE PLUTOT [FT] , LA MATRICE
C ---         TRANSPOSEE DE [F] :
C             -----------------
              DO 90 I = 1, 3
                FT(1,I) = DUDX(I)
                FT(2,I) = DUDY(I)
                FT(3,I) = DUDZ(I)
  90          CONTINUE
C
              FT(1,1) = FT(1,1) + UN
              FT(2,2) = FT(2,2) + UN
              FT(3,3) = FT(3,3) + UN
C
C ---         CONSTRUCTION DU TENSEUR DES CONTRAINTES PK2T A
C ---         PARTIR DU VECTEUR PK2 DES COMPOSANTES DE CE TENSEUR :
C             ---------------------------------------------------
              PK2T(1,1) = PK2(1,KPGS)
              PK2T(2,2) = PK2(2,KPGS)
              PK2T(3,3) = PK2(3,KPGS)
              PK2T(1,2) = PK2(4,KPGS)
              PK2T(1,3) = PK2(5,KPGS)
              PK2T(2,3) = PK2(6,KPGS)
              PK2T(2,1) = PK2T(1,2)
              PK2T(3,1) = PK2T(1,3)
              PK2T(3,2) = PK2T(2,3)
C
C ---         PASSAGE DU PK2 DU REPERE LOCAL A L'ELEMENT AU
C ---         REPERE GLOBAL :
C             -------------
              CALL BTKB (3,3,3,PK2T,VECTT,BID33,PK2G)
C
C ---         CALCUL DU TENSEUR DE CAUCHY :
C             ===========================
C ---         D'ABORD CALCUL DE [SIGMAG] = [F]*[PK2]*[FT] :
C             -------------------------------------------
              CALL UTBTAB('ZERO',3,3,PK2G,FT,XAB,SIGMAG)
C
C ---         MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL :
C             --------------------------------------------------
              DO 100 I = 1, 3
              DO 100 J = 1, 3
                 VECTTT(I,J) = VECTT(J,I)
 100          CONTINUE
C
C ---         PASSAGE DU TENSEUR DES CONTRAINTES DE CAUCHY DU
C ---         REPERE GLOBAL AU REPERE LOCAL :
C             -----------------------------
              CALL BTKB (3,3,3,SIGMAG,VECTTT,BID33,SIGMAT)
C
C ---         CALCUL DU DETERMINANT DE [F] ( = DET [FT] ) :
C             -------------------------------------------
              COF11 = FT(2,2)*FT(3,3) - FT(2,3)*FT(3,2)
              COF21 = FT(3,1)*FT(2,3) - FT(2,1)*FT(3,3)
              COF31 = FT(2,1)*FT(3,2) - FT(3,1)*FT(2,2)
C
              DETF = FT(1,1)*COF11 + FT(1,2)*COF21 + FT(1,3)*COF31
              DETFM1 = UN/(DETF+R8PREM())
C
C ---         AFFECTATION DU VECTEUR DES COMPOSANTES DU TENSEUR
C ---         DE CAUCHY :
C             ---------
              SIGMA(1,KPGS) = SIGMAT(1,1)*DETFM1
              SIGMA(2,KPGS) = SIGMAT(2,2)*DETFM1
              SIGMA(3,KPGS) = SIGMAT(3,3)*DETFM1
              SIGMA(4,KPGS) = SIGMAT(1,2)*DETFM1
              SIGMA(5,KPGS) = SIGMAT(1,3)*DETFM1
              SIGMA(6,KPGS) = SIGMAT(2,3)*DETFM1

C
  70       CONTINUE
  60    CONTINUE
  50  CONTINUE
C
 9999 CONTINUE
C.============================ FIN DE LA ROUTINE ======================
      END
