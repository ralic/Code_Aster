      SUBROUTINE DKTMAS(NOMTE,XYZL,OPTION,PGL,MAS,ENER,MULTIC,GRILLE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2004   AUTEUR CIBHHLV L.VIVAN 
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
      IMPLICIT NONE
      REAL*8        XYZL(3,*), PGL(*), MAS(*), ENER(*)
      CHARACTER*16  OPTION , NOMTE
      LOGICAL       GRILLE
C     ------------------------------------------------------------------
C     MATRICE MASSE DE L'ELEMENT DE PLAQUE DKT
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM_DEPL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
C     OUT MAS    : MATRICE DE RIGIDITE
C     IN  GRILLE : .TRUE. => ELEMENT DE GRILLE (MEGRDKT)
C          3 POUR UN MATERIAU ORTHOTROPE (MEGRDKT / MEGRDKQ)
C     OUT ENER   : TERMES POUR ENER_CIN (ECIN_ELEM_DEPL)
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      INTEGER  I, J, K, I1, I2, LZR, INT, MULTIC, JCOQU, JDEPG
      REAL*8   DETJ,WGT,WKT(9),DEPL(18),NFX(9),NFY(9),NMI(3)
      REAL*8   FLEX(9,9),MEMB(6,6),MEFL(6,9),MASLOC(171),MASGLO(171)
      REAL*8   RHO,EPAIS,ROE,ROF,CTOR,EXCENT,XINERT
      REAL*8   R8GAEM,ZERO,UN,SIX,DOUZE,WGTF, WGTMF
      LOGICAL      EXCE, INER
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER NPG,NC,NNO
      INTEGER LDETJ,LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
      INTEGER LAIRE
      PARAMETER (NPG=3)
      PARAMETER (NNO=3)
      PARAMETER (NC=3)
      PARAMETER (LDETJ=1)
      PARAMETER (LJACO=2)
      PARAMETER (LTOR=LJACO+4)
      PARAMETER (LQSI=LTOR+1)
      PARAMETER (LETA=LQSI+NPG+NNO)
      PARAMETER (LWGT=LETA+NPG+NNO)
      PARAMETER (LXYC=LWGT+NPG)
      PARAMETER (LCOTE=LXYC+2*NC)
      PARAMETER (LCOS=LCOTE+NC)
      PARAMETER (LSIN=LCOS+NC)
      PARAMETER (LAIRE=LSIN+NC)
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ',LZR)
C
      ZERO  =  0.0D0
      UN    =  1.0D0
      SIX   =  6.0D0
      DOUZE = 12.0D0
C
      CALL DXROEP(RHO,EPAIS)
      ROE = RHO*EPAIS
      ROF = RHO*EPAIS*EPAIS*EPAIS/DOUZE
C
      CALL JEVECH('PCACOQU','L',JCOQU)
      CTOR = ZR(JCOQU+3)
      EXCENT = ZERO
C
      IF ( GRILLE ) THEN
         CTOR = ZR(JCOQU+7)
      ELSE
         EXCENT = ZR(JCOQU+4)
         XINERT = ZR(JCOQU+5)
      ENDIF
C
      EXCE = .FALSE.
      INER = .FALSE.
      IF (ABS(EXCENT).GT.UN/R8GAEM()) EXCE = .TRUE.
      IF (ABS(XINERT).GT.UN/R8GAEM()) INER = .TRUE.
      IF ( .NOT. INER )  ROF = 0.0D0
C
C --- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE :
C     -------------------------------------------------
      CALL GTRIA3(XYZL,ZR(LZR))
C
C --- INITIALISATIONS :
C     ---------------
      DO 10 K = 1,54
        MEFL(K,1) = ZERO
   10 CONTINUE
      DO 20 K = 1,81
        FLEX(K,1) = ZERO
   20 CONTINUE
      DO 30 K = 1,36
        MEMB(K,1) = ZERO
   30 CONTINUE
C
      DETJ = ZR(LZR-1+LDETJ)
C
C======================================
C ---  CALCUL DE LA MATRICE DE MASSE  =
C======================================
C=====================================================================
C ---  CALCUL DE LA PARTIE MEMBRANE CLASSIQUE DE LA MATRICE DE MASSE =
C ---  LES TERMES SONT EN NK*NP                                      =
C=====================================================================
C
      MEMB(1,1) = ZR(LZR-1+LAIRE)*ROE/SIX
      MEMB(1,3) = ZR(LZR-1+LAIRE)*ROE/DOUZE
      MEMB(1,5) = MEMB(1,3)
      MEMB(2,2) = MEMB(1,1)
      MEMB(2,4) = MEMB(1,3)
      MEMB(2,6) = MEMB(1,3)
      MEMB(3,1) = MEMB(1,3)
      MEMB(3,3) = MEMB(1,1)
      MEMB(3,5) = MEMB(1,3)
      MEMB(4,2) = MEMB(1,3)
      MEMB(4,4) = MEMB(1,1)
      MEMB(4,6) = MEMB(1,3)
      MEMB(5,1) = MEMB(1,3)
      MEMB(5,3) = MEMB(1,3)
      MEMB(5,5) = MEMB(1,1)
      MEMB(6,2) = MEMB(1,3)
      MEMB(6,4) = MEMB(1,3)
      MEMB(6,6) = MEMB(1,1)
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     ===================================
      DO 40 INT = 1,NPG
C
C===========================================================
C ---  CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE  =
C===========================================================
C
C ---   CALCUL DES FONCTIONS D'INTERPOLATION DE LA FLECHE :
C       -------------------------------------------------
        CALL DKTNIW(INT,ZR(LZR),WKT)
C
C ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION W
C ---   EST EGALE A RHO_E = RHO*EPAIS :
C       -----------------------------
        WGT = ZR(LZR-1+LWGT+INT-1)*DETJ*ROE
C
C ---   CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE
C ---   DUE AUX SEULS TERMES DE LA FLECHE W :
C       -----------------------------------
        DO 50 I = 1,9
          DO 60 J = 1,9
            FLEX(I,J) = FLEX(I,J) + WKT(I)*WKT(J)*WGT
   60     CONTINUE
   50   CONTINUE
C
C ---   CALCUL DES FONCTIONS D'INTERPOLATION DES ROTATIONS :
C       --------------------------------------------------
        CALL DKTNIB(INT,ZR(LZR),NFX,NFY)
C
C ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION BETA
C ---   EST EGALE A RHO_F = RHO*EPAIS**3/12 + D**2*EPAIS*RHO :
C       ----------------------------------------------------
        WGTF = ZR(LZR-1+LWGT+INT-1)*DETJ*(ROF+EXCENT*EXCENT*ROE)
C
C ---   PRISE EN COMPTE DES TERMES DE FLEXION DUS AUX ROTATIONS :
C       -------------------------------------------------------
        DO 70 I = 1,9
          DO 80 J = 1,9
            FLEX(I,J) = FLEX(I,J)+(NFX(I)*NFX(J)+NFY(I)*NFY(J))*WGTF
   80     CONTINUE
   70   CONTINUE
C
C====================================================================
C ---  CAS OU L'ELEMENT EST EXCENTRE                                =
C ---  CALCUL DE LA PARTIE MEMBRANE-FLEXION DE LA MATRICE DE MASSE  =
C====================================================================
C
        IF (EXCE) THEN
C
C ---     FONCTIONS D'INTERPOLATION MEMBRANE :
C         ----------------------------------
          CALL DXTNIM(INT,ZR(LZR),NMI)
C
C ---     POUR LE COUPLAGE MEMBRANE-FLEXION, ON DOIT TENIR COMPTE
C ---     DE LA MASSE VOLUMIQUE
C ---     RHO_MF = D*EPAIS*RHO 
C         --------------------
          WGTMF = ZR(LZR-1+LWGT+INT-1)*DETJ*EXCENT*ROE
C
C ---     TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA :
C         ------------------------------------------
          DO 90 K = 1, 3
            DO 100 J = 1,9
              I1 = 2*(K-1)+1
              I2 = I1     +1
              MEFL(I1,J) = MEFL(I1,J)+NMI(K)*NFX(J)*WGTMF
              MEFL(I2,J) = MEFL(I2,J)+NMI(K)*NFY(J)*WGTMF
  100       CONTINUE
   90     CONTINUE
        ENDIF
C
C ---   FIN DU TRAITEMENT DU CAS D'UN ELEMENT EXCENTRE
C       ----------------------------------------------
   40 CONTINUE
C --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C     ---------------------------------------------
C
C
C --- INSERTION DES DIFFERENTES PARTIES CALCULEES DE LA MATRICE
C --- DE MASSE A LA MATRICE ELLE MEME :
C     ===============================   
      IF (( OPTION .EQ. 'MASS_MECA' ).OR.(OPTION.EQ.'M_GAMMA')) THEN
        CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,MAS)
      ELSE IF (OPTION.EQ.'MASS_MECA_DIAG') THEN
        CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,MASLOC)
        WGT = ZR(LZR-1+LAIRE)*ROE
        CALL UTPSLG(3,6,PGL,MASLOC,MASGLO)
        CALL DIALUM(3,6,18,WGT,MASGLO,MAS)
      ELSE IF (OPTION.EQ.'ECIN_ELEM_DEPL') THEN
        CALL JEVECH('PDEPLAR','L',JDEPG)
        CALL UTPVGL(3,6,PGL,ZR(JDEPG),DEPL)
        CALL DXTLOE(FLEX,MEMB,MEFL,CTOR,0,DEPL,ENER)
      END IF
      CALL JEDEMA()
      END
