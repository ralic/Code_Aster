      SUBROUTINE DSTMAS ( XYZL, OPTION, PGL, MAS, ENER )
      IMPLICIT NONE
      REAL*8        XYZL(3,*), PGL(*), MAS(*), ENER(*)
      CHARACTER*16  OPTION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2005   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     MATRICE MASSE DE L'ELEMENT DE PLAQUE DST
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM_DEPL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
C     OUT MAS    : MATRICE DE RIGIDITE
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
      INTEGER  I, J, K, I1,I2,J1,J2, INT, P, MULTIC, JCOQU, JDEPG
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      REAL*8   NFX(9),NFY(9),NMX(6),NMY(6),NMI(3)
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8   HFT2(2,6), BCA(2,3),AN(3,9),AM(3,6)
      REAL*8   FLEX(9,9), MEMB(6,6), MEFL(6,9)
      REAL*8   WST(9), WMEST(6), DEPL(18)
      REAL*8   MASLOC(171), MASGLO(171)
      REAL*8   RHO,EPAIS,ROE,ROF,CTOR,EXCENT,DETJ,WGT
      REAL*8   R8GAEM,ZERO,UN,SIX,DOUZE,WGTF,WGTM,WGTMF
      REAL*8   QSI, ETA, CARAT3(21), T2EV(4), T2VE(4), T1VE(9)
      LOGICAL  ELASCO, EXCE
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
C
      ZERO  =  0.0D0
      UN    =  1.0D0
      SIX   =  6.0D0
      DOUZE = 12.0D0
C
      EXCE   = .FALSE.
      EXCENT =  ZERO
C
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
      CALL R8INIR(18,ZERO,AM,1)
C
      CALL DXROEP(RHO,EPAIS)
      ROE = RHO*EPAIS
      ROF = RHO*EPAIS*EPAIS*EPAIS/DOUZE

      CALL JEVECH('PCACOQU','L',JCOQU)
      CTOR   = ZR(JCOQU+3)
      EXCENT = ZR(JCOQU+4)
      IF (ABS(EXCENT).GT.UN/R8GAEM()) EXCE = .TRUE.

C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
      CALL GTRIA3 ( XYZL, CARAT3 )

C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEE --------------------------
      CALL DXMATE(DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,.FALSE.,
     +                                         ELASCO,T2EV,T2VE,T1VE)

C     -------- CALCUL DU PRODUIT HF.T2 ---------------------------------
      CALL DSXHFT ( DF, CARAT3(9), HFT2 )

      IF (EXCE) THEN
C ---   CALCUL DES MATRICES BCA ,AN ET AM DANS LE CAS DE L'EXCENTREMENT:
C       ---------------------------------------------------------------
        CALL DSTCI2 ( DCI, CARAT3, HFT2, DFC, DMC, BCA, AN, AM )
      ELSE
C ---   CALCUL DES MATRICES BCA ET AN DANS LE CAS NON EXCENTRE :
C       ------------------------------------------------------
        CALL DSTCIS ( DCI , CARAT3 , HFT2 , BCA , AN )
      ENDIF
C
      DETJ = CARAT3(7)
C
C===========================================================
C ---  CALCUL DE LA PARTIE MEMBRANE DE LA MATRICE DE MASSE =
C===========================================================
C
C --- PRISE EN COMPTE DES TERMES DE MEMBRANE CLASSIQUES
C --- EN U*U ET V*V :
C     -------------
      MEMB(1,1) = CARAT3(8)*ROE/SIX
      MEMB(1,3) = CARAT3(8)*ROE/DOUZE
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
C
      DO 40 INT = 1,NPG
        QSI = ZR(ICOOPG-1+NDIM*(INT-1)+1)
        ETA = ZR(ICOOPG-1+NDIM*(INT-1)+2)
C
C ---   CALCUL DES FONCTIONS D'INTERPOLATION DE LA FLECHE :
C       -------------------------------------------------
        CALL DSTNIW ( QSI, ETA, CARAT3, DCI, BCA, AN, AM, WST, WMEST )
C
C ---   CALCUL DES FONCTIONS D'INTERPOLATION DES ROTATIONS :
C       --------------------------------------------------
        CALL DSTNIB ( QSI, ETA, CARAT3, AN, AM, NFX, NFY, NMX, NMY )
C
C ---   CALCUL DES FONCTIONS DE FORME DE MEMBRANE :
C       -----------------------------------------
        CALL DXTNIM ( QSI, ETA, NMI )
C
C ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION W
C ---   EST EGALE A RHO_F = RHO*EPAIS :
C       -----------------------------
        WGT = ZR(IPOIDS+INT-1)*DETJ*ROE
C
C==========================================================
C ---  CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE =
C==========================================================
C
C ---   CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE
C ---   DUE AUX SEULS TERMES DE LA FLECHE W :
C       -----------------------------------
        DO 50 I = 1,9
          DO 60 J = 1,9
            FLEX(I,J) = FLEX(I,J) + WST(I)*WST(J)*WGT
   60     CONTINUE
   50   CONTINUE
C
C ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION BETA
C ---   EST EGALE A RHO_F = RHO*EPAIS**3/12 + D**2*EPAIS*RHO :
C       ----------------------------------------------------
          WGTF = ZR(IPOIDS+INT-1)*DETJ*(ROF+EXCENT*EXCENT*ROE)
C
C ---   PRISE EN COMPTE DES TERMES DE FLEXION DUS AUX ROTATIONS :
C       -------------------------------------------------------
          DO 70 I = 1, 9
            DO 80 J = 1, 9
              FLEX(I,J) = FLEX(I,J)+(NFX(I)*NFX(J)+NFY(I)*NFY(J))*WGTF
   80     CONTINUE
   70   CONTINUE
C==============================================================
C ---   CAS D'UN ELEMENT EXCENTRE : IL APPARAIT DE TERMES DE  =
C ---   COUPLAGE MEMBRANE-FLEXION ET DE NOUVEAUX TERMES POUR  =
C ---   PARTIE MEMBRANE DE LA MATRICE DE MASSE :              =
C==============================================================
C
        IF (EXCE) THEN
C
C===================================================================
C ---  CALCUL DE LA PARTIE MEMBRANE-FLEXION DE LA MATRICE DE MASSE =
C===================================================================
C
C ---     POUR LE COUPLAGE MEMBRANE-FLEXION, ON DOIT TENIR COMPTE
C ---     DE 3 MASSES VOLUMIQUES
C ---     RHO_M  = EPAIS*RHO 
C ---     RHO_MF = D*EPAIS*RHO 
C ---     RHO_F  = RHO*EPAIS**3/12 + D**2*EPAIS*RHO
C         -------------------------------------------------
          WGTM  = ZR(IPOIDS+INT-1)*DETJ*ROE
          WGTMF = ZR(IPOIDS+INT-1)*DETJ*EXCENT*ROE
          WGTF  = ZR(IPOIDS+INT-1)*DETJ*(ROF+EXCENT*EXCENT*ROE)
C
C ---     PRISE EN COMPTE DES TERMES DE COUPLAGE MEMBRANE-FLEXION 
C ---     ON A 3 TYPES DE TERMES DONT IL FAUT TENIR COMPTE
C ---     1) LES TERMES U*BETA     -> NMI*NFX ET NMI*NFY (RHO_MF)
C ---     2) LES TERMES W*W        -> WMEST*WST          (RHO_M)
C ---     3) LES TERMES BETA*BETA  -> NMX*NFX            (RHO_F)
C         ------------------------------------------------------
C
C ---      1) TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA :
C             ------------------------------------------
          DO 90 K = 1, 3
            DO 100 J = 1, 9
                I1 = 2*(K-1)+1
                I2 = I1     +1
                MEFL(I1,J) = MEFL(I1,J)+NMI(K)*NFX(J)*WGTMF
                MEFL(I2,J) = MEFL(I2,J)+NMI(K)*NFY(J)*WGTMF
  100       CONTINUE
   90     CONTINUE
C ---      2) TERMES DE COUPLAGE MEMBRANE-FLEXION W*W ET BETA*BETA :
C             ----------------------------------------------------
            DO 110 I = 1, 6
            DO 120 J = 1, 9
                MEFL(I,J) = MEFL(I,J) + WMEST(I)*WST(J)*WGTM
     +                + (NMX(I)*NFX(J) + NMY(I)*NFY(J))*WGTF
  120       CONTINUE
  110     CONTINUE
C
C===========================================================
C ---  AJOUT DE NOUVEAUX TERMES A LA PARTIE MEMBRANE       =
C ---  DE LA MATRICE DE MASSE DUS A L'EXCENTREMENT         =
C===========================================================
C
C ---     PRISE EN COMPTE DES TERMES DE MEMBRANE
C ---     ON A 3 TYPES DE TERMES DONT IL FAUT TENIR COMPTE
C ---     1) LES TERMES U*BETA     -> NMI*NMX ET NMI*NMY (RHO_MF)
C ---     2) LES TERMES W*W        -> WMEST*WMEST        (RHO_M)
C ---     3) LES TERMES BETA*BETA  -> NMX*NMX            (RHO_F)
C         ------------------------------------------------------
C
C ---      1) TERMES DE MEMBRANE U*BETA :
C             -------------------------
          DO 130 K = 1, 3
            I1 = 2*(K-1)+1
            I2 = I1     +1
            DO 140 P = 1, 3
              J1 = 2*(P-1)+1
              J2 = J1     +1
              MEMB(I1,J1) = MEMB(I1,J1)+
     +                      (NMI(K)*NMX(J1)+NMI(P)*NMX(I1))*WGTMF
              MEMB(I1,J2) = MEMB(I1,J2)+
     +                      (NMI(K)*NMX(J2)+NMI(P)*NMY(I1))*WGTMF
              MEMB(I2,J1) = MEMB(I2,J1)+
     +                      (NMI(K)*NMY(J1)+NMI(P)*NMX(I2))*WGTMF
              MEMB(I2,J2) = MEMB(I2,J2)+
     +                      (NMI(K)*NMY(J2)+NMI(P)*NMY(I2))*WGTMF
  140       CONTINUE
  130     CONTINUE
C ---      2) TERMES DE MEMBRANE WMEST*WMEST ET BETA*BETA :
C             -------------------------------------------
          DO 150 I = 1, 6
            DO 160 J = 1, 6
                MEMB(I,J) = MEMB(I,J) + WMEST(I)*WMEST(J)*WGTM
     +                + (NMX(I)*NMX(J) + NMY(I)*NMY(J))*WGTF
  160       CONTINUE
  150     CONTINUE
C
        ENDIF
C ---   FIN DU TRAITEMENT DU CAS D'UN ELEMENT EXCENTRE
C       ----------------------------------------------
   40 CONTINUE
C --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C     ---------------------------------------------
C
C --- INSERTION DES DIFFERENTES PARTIES CALCULEES DE LA MATRICE
C --- DE MASSE A LA MATRICE ELLE MEME :
C     ===============================   
      IF (( OPTION .EQ. 'MASS_MECA' ).OR.(OPTION.EQ.'M_GAMMA')) THEN
        CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,MAS)

      ELSE IF (OPTION.EQ.'MASS_MECA_DIAG') THEN
        CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,MASLOC)
        WGT = CARAT3(8)*ROE
        CALL UTPSLG(3,6,PGL,MASLOC,MASGLO)
        CALL DIALUM(3,6,18,WGT,MASGLO,MAS)

      ELSE IF (OPTION.EQ.'ECIN_ELEM_DEPL') THEN
        CALL JEVECH('PDEPLAR','L',JDEPG)
        CALL UTPVGL(3,6,PGL,ZR(JDEPG),DEPL)
        CALL DXTLOE(FLEX,MEMB,MEFL,CTOR,0,DEPL,ENER)
      END IF
C
      END
