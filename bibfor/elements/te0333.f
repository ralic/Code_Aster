      SUBROUTINE TE0333 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
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
C
C     BUT: CALCUL DES DEFORMATIONS PLASTIQUES AUX NOEUDS ET PG ET DES
C          DEFORMATIONS DE FLUAGE
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C     IN   OPTION : OPTIONS DE CALCUL
C                   'EPSP_ELNO'   'EPSP_ELGA'
C                   'EPGR_ELNO'   'EPGR_ELGA'
C          NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      INTEGER            NBPG(10), MXCMEL, NBRES, NBSGM, MXCMPG, I,
     +                   JIN, NDIM, NNO, NBFPG, NBSIGM, NBSIG, IDSIG,
     +                   NNOS, NPG, JVAL, IPOIDS, IVF, IDFDE, IDFDN,
     +                   IDFDK, IGAU, ISIG, INO, IGEOM, IDEPL,
     +                   ITEMPE, ITREF, ITEMPS, IMATE, IDEFA, IDECPG,
     +                   IDEFP, IHYDRE, ISECHE, ICOMPO, NBVARI, IVARI,
     +                   K, NVI, NVIF, IBID, JTAB(7)
      PARAMETER          (MXCMEL = 162)
      PARAMETER          (NBRES  =   3)
      PARAMETER          (NBSGM  =   6)
      PARAMETER          (MXCMPG =  27)
      REAL*8             VALRES(NBRES)
      REAL*8             EPSM(MXCMEL), EPSANE(MXCMEL), EPSPLA(MXCMEL)
      REAL*8             EPSPLN(MXCMEL), EPSFLU(MXCMEL), SIGMA(NBSGM)
      REAL*8             VALPAR(4), C1, C2, TRSIG, HYDRG, SECHG
      REAL*8             HYDR(MXCMPG), SECH(MXCMPG)
      REAL*8             REPERE(7), NHARM, E, NU, ZERO, UN, TEMPG
      REAL*8             EPSFL(NBSGM), EPSFLF(NBSGM), TMPDMX, TMPFMX
      CHARACTER*2        CODRET(NBRES)
      CHARACTER*8        NOMRES(NBRES)
      CHARACTER*8         ELREFE, NOMPAR(4), MODELI, MOD3D
      CHARACTER*16       OPTIO2, PHENOM, CMP1, CMP2, CMP3
      CHARACTER*24       CHVAL, CHCTE
      LOGICAL            LFLU, LPLAS, LTEMP
C DEB ------------------------------------------------------------------
C
C --- INITIALISATIONS :
C     ---------------
      CALL ELREF1(ELREFE)
      ZERO        = 0.0D0
      UN          = 1.0D0
      NHARM       = ZERO
      MODELI(1:2) = NOMTE(3:4)
      MOD3D       = '3D'
C
      DO 10 I = 1, MXCMEL
         EPSANE(I) = ZERO
         EPSPLN(I) = ZERO
  10  CONTINUE
      DO 20 I = 1, MXCMPG
         HYDR(I)   = ZERO
         SECH(I)   = ZERO
  20  CONTINUE
C
C --- CARACTERISTIQUES DU TYPE D'ELEMENT :
C --- GEOMETRIE ET INTEGRATION
C     ------------------------
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      NNOS  = ZI(JIN+3+NBFPG+1-1)
      DO 30 I = 1,NBFPG
        NBPG(I) = ZI(JIN+3+I-1)
 30   CONTINUE
C
      IF (OPTION(6:9).EQ.'ELNO') THEN
C
CJMP CORRECTION AL 2000-240
C          IF(ELREFE.EQ.'TETRA10'.OR.ELREFE.EQ.'HEXA20' ) THEN
C              NPG = NBPG(3)
C          ELSE IF(ELREFE.EQ.'PENTA15' ) THEN
CC              NPG = NBPG(2)
C          ELSE
C              NPG = NBPG(1)
C          ENDIF
         NPG = NBPG(1)
         IF (ELREFE.EQ.'PENTA15') THEN
             NPG = NBPG(2)
         END IF
      ELSE
          NPG = NBPG(1)
      ENDIF
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
C      IF(ELREFE.EQ.'TETRA10'.OR.ELREFE.EQ.'HEXA20' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C     +                  + NBPG(2)*(1+(NDIM+1)*NNO)
C       ELSE IF(ELREFE.EQ.'PENTA15' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C      ENDIF
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
C
C --- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
C      -----------------------------------------
      NBSIG = NBSIGM(MODELI)
C
C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C     ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C --- RECUPERATION DU MATERIAU :
C     ------------------------
      CALL JEVECH('PMATERC','L',IMATE)
C
C --- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
C     ------------------------------------------------------------
      CALL ORTREP(ZI(IMATE),NDIM,REPERE)
C
C --- RECUPERATION DES TEMPERATURES AUX NOEUDS DE L'ELEMENT :
C     -----------------------------------------------------
      CALL JEVECH('PTEMPER','L',ITEMPE)
C
C --- RECUPERATION DE LA TEMPERATURE DE REFERENCE :
C     -------------------------------------------
      CALL JEVECH('PTEREF','L',ITREF)
C
C --- RECUPERATION DE L'HYDRATATION AUX POINTS DE GAUSS DE L'ELEMENT :
C     -----------------------------------------------------
      CALL TECACH(.FALSE.,.FALSE.,'PHYDRER',1,IHYDRE)
      IF(IHYDRE.NE.0) THEN
      DO 40 I = 1, NPG
         HYDR(I)   = ZR(IHYDRE+I-1)
  40  CONTINUE
      ELSE
      ENDIF
C
C --- RECUPERATION DU SECHAGE AUX NOEUDS DE L'ELEMENT :
C     -----------------------------------------------------
      CALL TECACH(.FALSE.,.FALSE.,'PSECHER',1,ISECHE)
      IF(ISECHE.NE.0) THEN
      DO 50 I = 1, NNO
         SECH(I)   = ZR(ISECHE+I-1)
  50  CONTINUE
      ELSE
      ENDIF
C
C --- RECUPERATION DE L'INSTANT COURANT :
C     ---------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)
C
      IF (OPTION(1:4).EQ.'EPSP') THEN
         LPLAS = .TRUE.
C
C
C ---    RECUPERATION DU CHAMP DE DEPLACEMENTS AUX NOEUDS  :
C        ------------------------------------------------
         CALL JEVECH('PDEPLAR','L',IDEPL)
C
C ---    RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION :
C        -------------------------------------------------------------
         CALL JEVECH('PCONTRR','L',IDSIG)
C
C ---    RECUPERATION DES DEFORMATIONS ANELASTIQUES AUX NOEUDS
C ---    DE L'ELEMENT :
C        ------------
         CALL TECACH(.TRUE.,.FALSE.,'PDEFAPR',1,IDEFA)
C
C ---    ON VERIFIE QUE LE MATERIAU EST ISOTROPE
C ---    (POUR L'INSTANT PAS D'ORTHOTROPIE NI D'ISOTROPIE TRANSVERSE
C ---    EN PLASTICITE) :
C        --------------
         CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
C
         IF (PHENOM.EQ.'ELAS_ORTH'.OR.PHENOM.EQ.'ELAS_ISTR'.OR.
     +      PHENOM.EQ.'ELAS_ORTH_FO'.OR.PHENOM.EQ.'ELAS_ISTR_FO') THEN
           CALL UTMESS('F','TE0333','LE MATERIAU '//PHENOM(1:12)//
     +                ' N''EST PAS AUTORISE POUR CALCULER LES '
     +              //'DEFORMATIONS PLASTIQUES : SEULS LES '//
     +                'MATERIAUX ISOTROPES SONT TRAITES EN PLASTICITE.')
         ENDIF
C
C ---    CALCUL DES DEFORMATIONS HORS THERMIQUES CORRESPONDANTES AU
C ---    CHAMP DE DEPLACEMENT I.E. EPSM = EPST - EPSTH
C ---    OU EPST  SONT LES DEFORMATIONS TOTALES
C ---       EPST = B.U
C ---    ET EPSTH SONT LES DEFORMATIONS THERMIQUES
C ---       EPSTH = ALPHA*(T-TREF) :
C           ----------------------
         OPTIO2 = 'EPMH_' // OPTION(6:9) // '_DEPL'
         CALL EPSVMC(MODELI, NNO, NDIM, NBSIG, NPG, ZR(IVF),
     +               ZR(IDFDE), ZR(IDFDN), ZR(IDFDK), ZR(IPOIDS),
     +               ZR(IGEOM), ZR(IDEPL), ZR(ITEMPE), ZR(ITREF),
     +               HYDR, SECH, ZR(ITEMPS), ZI(IMATE),
     +               REPERE, NHARM, OPTIO2, EPSM)
C
C ---    AFFECTATION DU VECTEUR DES DEFORMATIONS ANELASTIQUES AUX
C ---    POINTS D'INTEGRATION DE L'ELEMENT :
C        --------------------------------
         IF (IDEFA.NE.0) THEN
           CALL EPSAMC(NNO, NPG, NBSIG, ZR(IVF), ZR(IDEFA), EPSANE)
         ENDIF
      ELSE
         LPLAS = .FALSE.
      ENDIF
C
C --- RECUPERATION DU COMPORTEMENT  :
C     -------------------------------
      CALL JEVECH('PCOMPOR','L',ICOMPO)
C
C --- RECUPERATION DES VARIABLES INTERNES AUX PT D'INTEGRATION COURANT :
C    ------------------------------------------------------------------
      CALL JEVECH('PVARIGR','L',IVARI)
      CALL TECACH(.TRUE.,.TRUE.,'PVARIGR',7,JTAB)
      NBVARI = MAX(JTAB(6),1)*JTAB(7)

C
C --- VERIFICATION DU COMPORTEMENT FLUAGE :
C     -------------------------------------
      CMP1 = ZK16(ICOMPO)
      CMP2 = ZK16(ICOMPO+7)
      CMP3 = ZK16(ICOMPO+8)
      IF (CMP1(1:10).NE.'GRANGER_FP'.AND.
     +   (CMP1(1:7) .NE.'KIT_DDI'.OR.
     +    CMP2(1:10).NE.'GRANGER_FP')) THEN
         LFLU = .FALSE.
         DO 60 I = 1, MXCMEL
             EPSPLA(I) = ZERO
  60     CONTINUE
         DO 70 I = 1, NBSIG
             EPSFLF(I) = ZERO
  70     CONTINUE
      ELSE
         CALL GRANVI ( MOD3D , IBID , IBID , NVIF )
         LFLU = .TRUE.
      ENDIF
C
C --- DEPENDANCE DES CARACTERISTIQUES MECANIQUES AVEC LA TEMPERATURE :
C     ----------------------------------------------------------------
      LTEMP = .FALSE.
      IF (CMP1(1:15).EQ.'BETON_DOUBLE_DP') THEN
         NVI = 3
         LTEMP = .TRUE.
      ELSEIF (CMP1(1:7).EQ. 'KIT_DDI') THEN
         IF (CMP3(1:15).EQ. 'BETON_DOUBLE_DP' ) THEN
            IF (CMP2(1:10).EQ.'GRANGER_FP') THEN
               NVI = NVIF + 3
               LTEMP = .TRUE.
            ELSE
               CALL UTMESS('F','TE0333',
     &        'COUPLAGE FLUAGE/FISSURATION : LA LOI BETON_DOUBLE_DP '
     &        // 'NE PEUT ETRE COUPLEE QU AVEC UNE LOI DE FLUAGE DE '
     &        // 'GRANGER.')
            ENDIF
         ENDIF
      ENDIF
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
C
      DO 140 IGAU = 1, NPG
         IDECPG = NNO*(IGAU-1) - 1
C
C  ---   TEMPERATURE AU POINT D'INTEGRATION COURANT :
C        ------------------------------------------
         TEMPG     = ZERO
         SECHG     = ZERO
         HYDRG     = ZERO
         HYDRG     = HYDR(IGAU)
C
         DO 80 I = 1, NNO
            TEMPG     = TEMPG    + ZR(IVF+I+IDECPG)*ZR(ITEMPE+I-1)
            SECHG   = SECHG    + ZR(IVF+I+IDECPG)*SECH(I)
  80     CONTINUE
         IF (LTEMP) THEN
            IF (TEMPG.LT.ZR(IVARI+(IGAU-1)*NBVARI+NVI-1))
     +         TEMPG=ZR(IVARI+(IGAU-1)*NBVARI+NVI-1)
         ENDIF
C
C ---    RECUPERATION DES CARACTERISTIQUES DU MATERIAU :
C        ---------------------------------------------
         NOMRES(1) = 'E'
         NOMRES(2) = 'NU'
         NOMRES(3) = 'ALPHA'
C
         NOMPAR(1) = 'TEMP'
         NOMPAR(2) = 'HYDR'
         NOMPAR(3) = 'SECH'
         NOMPAR(4) = 'INST'
         VALPAR(1) = TEMPG
         VALPAR(2) = HYDRG
         VALPAR(3) = SECHG
         VALPAR(4) = ZR(ITEMPS)
C
         CALL RCVALA(ZI(IMATE),'ELAS',4,NOMPAR,VALPAR,2,NOMRES,
     +               VALRES,CODRET,'FM')
C
         CALL RCVALA(ZI(IMATE),'ELAS',4,NOMPAR,VALPAR,1,NOMRES(3),
     +               VALRES(3),CODRET(3),'  ')
C
         E     = VALRES(1)
         NU    = VALRES(2)
         IF (CODRET(3).NE.'OK') THEN
            VALRES(3) = ZERO
         ENDIF
C
C ---    TENSEUR DE DEFORMATION DE FLUAGE AU PT D'INTEGRATION COURANT :
C        --------------------------------------------------------------
         IF(LFLU) THEN
            DO 100 K=1,NBSIG
               EPSFL(K) = ZR(IVARI+(IGAU-1)*NBVARI+8*NBSIG+K-1)
               DO 90 I=1,8
                  EPSFL(K) = EPSFL(K)
     +                     - ZR(IVARI+(IGAU-1)*NBVARI+(I-1)*NBSIG+K-1)
  90           CONTINUE
  100       CONTINUE
C
            C1     = (UN + NU)
            C2     =  - NU
            EPSFLF(1) =      EPSFL(1) + C2 * EPSFL(2) + C2 * EPSFL(3)
            EPSFLF(2) = C2 * EPSFL(1) +      EPSFL(2) + C2 * EPSFL(3)
            EPSFLF(3) = C2 * EPSFL(1) + C2 * EPSFL(2) +      EPSFL(3)
            DO 110 I = 4, NBSIG
               EPSFLF(I) = C1 * EPSFL(I)
 110        CONTINUE
         ENDIF
C
         IF(LPLAS) THEN
C
C ----      TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION COURANT :
C           ------------------------------------------------------
            DO 120 I = 1, NBSIG
              SIGMA(I) = ZR(IDSIG+(IGAU-1)*NBSIG+I-1)
 120        CONTINUE
C
            TRSIG  = SIGMA(1) + SIGMA(2) + SIGMA(3)
C
            C1     = (UN + NU)/E
            C2     =  NU/E
C
C ---       TENSEUR DES DEFORMATIONS PLASTIQUES AU POINT
C ---       D'INTEGRATION COURANT
C ---       I.E. EPSPLA = EPS_TOT - EPS_THERM - EPS_ELAS - EPS_ANELAS :
C ---                             - EPS_FLUAGE :
C           ---------------------------------------------------------
            EPSPLA(NBSIG*(IGAU-1)+1) =   EPSM  (NBSIG*(IGAU-1)+1)
     +                                 - EPSANE(NBSIG*(IGAU-1)+1)
     +                                 - (C1*SIGMA(1) - C2*TRSIG)
     +                                 - EPSFLF(1)
            EPSPLA(NBSIG*(IGAU-1)+2) =   EPSM  (NBSIG*(IGAU-1)+2)
     +                                - EPSANE(NBSIG*(IGAU-1)+2)
     +                                 - (C1*SIGMA(2) - C2*TRSIG)
     +                                 - EPSFLF(2)
            EPSPLA(NBSIG*(IGAU-1)+3) =   EPSM  (NBSIG*(IGAU-1)+3)
     +                                 - EPSANE(NBSIG*(IGAU-1)+3)
     +                                 - (C1*SIGMA(3) - C2*TRSIG)
     +                                 - EPSFLF(3)
            EPSPLA(NBSIG*(IGAU-1)+4) =   EPSM  (NBSIG*(IGAU-1)+4)
     +                                 - EPSANE(NBSIG*(IGAU-1)+4)
     +                                 - C1*SIGMA(4)
     +                                 - EPSFLF(4)
            EPSPLA(NBSIG*(IGAU-1)+5) =   EPSM  (NBSIG*(IGAU-1)+5)
     +                                 - EPSANE(NBSIG*(IGAU-1)+5)
     +                                 - C1*SIGMA(5)
     +                                 - EPSFLF(5)
            EPSPLA(NBSIG*(IGAU-1)+6) =   EPSM  (NBSIG*(IGAU-1)+6)
     +                                 - EPSANE(NBSIG*(IGAU-1)+6)
     +                                 - C1*SIGMA(6)
     +                                 - EPSFLF(6)
         ELSE
C
C ---      TENSEUR DES DEFORMATIONS DE FLUAGE AU POINT
C ---      D'INTEGRATION COURANT
C          ----------------------------------------------
           DO 130 I = 1, NBSIG
             EPSPLA(NBSIG*(IGAU-1)+I) = EPSFLF(I)
 130       CONTINUE
C
       ENDIF
C
 140  CONTINUE
C
C --- RECUPERATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
C     -------------------------------------------------------------
      CALL JEVECH('PDEFOPL','E',IDEFP)
C
C --- AFFECTATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
C     ------------------------------------------------------------
      IF (OPTION(6:9).EQ.'ELGA') THEN
C
C ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---    POINTS D'INTEGRATION :
C        --------------------
         DO 150 IGAU = 1, NPG
         DO 150 ISIG = 1, NBSIG
           ZR(IDEFP+NBSIG*(IGAU-1)+ISIG-1) = EPSPLA(NBSIG*(IGAU-1)+ISIG)
 150     CONTINUE
C
      ELSE IF ( OPTION(6:9) .EQ. 'ELNO' ) THEN
C
C ---    DEFORMATIONS AUX NOEUDS :
C        -----------------------
         CALL PPGANO ( NNOS, NPG, NBSIG, EPSPLA, EPSPLN)
C
C ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---    NOEUDS :
C        ------
         DO 160 INO  = 1, NNO
         DO 160 ISIG = 1, NBSIG
           ZR(IDEFP+NBSIG*(INO-1)+ISIG-1) = EPSPLN(NBSIG*(INO-1)+ISIG)
 160     CONTINUE
C
      ENDIF
C
      END
