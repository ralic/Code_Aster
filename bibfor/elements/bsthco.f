      SUBROUTINE BSTHCO ( NOMTE, BSIGTH, INDITH )
      IMPLICIT  NONE
      REAL*8                     BSIGTH(51)
      LOGICAL                            INDITH
      CHARACTER*8         NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C      CALCUL DU BSIGMA POUR LES CONTRAINTES THERMIQUES
C      (I.E. BT*D*ALPHA(T-TREF)) POUR LES ELEMENTS DE COQUE (COQUE_3D) 
C     ------------------------------------------------------------------
C     IN  NOMTE  : NOM DU TYPE D'ELEMENT
C     OUT BSIGTH : BT*SIGMA POUR LES CONTRAINTES THERMIQUES
C     OUT INDITH : LOGICAL = .TRUE.  YA DES DEFORMATIONS THERMIQUES
C                          = .FALSE. SINON
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       I, ICARA, ICOMPO, ICOU, IER, IMATE, INTE, INTSN, K,
     +              INTSR, ITEMP, ITEMPF, ITREF, JGEOM, LZI, LZR, NB1,
     +              NB2, NBCOU, NBPAR, NBV, NPGE, NPGSN, NPGSR, KWGT,
     +              ITAB(8), IRET
      PARAMETER (NPGE=3)
      REAL*8        VECTA(9,2,3),VECTN(9,3),VECTPT(9,2,3)
      REAL*8        VECTG(2,3),VECTT(3,3)
      REAL*8        HSFM(3,9),HSS(2,9),HSJ1M(3,9),HSJ1S(2,9)
      REAL*8        BTDM(4,3,42),BTDS(4,2,42)
      REAL*8        HSF(3,9),HSJ1FX(3,9),WGT
      REAL*8        BTDF(3,42),BTILD(5,42)
      REAL*8        EPSTH(5),SIGMTH(5),BSIGT1(42)
      REAL*8        KSI3S2,KAPPA,MATC(5,5),VALPU(2)
      REAL*8        ALPHA, COEF, DEUX, EPAIS, EPTOT, QUATRE, T, TEMPG3,
     +              TINF, TPG1, TREF, TROIS, TSUP, UN, VALPAR, VALRES,
     +              ZERO, ZIC, ZMIN, SIX, TEMPG, TEMPG1, TEMPG2
      LOGICAL       TEMPNO  
      CHARACTER*2   CODRET
      CHARACTER*8   NOMPU(2) , NOMRES(2) , NOMPAR
      CHARACTER*10  PHENOM
C     ------------------------------------------------------------------
C
C --- INITIALISATIONS :
C     ---------------
      ZERO   = 0.0D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      QUATRE = 4.0D0
      SIX    = 6.0D0
C
      INDITH = .FALSE.
C
      EPSTH(1) = ZERO
      EPSTH(2) = ZERO
      EPSTH(3) = ZERO
      EPSTH(4) = ZERO
      EPSTH(5) = ZERO
C
      SIGMTH(1) = ZERO
      SIGMTH(2) = ZERO
      SIGMTH(3) = ZERO
      SIGMTH(4) = ZERO
      SIGMTH(5) = ZERO
C
      DO 10 I = 1, 42
        BSIGT1(I) = ZERO
  10  CONTINUE
C
      DO 20 I = 1, 51
        BSIGTH(I) = ZERO
  20  CONTINUE
C
C --- RECUPERATION DE L'OBJET .DESI :
C     -----------------------------
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESI',' ',LZI)
C
C --- NOMBRE DE NOEUDS (NB1 : SERENDIP, NB2 : LAGRANGE) :
C     -------------------------------------------------
      NB1 = ZI(LZI-1+1)
      NB2 = ZI(LZI-1+2)
C
C --- NOMBRE DE POINTS D'INTEGRATION DANS LE PLAN MOYEN
C --- (INTEGRATION REDUITE) :
C     ---------------------
      NPGSR = ZI(LZI-1+3)
C
C --- NOMBRE DE POINTS D'INTEGRATION DANS LE PLAN MOYEN
C --- (INTEGRATION NORMALE) :
C     ---------------------
      NPGSN = ZI(LZI-1+4)
C
C --- RECUPERATION DE L'OBJET .DESR :
C     -----------------------------
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ',LZR)
C
C --- RECUPERATION DE LA CARTE DE COMPORTEMENT :
C     ----------------------------------------
CCC      CALL JEVECH('PCOMPOR','L',ICOMPO)
      CALL TECACH ('NNN','PCOMPOR',8,ITAB,IRET)
      ICOMPO = ITAB(1)
      IF ( ICOMPO .EQ. 0 ) THEN
         NBCOU = 1
      ELSE
C
C ------ NOMBRE DE COUCHES :
C        -----------------
         READ (ZK16(ICOMPO+6-1),'(I3)') NBCOU
C
         IF (NBCOU.LE.0) THEN
           CALL UTMESS('F','BSTHCO','NOMBRE DE COUCHES OBLIGATOIREMENT'
     +                            //' SUPERIEUR A 0 ')
         ENDIF
C
         IF (NBCOU.GT.10) THEN
           CALL UTMESS('F','BSTHCO','NOMBRE DE COUCHES LIMITE A 10 '
     +                            //'POUR LES COQUES 3D ')
         ENDIF
      ENDIF
C
C
C --- CARACTERISTIQUES DE COQUES :
C     --------------------------
      CALL JEVECH('PCACOQU','L',ICARA)
C ---   EPAISSEUR TOTALE :
      EPTOT = ZR(ICARA)
C ---   COORDONNEE MINIMALE SUIVANT L'EPAISSEUR
      ZMIN = -EPTOT/DEUX
C ---   COEFFICIENT DE CORRECTION DU CISAILLEMENT
      KAPPA = ZR(ICARA+3)
C ---   EPAISSEUR D'UNE COUCHE 
      EPAIS = EPTOT/NBCOU
C
C --- RECUPERATION DES COORDONNEES DES NOEUDS DANS LA GEOMETRIE 
C --- INITIALE :
C     --------
      CALL JEVECH('PGEOMER','L',JGEOM)
C
C --- RECUPERATION DU MATERIAU :
C     ------------------------
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
C______________________________________________________________________
C
C---- RECUPERATION DE LA TEMPERATURE
C
      TREF = ZERO
      CALL TECACH('ONN','PTEREF',1,ITREF,IRET)
      IF (ITREF.NE.0) THEN
        TREF = ZR(ITREF)
      ENDIF
C
      CALL TECACH ('OON','PTEMPER',8,ITAB,IRET)
      ITEMP=ITAB(1)
      IF (ITEMP.NE.0) THEN
        TEMPNO = .TRUE.
        NBPAR = 1
        NOMPAR = 'TEMP'
        TPG1 = ZERO
        DO 30 I = 1,NB2
          IF (IRET.EQ.3)
     +       CALL DXTPIF(ZR(ITEMP+3*(I-1)),ZL(ITAB(8)+3*(I-1)))
          T    = ZR(ITEMP+3* (I-1))
          TINF = ZR(ITEMP+1+3* (I-1))
          TSUP = ZR(ITEMP+2+3* (I-1))
          TPG1 = TPG1 + T + (TSUP+TINF-DEUX*T)/SIX
   30   CONTINUE
        VALPAR = TPG1/NB2
      ELSE
C     -- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS':
        CALL TECACH('ONN','PTEMPEF',1,ITEMPF,IRET)
        IF (ITEMPF.GT.0) THEN
          TEMPNO = .FALSE.
          NBPAR = 1
          NOMPAR = 'TEMP'
          NOMPU(1) = 'INST'
          NOMPU(2) = 'EPAIS'
          VALPU(1) = ZERO
          VALPU(2) = ZERO
          CALL FOINTE('FM',ZK8(ITEMPF),2,NOMPU,VALPU,VALPAR,IER)
C     -- SI LA TEMPERATURE N'EST PAS DONNEE:
        ELSE
          TEMPNO = .TRUE.
          NBPAR = 0
          NOMPAR = ' '
          VALPAR = ZERO
        END IF
      END IF
      IF (NBPAR.EQ.0) GOTO 9999
      INDITH = .TRUE.
C______________________________________________________________________
C
C --- DETERMINATION DES REPERES LOCAUX AUX NOEUDS DANS LA 
C --- CONFIGURATION INITIALE 
C --- VECTA DESIGNE LES VECTEURS COVARIANTS DANS LE PLAN MOYEN A
C ---       CHAQUE NOEUD
C --- VECTN DESIGNE LES VECTEURS NORMAUX AU PLAN MOYEN
C --- VECTPT DESIGNE LES REPERES LOCAUX ORTHORNORMES EN CHAQUE
C --- NOEUD DANS LA CONFIGURATION INITIALE :
C     ------------------------------------
      CALL VECTAN(NB1,NB2,ZR(JGEOM),ZR(LZR),VECTA,VECTN,VECTPT)
C
C --- COMPTEUR SERVANT A L'INTEGRATION :
C     --------------------------------
      KWGT = 0
C
C --- BOUCLE SUR LES COUCHES :
C     ----------------------
      DO 40 ICOU = 1, NBCOU
C
C ---   BOUCLE SUR LES POINTS D'INTEGRATION DANS L'EPAISSEUR :
C       ----------------------------------------------------
        DO 50 INTE = 1, NPGE
C
C ---      POSITION DANS L'EPAISSEUR :
           IF (INTE.EQ.1) THEN
              ZIC = ZMIN + (ICOU-1)*EPAIS
              COEF = UN/TROIS
           ELSEIF (INTE.EQ.2) THEN
              ZIC = ZMIN + EPAIS/DEUX + (ICOU-1)*EPAIS
              COEF = QUATRE/TROIS
           ELSEIF (INTE.EQ.3) THEN
              ZIC = ZMIN + EPAIS + (ICOU-1)*EPAIS
              COEF = UN/TROIS
           ENDIF
C ---      COORDONNEE ISOPARAMETRIQUE DANS L'EPAISSEUR DIVISEE PAR 2
           KSI3S2 = ZIC/EPAIS
C
C ---      CALCUL POUR L'INTEGRATION REDUITE DES PARTIES MEMBRANE
C ---      BTDM ET CISAILLEMENT BTDS DE LA MATRICE B :
C          =========================================
C
C ---      BOUCLE SUR LES POINTS DE L'INTEGRATION REDUITE :
C          ----------------------------------------------
           DO 60 INTSR = 1,NPGSR
C
C ---       .D'UNE PART :
C ---        DETERMINATION DES REPERES LOCAUX AUX POINTS D'INTEGRATION 
C ---        DANS LA CONFIGURATION INITIALE 
C ---        VECTG DESIGNE LES VECTEURS COVARIANTS DANS LE PLAN MOYEN 
C ---              EN CHAQUE POINT D'INTEGRATION
C ---        VECTT DESIGNE LES REPERES LOCAUX ORTHORNORMES EN CHAQUE
C ---        POINT D'INTEGRATION DANS LA CONFIGURATION INITIALE 
C ---       .D'AUTRE-PART :
C ---        SOIT H LA MATRICE DE PASSAGE DU TENSEUR DE GREEN-LAGRANGE
C ---        DU REPERE LOCAL AU REPERE GLOBAL
C ---        SOIT S LA MATRICE CONSTANTE TELLE QUE [S]*(DU/DX)
C ---        REPRESENTE LA PARTIE LINEAIRE DU TENSEUR DE GREEN-LAGRANGE
C ---        ON CALCULE LES PRODUITS [HSFM] = [H]*[S] POUR LA PARTIE
C ---                                MEMBRANE-FLEXION
C ---                                [HSS]  = [H] * [S] POUR LA PARTIE
C ---                                CISAILLEMENT :
C           -------------------------------------
            CALL MAHSMS(0,NB1,ZR(JGEOM),KSI3S2,INTSR,ZR(LZR),EPAIS,
     +                  VECTN,VECTG,VECTT,HSFM,HSS)
C
C ---       MULTIPLICATION DES MATRICES [HSFM] ET [HSS] PAR L'INVERSE
C ---       DE LA MATRICE JACOBIENNE [JM1]:
C ---       [HSJ1M] = [HSFM]*[JM1] , [HSJ1S] = [HSS]*[JM1] :
C           ----------------------------------------------
            CALL HSJ1MS(EPAIS,VECTG,VECTT,HSFM,HSS,HSJ1M,HSJ1S)
C
C ---       CALCUL POUR L'INTEGRATION REDUITE DES PARTIES MEMBRANE
C ---       BTDM ET CISAILLEMENT BTDS DE LA MATRICE B :
C           -----------------------------------------
            CALL BTDMSR(NB1,NB2,KSI3S2,INTSR,ZR(LZR),EPAIS,
     +                  VECTPT,HSJ1M,HSJ1S,BTDM,BTDS)
   60     CONTINUE
C
C ---      CALCUL POUR L'INTEGRATION NORMALE DE LA PARTIE FLEXION
C ---      BTDFN DE LA MATRICE B :
C          =====================
C
C ---      BOUCLE SUR LES POINTS DE L'INTEGRATION NORMALE 
C ---      DANS LE PLAN MOYEN :
C          ------------------
           DO 70 INTSN = 1, NPGSN
C
C ---       .D'UNE PART :
C ---        DETERMINATION DES REPERES LOCAUX AUX POINTS D'INTEGRATION 
C ---        DANS LA CONFIGURATION INITIALE 
C ---        VECTG DESIGNE LES VECTEURS COVARIANTS DANS LE PLAN MOYEN 
C ---              EN CHAQUE POINT D'INTEGRATION
C ---        VECTT DESIGNE LES REPERES LOCAUX ORTHORNORMES EN CHAQUE
C ---        POINT D'INTEGRATION DANS LA CONFIGURATION INITIALE 
C ---       .D'AUTRE-PART :
C ---        SOIT H LA MATRICE DE PASSAGE DU TENSEUR DE GREEN-LAGRANGE
C ---        DU REPERE LOCAL AU REPERE GLOBAL
C ---        SOIT S LA MATRICE CONSTANTE TELLE QUE [S]*(DU/DX)
C ---        REPRESENTE LA PARTIE LINEAIRE DU TENSEUR DE GREEN-LAGRANGE
C ---        ON CALCULE LE PRODUIT [HSF] = [H]*[S] POUR LA PARTIE
C ---                              FLEXION :
C           ------------------------------
            CALL MAHSF(1,NB1,ZR(JGEOM),KSI3S2,INTSN,ZR(LZR),EPAIS,
     +                 VECTN,VECTG,VECTT,HSF)
C
C ---       MULTIPLICATION DE LA MATRICE [HSF] PAR L'INVERSE
C ---       DE LA MATRICE JACOBIENNE [JM1]:
C ---       [HSJ1FX] = [HSF]*[JM1]  :
C           ----------------------
            CALL HSJ1F(INTSN,ZR(LZR),EPAIS,VECTG,VECTT,
     +                 HSF,KWGT,HSJ1FX,WGT)
C
C ---       PRODUIT DU POIDS DU POINT DE GAUSS DANS L'EPAISSEUR PAR WGT
C ---       QUI EST LE PRODUIT DU POIDS DU POINT DE GAUSS COURANT
C ---       DANS L'EPAISSEUR PAR LE JACOBIEN EN CE POINT :
C           --------------------------------------------
            WGT = COEF*WGT
C
C ---       CALCUL POUR L'INTEGRATION NORMALE DE LA PARTIE FLEXION
C ---       BTDF DE LA MATRICE B :
C           --------------------
            CALL BTDFN(1,NB1,NB2,KSI3S2,INTSN,ZR(LZR),EPAIS,
     +                 VECTPT,HSJ1FX,BTDF)
C
C ---       CALCUL DE LA MATRICE B [BTILD] PAR INTEGRATION SELECTIVE
C ---       ET INSERTION DES PARTIES [BTDM] ET [BDTS] ET INSERTION
C ---       DE LA PARTIE [BTDF]  :
C           -------------------
            CALL BTDMSN(1,NB1,INTSN,NPGSR,ZR(LZR),BTDM,
     +                  BTDF,BTDS,BTILD)
C
C ---       EVALUATION DES DEFORMATIONS THERMIQUES :
C           ======================================
            K = 459 + 9*(INTSN-1)
            IF (TEMPNO) THEN
              TEMPG1 = ZERO
              TEMPG2 = ZERO
              TEMPG3 = ZERO
C
C ---         TEMPERATURE AU POINT D'INTEGRATION COURANT :
C ---         TEMPG1 SERA LA TEMPERATURE DANS LE PLAN MOYEN DE LA COUCHE
C ---         TEMPG2 SERA LA TEMPERATURE DANS LE PLAN INF   DE LA COUCHE
C ---         TEMPG3 SERA LA TEMPERATURE DANS LE PLAN SUP   DE LA COUCHE
C             ----------------------------------------------------------
              DO 80 I = 1,NB2
C
                  TEMPG1 = TEMPG1 + ZR(ITEMP+3*I-3)*ZR(LZR-1+K+I)
                  TEMPG2 = TEMPG2 + ZR(ITEMP+3*I-2)*ZR(LZR-1+K+I)
                  TEMPG3 = TEMPG3 + ZR(ITEMP+3*I-1)*ZR(LZR-1+K+I)
C
  80          CONTINUE
C
              TEMPG = DEUX* (TEMPG2+TEMPG3-DEUX*TEMPG1)* (ZIC/EPTOT)* 
     +              (ZIC/EPTOT) + (TEMPG3-TEMPG2)* (ZIC/EPTOT) + TEMPG1
C
            ELSE
C
                VALPU(2) = ZIC
                VALPU(1) = ZERO
                CALL FOINTE('FM',ZK8(ITEMPF),2,NOMPU,VALPU,TEMPG,IER)
C
            END IF
C
            VALPAR = TEMPG
            NBV = 1
            NOMRES(1)  = 'ALPHA'
            CALL RCVALA ( ZI(IMATE) , PHENOM , NBPAR , NOMPAR, VALPAR,
     +                    NBV , NOMRES , VALRES , CODRET , '  ' )
            IF (CODRET.NE.'OK') VALRES = ZERO
            ALPHA = VALRES 
C
            EPSTH(1) = ALPHA*(TEMPG-TREF)
            EPSTH(2) = ALPHA*(TEMPG-TREF)
C
C ---       CALCUL DE LA MATRICE DE COMPORTEMENT  MATC(5,5) :
C           ----------------------------------------------
            CALL MATRC2( NBPAR, NOMPAR, VALPAR, KAPPA, MATC, VECTT )
C
C ---       CALCUL DES CONTRAINTES THERMIQUES SIGMTH(5) :
C           ------------------------------------------- 
            CALL PROMAT ( MATC, 5, 5, 5, EPSTH, 5, 5, 1, SIGMTH)
C
C ---       CALCUL DES FORCES INTERNES DUES AUX CONTRAINTES THERMIQUES :
C           ----------------------------------------------------------
            CALL BTSIG ( 5*NB1 + 2 , 5, WGT, BTILD, SIGMTH, BSIGT1)
C
  70       CONTINUE
  50     CONTINUE
  40   CONTINUE
C
C --- EXPANSION DE BSIGT1 DANS BSIGTH :
C     -------------------------------
      CALL VEXPAN ( NB1, BSIGT1, BSIGTH )
      BSIGTH(6*NB1+1) = BSIGT1(5*NB1+1)
      BSIGTH(6*NB1+2) = BSIGT1(5*NB1+2)
C
 9999 CONTINUE
C
      END
