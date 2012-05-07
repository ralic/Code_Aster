      SUBROUTINE AFRELA(COEFR,COEFC,DDL,NOEUD,NDIM,DIRECT,NBTERM,BETAR,
     &                  BETAC,BETAF,TYPCOE,TYPVAL,TYPLAG,EPSI,LISREZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 07/05/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

C       AFRELA -- AFFECTATION D'UNE RELATION A LA .S.D DE TYPE
C                 LISTE_RELA ET DE NOM LISREL

C----------------------------------------------------------------------
C BUT : AFFECTATION D'UNE  RELATION ENTRE DDLS A UNE SD LISTE_RELA
C       (SI L'OBJET LISREZ N'EXISTE PAS, IL EST CREE)

C----------------------------------------------------------------------
C COEFR(NBTERM) - IN - R   - : TABLEAU DES COEFFICIENTS DE LA RELATION
C                              LES COEFFICIENTS SONT REELS
C----------------------------------------------------------------------
C COEFC(NBTERM) - IN - C   - : TABLEAU DES COEFFICIENTS DE LA RELATION
C                              LES COEFFICIENTS SONT COMPLEXES
C----------------------------------------------------------------------
C DDL(NBTERM)   - IN - K8  - : TABLEAU DES DDL DE LA RELATION
C----------------------------------------------------------------------
C NOEUD(NBTERM) - IN - K8  - : TABLEAU DES NOEUDS DE LA RELATION
C-----------------------------------------------------------------------
C NDIM(NBTERM)  - IN - I   - : DIMENSION DU PROBLEME (0, 2 OU 3)
C                              SI = 0 PAS DE CHANGEMENT DE REPERE
C                              LA RELATION EST DONNEE DANS LA BASE
C                              GLOBALE
C----------------------------------------------------------------------
C DIRECT(3,NBTERM)- IN - R - : TABLEAU DES VECTEURS RELATIFS A CHAQUE
C                              TERME DEFINISSANT LA DIRECTION DE LA
C                              COMPOSANTE QUE L'ON VEUT CONTRAINDRE
C----------------------------------------------------------------------
C NBTERM        - IN - I   - : NOMBRE DE TERMES DE LA RELATION
C----------------------------------------------------------------------
C BETAR         - IN - R   - : VALEUR REELLE DU SECOND MEMBRE
C----------------------------------------------------------------------
C BETAC         - IN - C   - : VALEUR COMPLEXE DU SECOND MEMBRE
C----------------------------------------------------------------------
C BETAF         - IN - K19 - : VALEUR FONCTION DU SECOND MEMBRE
C----------------------------------------------------------------------
C TYPCOE        - IN - K4  - : TYPE DES COEFFICIENTS DE LA RELATION :
C                              = 'REEL' OU 'COMP'
C----------------------------------------------------------------------
C TYPVAL        - IN - K4  - : TYPE DU SECOND MEMBRE
C                              = 'REEL' OU 'COMP' OU 'FONC'
C----------------------------------------------------------------------
C TYPLAG        - IN - K2  - : TYPE DES MULTIPLICATEURS DE LAGRANGE
C                              ASSOCIES A LA RELATION :
C                              SI = '12'  LE PREMIER LAGRANGE EST AVANT
C                                         LE NOEUD PHYSIQUE
C                                         LE SECOND LAGRANGE EST APRES
C                              SI = '22'  LE PREMIER LAGRANGE EST APRES
C                                         LE NOEUD PHYSIQUE
C                                         LE SECOND LAGRANGE EST APRES
C----------------------------------------------------------------------
C EPSI       - IN - R  - : VALEUR EN DECA DE LAQUELLE LES COEFFICIENTS
C                          SONT SUPPOSES NULS
C----------------------------------------------------------------------
C LISREZ        - IN - K19 - : NOM DE LA SD LISTE_RELA
C               - JXVAR    -
C----------------------------------------------------------------------

C.========================= DEBUT DES DECLARATIONS ====================
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

C -----  ARGUMENTS
      REAL*8 COEFR(NBTERM),DBLE,DIMAG,EPSI
      REAL*8 DIRECT(3,NBTERM),BETAR2

      INTEGER NDIM(NBTERM)

      COMPLEX*16 BETAC,BETAC2,COEFC(NBTERM)

      CHARACTER*2 TYPLAG
      CHARACTER*4 TYPCOE,TYPVAL,TYPCO2
      CHARACTER*(*) BETAF
      CHARACTER*8 DDL(NBTERM),NOEUD(NBTERM)
      CHARACTER*(*) LISREZ
C ------ VARIABLES LOCALES

      INTEGER     IMULT
      CHARACTER*1 K1BID
      CHARACTER*8 DDLTRA(3),DDLROT(3)
      CHARACTER*19 LISREL
C.========================= DEBUT DU CODE EXECUTABLE ==================
C  DEB
      CALL JEMARQ()
      LISREL = LISREZ
      TYPCO2 = TYPCOE
      IF (TYPCOE.EQ.'FONC') TYPCO2 = 'REEL'
      BETAR2=BETAR
      BETAC2=BETAC


C     -- IMPRESSION DE LA RELATION SI INFO:2:
C     ---------------------------------------
      CALL INFNIV(IFM,NIV)
      IF (NIV.EQ.2) THEN
        WRITE (IFM,*) ' '
        WRITE (IFM,*) '_RELA IMPRESSION D''UNE RELATION LINEAIRE ENTRE '
     &    ,NBTERM,' DDLS. (AVANT NORMALISATION DE LA RELATION)'
        DO 10,K = 1,NBTERM
          IF (NDIM(K).EQ.0) THEN
            IF (TYPCO2.EQ.'REEL') THEN
              WRITE (IFM,1001) COEFR(K),NOEUD(K),DDL(K)
            ELSE IF (TYPCO2.EQ.'COMP') THEN
              WRITE (IFM,1003) DBLE(COEFC(K)),DIMAG(COEFC(K)),NOEUD(K),
     &          DDL(K)
            END IF
          ELSE
            IF (TYPCO2.EQ.'REEL') THEN
              WRITE (IFM,1002) COEFR(K),NOEUD(K),DDL(K),
     &          (DIRECT(KK,K),KK=1,NDIM(K))
            ELSE IF (TYPCO2.EQ.'COMP') THEN
              WRITE (IFM,1004) DBLE(COEFC(K)),DIMAG(COEFC(K)),NOEUD(K),
     &          DDL(K), (DIRECT(KK,K),KK=1,NDIM(K))
            END IF
          END IF
   10   CONTINUE
        IF (TYPVAL.EQ.'REEL') THEN
          WRITE (IFM,*) '_RELA = ',BETAR2
        ELSE IF (TYPVAL.EQ.'COMP') THEN
          WRITE (IFM,*) '_RELA = ',BETAC2
        ELSE IF (TYPVAL.EQ.'FONC') THEN
          WRITE (IFM,*) '_RELA = ',BETAF
        END IF
      END IF


C     1.1 : CALCUL DU COEF DE NORMALISATION RCOEF :
C     ----------------------------------------------
      IF (TYPCO2.EQ.'REEL') THEN
        RCOEF = 0.D0
        DO 20,K = 1,NBTERM
          RCOEF = MAX(RCOEF,ABS(COEFR(K)))
   20   CONTINUE
        IF (RCOEF.EQ.0.D0) CALL U2MESS('F','MODELISA_97')

      ELSE IF (TYPCO2.EQ.'COMP') THEN
        RCOEF = 0.D0
        DO 30,K = 1,NBTERM
          RCOEF = MAX(RCOEF,ABS(COEFC(K)))
   30   CONTINUE
        IF (RCOEF.EQ.0.D0) CALL U2MESS('F','MODELISA_97')
      ELSE
        CALL U2MESK('F','MODELISA_98',1,TYPCO2)
      END IF


      IF (TYPVAL.EQ.'REEL') THEN
        BETAR2= BETAR2/RCOEF
      ELSE IF (TYPVAL.EQ.'COMP') THEN
        BETAC2= BETAC2/RCOEF
      ELSE IF (TYPVAL.EQ.'FONC') THEN
C       -- ON ALARME SI LRCOEF EST TROP DIFFERENT DE 1.
        IF ((RCOEF.GT.1.D3) .OR. (RCOEF.LT.1.D-3))
     &     CALL U2MESS('A','MODELISA_99')
C       -- ON NE PEUT PAS "DIVISER" LE SECOND MEMBRE (FONCTION) !
        RCOEF=1.D0
      END IF


      DDLTRA(1) = 'DX'
      DDLTRA(2) = 'DY'
      DDLTRA(3) = 'DZ'

      DDLROT(1) = 'DRX'
      DDLROT(2) = 'DRY'
      DDLROT(3) = 'DRZ'

      IROT = 0

C --- SI L'OBJET LISREL N'EXISTE PAS, ON LE CREE :
C     ------------------------------------------
      CALL JEEXIN(LISREL//'.RLCO',IRET)
      IF (IRET.EQ.0) CALL CRELRL(TYPCO2,TYPVAL,'V',LISREL)

C --- NOMBRE DE RELATIONS DE LA LISTE_DE_RELATIONS :
C     --------------------------------------------
      CALL JEVEUO(LISREL//'.RLNR','E',IDNBRE)
      NBREL0 = ZI(IDNBRE)
      NBRELA = NBREL0 + 1

C --- LONGUEUR TOTALE DES VECTEURS RELATIFS A TOUS LES TERMES
C --- DES RELATIONS :
C     -------------
      CALL JELIRA(LISREL//'.RLCO','LONMAX',LVECLR,K1BID)

C --- LONGUEUR EFFECTIVEMENT UTILISEE :
C     -------------------------------
      CALL JEVEUO(LISREL//'.RLPO','E',IDPOIN)
      IF (NBREL0.EQ.0) THEN
        LONUTI = 0
      ELSE
        LONUTI = ZI(IDPOIN+NBREL0-1)
      END IF

C --- SI LA RELATION N'EST PAS DONNEE DANS LE REPERE  GLOBAL, ON
C --- REAJUSTE SON NOMBRE DE TERMES, D'AUTRE_PART ON NE TIENT PAS
C --- COMPTE DES TERMES A COEFFICIENT NUL :
C     -----------------------------------
      NBTERR = 0
      DO 40 I = 1,NBTERM
        IF (TYPCO2.EQ.'COMP') THEN
          IF (ABS(COEFC(I)).GT.EPSI) THEN
            IF (NDIM(I).EQ.0) THEN
              NBTERR = NBTERR + 1
            ELSE
              NBTERR = NBTERR + NDIM(I)
            END IF
          END IF
        ELSE
          IF (ABS(COEFR(I)).GT.EPSI) THEN
            IF (NDIM(I).EQ.0) THEN
              NBTERR = NBTERR + 1
            ELSE
              NBTERR = NBTERR + NDIM(I)
            END IF
          END IF
        END IF
   40 CONTINUE

C --- ON RECREE LES OBJETS RELATIFS AUX TERMES DES RELATIONS EN
C --- AUGMENTANT LA TAILLE DE CES OBJETS SI LEUR LONGUEUR S'AVERE
C --- INSUFFISANTE :
C     ------------
      IF (LONUTI+NBTERR.GE.LVECLR) THEN
        IMULT = (LONUTI+NBTERR)/LVECLR + 1
        CALL JUVECA(LISREL//'.RLCO',IMULT*LVECLR)
        CALL JUVECA(LISREL//'.RLDD',IMULT*LVECLR)
        CALL JUVECA(LISREL//'.RLNO',IMULT*LVECLR)
      END IF

C --- NOMBRE MAX DE RELATIONS INITIALEMENT PREVU :
C     ------------------------------------------
      CALL JELIRA(LISREL//'.RLNT','LONMAX',NBRMAX,K1BID)

C --- ON RECREE LES OBJETS DIMENSIONNES AU NOMBRE DE RELATIONS
C --- EN AUGMENTANT LA TAILLE DE CES OBJETS  SI LEUR LONGUEUR
C --- S'AVERE INSUFFISANTE :
C     --------------------
      IF (NBRELA.GE.NBRMAX) THEN
        IMULT = NBRELA/NBRMAX + 1
        CALL JUVECA(LISREL//'.RLBE',IMULT*NBRMAX)
        CALL JUVECA(LISREL//'.RLNT',IMULT*NBRMAX)
        CALL JUVECA(LISREL//'.RLPO',IMULT*NBRMAX)
        CALL JUVECA(LISREL//'.RLSU',IMULT*NBRMAX)
        CALL JUVECA(LISREL//'.RLLA',IMULT*NBRMAX)
      END IF

C --- AFFECTATION DES COMPOSANTES DE LA RELATION A LA LISTE
C --- DE RELATIONS :
C     ------------
      CALL JEVEUO(LISREL//'.RLNR','E',IDNBRE)
      CALL JEVEUO(LISREL//'.RLCO','E',IDCOEF)
      CALL JEVEUO(LISREL//'.RLDD','E',IDDL)
      CALL JEVEUO(LISREL//'.RLNO','E',IDNOEU)
      CALL JEVEUO(LISREL//'.RLBE','E',IDBETA)
      CALL JEVEUO(LISREL//'.RLNT','E',IDTERM)
      CALL JEVEUO(LISREL//'.RLPO','E',IDPOIN)
      CALL JEVEUO(LISREL//'.RLSU','E',IDSURC)
      CALL JEVEUO(LISREL//'.RLLA','E',IDLAGR)

      ZI(IDNBRE) = NBRELA
      ZK8(IDLAGR+NBRELA-1) (1:2) = TYPLAG
      IF (NBREL0.EQ.0) THEN
        IPOINT = 0
      ELSE
        IPOINT = ZI(IDPOIN+NBREL0-1)
      END IF

      K = 0

C --- CAS DES COEFFICIENTS COMPLEXES :
C     ------------------------------
      IF (TYPCO2.EQ.'COMP') THEN
        DO 60 I = 1,NBTERM
C ---   ON NE TIENT COMPTE QUE DES COEFFICIENTS NON-NULS
          IF (ABS(COEFC(I)).GT.EPSI) THEN
            IF (NDIM(I).EQ.0) THEN
              K = K + 1
              ZC(IDCOEF+IPOINT+K-1) = COEFC(I)/RCOEF
              ZK8(IDDL+IPOINT+K-1) = DDL(I)
              ZK8(IDNOEU+IPOINT+K-1) = NOEUD(I)
            ELSE

C --- LA NOUVELLE RELATION ECRITE DANS LE REPERE GLOBAL EST
C --- DETERMINEE AVEC LA REGLE DE CORRESPONDANCE  SUIVANTE :
C ---  DEPL --> DIRECT(1)*U  + DIRECT(2)*V  + DIRECT(3)*W
C ---  ROTA --> DIRECT(1)*RX + DIRECT(2)*RY + DIRECT(3)*RZ
C      ---------------------------------------------------
              IF (DDL(I).EQ.'DEPL') THEN
                IROT = 0
              ELSE IF (DDL(I).EQ.'ROTA') THEN
                IROT = 1
              ELSE
                CALL ASSERT(.FALSE.)
              END IF

              MDIM = NDIM(I)
              DO 50 IDIM = 1,MDIM
                K = K + 1
                ZC(IDCOEF+IPOINT+K-1) = COEFC(I)/RCOEF*DIRECT(IDIM,I)
                ZK8(IDNOEU+IPOINT+K-1) = NOEUD(I)
                IF (IROT.EQ.0) THEN
                  ZK8(IDDL+IPOINT+K-1) = DDLTRA(IDIM)
                ELSE IF (IROT.EQ.1) THEN
                  ZK8(IDDL+IPOINT+K-1) = DDLROT(IDIM)
                END IF
   50         CONTINUE
            END IF
          END IF
   60   CONTINUE

C --- CAS DES COEFFICIENTS REELS :
C     --------------------------
      ELSE
        DO 80 I = 1,NBTERM
C ---   ON NE TIENT COMPTE QUE DES COEFFICIENTS NON-NULS
          IF (ABS(COEFR(I)).GT.EPSI) THEN
            IF (NDIM(I).EQ.0) THEN
              K = K + 1
              ZR(IDCOEF+IPOINT+K-1) = COEFR(I)/RCOEF
              ZK8(IDDL+IPOINT+K-1) = DDL(I)
              ZK8(IDNOEU+IPOINT+K-1) = NOEUD(I)
            ELSE

C --- LA NOUVELLE RELATION ECRITE DANS LE REPERE GLOBAL EST
C --- DETERMINEE AVEC LA REGLE DE CORRESPONDANCE  SUIVANTE :
C ---  DEPL --> DIRECT(1)*U  + DIRECT(2)*V  + DIRECT(3)*W
C ---  ROTA --> DIRECT(1)*RX + DIRECT(2)*RY + DIRECT(3)*RZ
C      ---------------------------------------------------
              IF (DDL(I).EQ.'DEPL') THEN
                IROT = 0
              ELSE IF (DDL(I).EQ.'ROTA') THEN
                IROT = 1
              ELSE
                CALL ASSERT(.FALSE.)
              END IF

              MDIM = NDIM(I)
              DO 70 IDIM = 1,MDIM
                K = K + 1
                ZR(IDCOEF+IPOINT+K-1) = COEFR(I)/RCOEF*DIRECT(IDIM,I)
                ZK8(IDNOEU+IPOINT+K-1) = NOEUD(I)
                IF (IROT.EQ.0) THEN
                  ZK8(IDDL+IPOINT+K-1) = DDLTRA(IDIM)
                ELSE IF (IROT.EQ.1) THEN
                  ZK8(IDDL+IPOINT+K-1) = DDLROT(IDIM)
                END IF
   70         CONTINUE
            END IF
          END IF
   80   CONTINUE
      END IF

      IF (TYPVAL.EQ.'REEL') THEN
        ZR(IDBETA+NBRELA-1) = BETAR2
      ELSE IF (TYPVAL.EQ.'COMP') THEN
        ZC(IDBETA+NBRELA-1) = BETAC2
      ELSE IF (TYPVAL.EQ.'FONC') THEN
        ZK24(IDBETA+NBRELA-1) = BETAF
      END IF

      ZI(IDTERM+NBRELA-1) = NBTERR
      IF (NBREL0.EQ.0) THEN
        ZI(IDPOIN) = NBTERR
      ELSE
        ZI(IDPOIN+NBRELA-1) = ZI(IDPOIN+NBREL0-1) + NBTERR
      END IF

      CALL JEDEMA()

 1001 FORMAT (' _RELA ',E14.7,A10,A10)
 1003 FORMAT (' _RELA ',E14.7,1X,E14.7,A10,A10)
 1002 FORMAT (' _RELA ',E14.7,A10,A10,3X,3 (1X,E14.7))
 1004 FORMAT (' _RELA ',E14.7,1X,E14.7,A10,A10,3X,3 (1X,E14.7))

      END
