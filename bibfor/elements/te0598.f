      SUBROUTINE TE0598 ( OPTION , NOMTE )
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
C TOLE CRP_20
C
      IMPLICIT NONE
C
      CHARACTER*16        OPTION , NOMTE
C
C ......................................................................
C    - CE PROGRAMME EST INSPIRE DE TE0078
C      LES MODIFICATIONS DE L'UN OU DE L'AUTRE DOIVENT ETRE SIMULTANEES
C
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_DLAG_EVOLST' EN STATIONNAIRE
C                                   'CHAR_DLAG_EVOLTR' EN TRANSITOIRE
C
C     TERMES PROVENANT DE LA DERIVATION DU PREMIER MEMBRE DE L'EQUATION.
C
C    - ATTENTION : ON SUPPOSE QUE RHO, CP ET LAMBDA SONT CONSTANTS
C      DANS L'ELEMENT. LEUR CONTRIBUTION EN GRADIENT EST DONC NULLE.
C
C    - EN STATIONNAIRE :
C      ===============
C
C      + LAMBDA ( GRAD(T).GRAD(THETA) ) . GRAD(T*)
C      + LAMBDA(GRAD(T)) . ( GRAD(T*).GRAD(THETA) )
C      - DIV(THETA) . ( LAMBDA(GRAD(T)).GRAD(T*) )
C
C    - ON NE FAIT AUCUN CALCUL SUR UN ELEMENT OU LE CHAMP THETA EST NUL
C      SUR TOUS LES NOEUDS. EN EFFET, DANS CE CAS LA DIVERGENCE ET LE
C      GRADIENT DE THETA SONT NULS SUR TOUS LES POINTS DE GAUSS DE
C      L'ELEMENT. DU COUP, LA CONTRIBUTION AU SECOND MEMBRE EST NULLE.
C
C    - EN TRANSITOIRE :
C      ==============
C
C      DANS LA MEME FORMULE, ON AJOUTE LES TERMES DE DERIVEE EN TEMPS
C      ET ON IMPLICITE LA TEMPERATURE DANS LE CALCUL DE SON GRADIENT :
C             GRAD(T) = THTIMP.GRAD(T+) + (1-THTIMP).GRAD(T-)
C
C        ( RHO . CP / DELTAT ) (DLAGTE-) . T*
C      - (1-THTIMP) LAMBDA ( GRAD(DLAGTE-) . GRAD(T*) )
C      - RHO . CP . DIVTHT . ((T+)-(T-))/DELTAT . T*
C      + LAMBDA ( GRAD(T).GRAD(THETA) ) . GRAD(T*)
C      + LAMBDA(GRAD(T)) . ( GRAD(T*).GRAD(THETA) )
C      - DIV(THETA) . ( LAMBDA(GRAD(T)).GRAD(T*) )
C
C      SI LE CHAMP THETA EST NUL, LES QUATRE DERNIERS TERMES SONT NULS
C
C    - ELEMENTS ISOPARAMETRIQUES 2D
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      INTEGER            NBRES
      PARAMETER         ( NBRES=3 )
C
      CHARACTER*2        CODRET(NBRES)
      CHARACTER*8        NOMRES(NBRES),ELREFE
      CHARACTER*16       PHENOM
      CHARACTER*24       CARAC,FF
C
      REAL*8             COORSE(18),VECTT(9)
      REAL*8             R
      REAL*8             POIDS, DFDX(9), DFDY(9)
      REAL*8             R8AUX, GRADTH(2,2), DIVTHT, THETAR
      REAL*8             R8DGRD, EPSI, R8PREM
      REAL*8             CP
      REAL*8             ORIG(2),LAMBOR(2),LAMBDA,FLUGLO(2),FLULOC(2)
      REAL*8             P(2,2),POINT(2), XU, YU, XNORM, ALPHA
      REAL*8             TMO, TPL, DLAGTG, GRADDT(2), FLUGLD(2)
      REAL*8             TEMPNO(9), GRADT(2), FLUGRM(2)
      REAL*8             DELTAT, THTIMP, UNMTHE
      REAL*8             VALRES(NBRES)
C
      INTEGER            IGEOM, IVECTT, ICARAC
      INTEGER            IFF, IPOIDS, IVF, IDFDE, IDFDK
      INTEGER            NNO, NPG1, NPG2, NPG3, NSE, NNOP2, C(6,9), ISE
      INTEGER            KP, I, K, IDEB, IFIN, NUNO
      INTEGER            IMATE, ICAMAS
      INTEGER            ITEMPM, ITEMPP, IDLAGT
      INTEGER            ITHETA
      INTEGER            ITEMPS
C
      LOGICAL THTNUL
      LOGICAL AXI
      LOGICAL TRANSI
      LOGICAL ANISO, GLOBAL
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C====
C 1. INITIALISATIONS
C====
C
C 1.1. ==> LES INDISPENSABLES
C
      CALL ELREF1(ELREFE)
      EPSI = R8PREM ()
C
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
      NPG2 = ZI(ICARAC+3)
      NPG3 = ZI(ICARAC+4)
C
      CALL JEVECH('PVECTTH','L',ITHETA)
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PVECTTR','E',IVECTT)
C
      CALL CONNEC ( NOMTE, ZR(IGEOM), NSE, NNOP2, C )
C
      DO 101 , I = 1 , NNOP2
        VECTT(I)=0.D0
  101 CONTINUE
C
C 1.2. ==> CONTROLE DE LA NULLITE DE THETA
C
      IDEB = ITHETA
      IFIN = ITHETA + 2*NNO - 1
      THTNUL = .TRUE.
      DO 102 , I = IDEB , IFIN
        IF ( ABS(ZR(I)).GT.EPSI ) THEN
          THTNUL = .FALSE.
        ENDIF
  102 CONTINUE
C
C 1.3. ==> LE CALCUL EST-IL STATIONNAIRE OU TRANSITOIRE ?
C
      IF ( OPTION(15:16).EQ.'ST' ) THEN
        TRANSI = .FALSE.
      ELSEIF ( OPTION(15:16).EQ.'TR' ) THEN
        TRANSI = .TRUE.
      ELSE
        CALL UTMESS ('F','TE0598','MAUVAISE OPTION')
      ENDIF
C
C====
C 2. IL N'Y A CALCUL QUE SI THETA EST NON NUL OU EN TRANSITOIRE
C    VOIR L'EN-TETE DU PROGRAMME
C====
C
      IF ( TRANSI .OR. .NOT.THTNUL ) THEN
C
C 2.1. ==> 2D PLAN OU AXI ?
C
      IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
        AXI = .TRUE.
      ELSE
        AXI = .FALSE.
      ENDIF
C
C 2.2. ==> FIN DES INITIALISATIONS
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
C
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPER','L',ITEMPM)
C
      IF ( TRANSI ) THEN
C
        CALL JEVECH('PTEMPEP','L',ITEMPP)
        CALL JEVECH('PDLAGTE','L',IDLAGT)
C
        DELTAT = ZR(ITEMPS+1)
        THTIMP = ZR(ITEMPS+2)
        UNMTHE = 1.D0 - THTIMP
C
      ENDIF
C
C 2.3. ==> MATERIAU
C
      CALL RCCOMA ( ZI(IMATE), 'THER', PHENOM, CODRET )
C
      IF ( PHENOM .EQ. 'THER') THEN
         NOMRES(1) = 'RHO_CP'
         NOMRES(2) = 'LAMBDA'
         CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'INST', ZR(ITEMPS),
     &                            2, NOMRES, VALRES, CODRET, 'FM' )
         CP     = VALRES(1)
         LAMBDA = VALRES(2)
         ANISO  = .FALSE.
      ELSEIF ( PHENOM .EQ. 'THER_ORTH') THEN
         NOMRES(1) = 'RHO_CP'
         NOMRES(2) = 'LAMBDA_L'
         NOMRES(3) = 'LAMBDA_T'
         CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'INST', ZR(ITEMPS),
     &                            3, NOMRES, VALRES, CODRET, 'FM' )
         CP        = VALRES(1)
         LAMBOR(1) = VALRES(2)
         LAMBOR(2) = VALRES(3)
         ANISO     = .TRUE.
      ELSE
         CALL UTMESS ('F','TE0598','COMPORTEMENT NON TROUVE')
      ENDIF
      GLOBAL = .FALSE.
      IF ( ANISO ) THEN
        CALL JEVECH('PCAMASS','L',ICAMAS)
        IF (ZR(ICAMAS).GT.0.D0) THEN
          GLOBAL = .TRUE.
          ALPHA  = ZR(ICAMAS+1)*R8DGRD()
          P(1,1) =  COS(ALPHA)
          P(2,1) =  SIN(ALPHA)
          P(1,2) = -SIN(ALPHA)
          P(2,2) =  COS(ALPHA)
        ELSE
          ORIG(1) = ZR(ICAMAS+4)
          ORIG(2) = ZR(ICAMAS+5)
        ENDIF
      ENDIF
C
C 2.4. ==> CALCUL NON LUMPE
C          ----------------
      IF (NOMTE(6:6).NE.'L') THEN
C
C 2.4.1. ==> TEMPERATURES AUX NOEUDS
C            EN TRANSITOIRE  : THTIMP.(T+) + (1-THTIMP).(T-)
C            EN STATIONNAIRE : T
C
        IF ( TRANSI ) THEN
          DO 2411 , I = 1 , NNO
            TEMPNO(I) = THTIMP*ZR(ITEMPP+I-1) + UNMTHE*ZR(ITEMPM+I-1)
 2411     CONTINUE
        ELSE
          DO 2412 , I = 1 , NNO
            TEMPNO(I) = ZR(ITEMPM+I-1)
 2412     CONTINUE
        ENDIF
C
        IPOIDS=IFF   +NPG1*(1+3*NNO)
        IVF   =IPOIDS+NPG2
        IDFDE =IVF   +NPG2*NNO
        IDFDK =IDFDE +NPG2*NNO
C
C 2.4.2. ==> BOUCLE SUR LES POINTS DE GAUSS
C
        DO 242 , KP = 1 , NPG2
C
          K = (KP-1)*NNO
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     >                  ZR(IGEOM),DFDX,DFDY,POIDS )
C
C 2.4.2.1. ==> EN TRANSITOIRE, CALCUL DE LA DERIVEE DE T ET DE SON
C              GRADIENT A L'INSTANT PRECEDENT.
C              SI LE CHAMP THETA N'EST PAS NUL, CALCUL DES TEMPERATURES
C              AUX DEUX INSTANTS
C              SI LE CHAMP THETA EST NUL, IL EST INUTILE DE CALCULER
C              LES TEMPERATURES + ET -.
C
          IF ( TRANSI ) THEN
C
            GRADDT(1) = 0.D0
            GRADDT(2) = 0.D0
            DLAGTG = 0.D0
            IF ( THTNUL ) THEN
              DO 2431 , I = 1 , NNO
                R8AUX = ZR(IVF+K+I-1)
                DLAGTG    = DLAGTG    + ZR(IDLAGT+I-1)*R8AUX
                GRADDT(1) = GRADDT(1) + ZR(IDLAGT+I-1)*DFDX(I)
                GRADDT(2) = GRADDT(2) + ZR(IDLAGT+I-1)*DFDY(I)
 2431         CONTINUE
            ELSE
              TMO = 0.D0
              TPL = 0.D0
              DO 2432 , I = 1 , NNO
                R8AUX = ZR(IVF+K+I-1)
                DLAGTG    = DLAGTG    + ZR(IDLAGT+I-1)*R8AUX
                GRADDT(1) = GRADDT(1) + ZR(IDLAGT+I-1)*DFDX(I)
                GRADDT(2) = GRADDT(2) + ZR(IDLAGT+I-1)*DFDY(I)
                TMO       = TMO       + ZR(ITEMPM+I-1)*R8AUX
                TPL       = TPL       + ZR(ITEMPP+I-1)*R8AUX
 2432         CONTINUE
            ENDIF
C
          ENDIF
C
C 2.4.2.2. ==> CALCUL DES GRADIENTS DE TEMPERATURE,
C             DU GRADIENT ET DE LA DIVERGENCE DE THETA AU POINT DE GAUSS
C   GRADTH(I,K) = D THETA I / D X K
C   DIVTHT = D THETA X / DX  +  D THETA Y / DY
C          = D THETA R / DR  +  D THETA Z / DZ  +  THETA R / R
C              SI LE CHAMP THETA EST NUL, IL EST INUTILE DE CALCULER
C              LES GRADIENTS DE THETA
C
          GRADT(1) = 0.D0
          GRADT(2) = 0.D0
          IF ( THTNUL ) THEN
            DO 2441 , I = 1 , NNO
              GRADT(1)    = GRADT(1)    + TEMPNO(I)*DFDX(I)
              GRADT(2)    = GRADT(2)    + TEMPNO(I)*DFDY(I)
 2441       CONTINUE
          ELSE
            GRADTH(1,1) = 0.D0
            GRADTH(1,2) = 0.D0
            GRADTH(2,1) = 0.D0
            GRADTH(2,2) = 0.D0
            DO 2442 , I = 1 , NNO
              GRADT(1)    = GRADT(1)    + TEMPNO(I)*DFDX(I)
              GRADT(2)    = GRADT(2)    + TEMPNO(I)*DFDY(I)
              GRADTH(1,1) = GRADTH(1,1) + ZR(ITHETA+2*I-2)*DFDX(I)
              GRADTH(1,2) = GRADTH(1,2) + ZR(ITHETA+2*I-2)*DFDY(I)
              GRADTH(2,1) = GRADTH(2,1) + ZR(ITHETA+2*I-1)*DFDX(I)
              GRADTH(2,2) = GRADTH(2,2) + ZR(ITHETA+2*I-1)*DFDY(I)
 2442       CONTINUE
            DIVTHT = GRADTH(1,1) + GRADTH(2,2)
          ENDIF
C
C 2.4.2.3. ==>
C EN 2D-AXI, MODIFICATION DU POIDS ET TERME
C COMPLEMENTAIRE SUR LA DIVERGENCE EN THETAR/R
C LES POINTS DE GAUSS ETANT TOUJOURS STRICTEMENT INTERIEURS
C A L'ELEMENT, R NE PEUT PAS ETRE NUL, DONC ON PEUT DIVISER PAR R.
C
          IF ( AXI ) THEN
            R = 0.D0
            IF ( THTNUL ) THEN
              DO 2451 , I = 1 , NNO
                R      = R      +  ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
 2451         CONTINUE
            ELSE
              THETAR = 0.D0
              DO 2452 , I = 1 , NNO
                R      = R      +  ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
                THETAR = THETAR + ZR(ITHETA+2*I-2)*ZR(IVF+K+I-1)
 2452         CONTINUE
              DIVTHT = DIVTHT + THETAR / R
            ENDIF
            POIDS = POIDS*R
          ENDIF
C
C 2.4.2.4. ==> CALCUL DES TERMES DE FLUX DE TEMPERATURE, FLUGLO, OU DE
C              DERIVEE DE TEMPERATURE, FLUGLD.
C
C FLUX GLOBAL : LAMBDA.GRAD(T) --> FLUGLO
C               LAMBDA.GRAD(DLAGT) --> FLUGLD
C
          IF ( .NOT.GLOBAL .AND. ANISO ) THEN
C
            POINT(1)=0.D0
            POINT(2)=0.D0
            DO 246 , NUNO = 1 , NNO
              POINT(1) = POINT(1) + ZR(IVF+K+NUNO-1)*ZR(IGEOM+2*NUNO-2)
              POINT(2) = POINT(2) + ZR(IVF+K+NUNO-1)*ZR(IGEOM+2*NUNO-1)
 246        CONTINUE
C
            XU = ORIG(1) - POINT(1)
            YU = ORIG(2) - POINT(2)
            XNORM = SQRT( XU**2 + YU**2 )
            XU = XU / XNORM
            YU = YU / XNORM
            P(1,1) =  XU
            P(2,1) =  YU
            P(1,2) = -YU
            P(2,2) =  XU
C
          ENDIF
C
          IF ( .NOT.ANISO ) THEN
            IF ( .NOT.THTNUL ) THEN
              FLUGLO(1) = LAMBDA*GRADT(1)
              FLUGLO(2) = LAMBDA*GRADT(2)
            ENDIF
            IF ( TRANSI ) THEN
              FLUGLD(1) = LAMBDA*GRADDT(1)
              FLUGLD(2) = LAMBDA*GRADDT(2)
            ENDIF
          ELSE
            IF ( .NOT.THTNUL ) THEN
              FLULOC(1) = P(1,1)*GRADT(1) + P(2,1)*GRADT(2)
              FLULOC(2) = P(1,2)*GRADT(1) + P(2,2)*GRADT(2)
              FLULOC(1) = LAMBOR(1)*FLULOC(1)
              FLULOC(2) = LAMBOR(2)*FLULOC(2)
              FLUGLO(1) = P(1,1)*FLULOC(1) + P(1,2)*FLULOC(2)
              FLUGLO(2) = P(2,1)*FLULOC(1) + P(2,2)*FLULOC(2)
            ENDIF
            IF ( TRANSI ) THEN
              FLULOC(1) = P(1,1)*GRADDT(1) + P(2,1)*GRADDT(2)
              FLULOC(2) = P(1,2)*GRADDT(1) + P(2,2)*GRADDT(2)
              FLULOC(1) = LAMBOR(1)*FLULOC(1)
              FLULOC(2) = LAMBOR(2)*FLULOC(2)
              FLUGLD(1) = P(1,1)*FLULOC(1) + P(1,2)*FLULOC(2)
              FLUGLD(2) = P(2,1)*FLULOC(1) + P(2,2)*FLULOC(2)
            ENDIF
          ENDIF
C
C PRODUIT CONTRACTE : FLUX.GRADIENT(THETA)
C
          IF ( .NOT.THTNUL ) THEN
            FLUGRM(1) = FLUGLO(1)*GRADTH(1,1) + FLUGLO(2)*GRADTH(2,1)
            FLUGRM(2) = FLUGLO(1)*GRADTH(1,2) + FLUGLO(2)*GRADTH(2,2)
          ENDIF
C
C 2.4.2.5. ==> CONTRIBUTION AU SECOND MEMBRE :
C        ( RHO . CP / DELTAT ) (DLAGTE-) . T*
C      - (1-THTIMP) LAMBDA ( GRAD(DLAGTE-) . GRAD(T*) )
C      - RHO . CP . DIVTHT . ((T+)-(T-))/DELTAT . T*
C      + LAMBDA ( GRAD(T).GRAD(THETA) ) . GRAD(T*)
C      + LAMBDA(GRAD(T)) . ( GRAD(T*).GRAD(THETA) )
C      - DIV(THETA) . ( LAMBDA(GRAD(T)).GRAD(T*) )
C
C         . TERMES SPECIFIQUES DU TRANSITOIRE :
C
          IF ( TRANSI ) THEN
C
          DO 2471 , I = 1 , NNO
             ZR(IVECTT+I-1) = ZR(IVECTT+I-1) + POIDS * (
     >              CP/DELTAT*DLAGTG*ZR(IVF+K+I-1)
     >            - UNMTHE*(FLUGLD(1)*DFDX(I) + FLUGLD(2)*DFDY(I))
     >            )
 2471     CONTINUE
C
          ENDIF
C
C         . TERMES APPARAISSANT SI THETA N'EST PAS NUL
C
          IF ( .NOT.THTNUL ) THEN
C
          IF ( TRANSI ) THEN
            R8AUX = - DIVTHT*CP/DELTAT*(TPL-TMO)
          ELSE
            R8AUX = 0.D0
          ENDIF
C
          DO 2472 , I = 1 , NNO
             ZR(IVECTT+I-1) = ZR(IVECTT+I-1) + POIDS * (
     >              R8AUX*ZR(IVF+K+I-1)
     >            + FLUGRM(1)*DFDX(I) + FLUGRM(2)*DFDY(I)
     >            + FLUGLO(1)*(DFDX(I)*GRADTH(1,1)+DFDY(I)*GRADTH(1,2))
     >            + FLUGLO(2)*(DFDX(I)*GRADTH(2,1)+DFDY(I)*GRADTH(2,2))
     >            - DIVTHT*(FLUGLO(1)*DFDX(I) + FLUGLO(2)*DFDY(I))
     >            )
 2472     CONTINUE
C
          ENDIF
C
  242   CONTINUE
C
      ELSE
C
C 2.5. ==> CALCUL LUMPE
C          ------------
C  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES
C
C BOUCLE SUR LES SOUS-ELEMENTS
C
        DO 252 , ISE=1,NSE
C
C COORDONNEES LOCALES
C
        DO 2521 , I = 1 , NNO
          COORSE(2*I-1) = ZR(IGEOM+2*C(ISE,I)-2)
          COORSE(2*I)   = ZR(IGEOM+2*C(ISE,I)-1)
 2521   CONTINUE
C
C 2.5.1. ==> TEMPERATURES AUX NOEUDS
C            EN TRANSITOIRE  : THTIMP.(T+) + (1-THTIMP).(T-)
C            EN STATIONNAIRE : T
C
        IF ( TRANSI ) THEN
          DO 2522 , I = 1 , NNO
            TEMPNO(I) = THTIMP*ZR(ITEMPP+C(ISE,I)-1)
     >                + UNMTHE*ZR(ITEMPM+C(ISE,I)-1)
 2522     CONTINUE
        ELSE
          DO 2523 , I = 1 , NNO
            TEMPNO(I) = ZR(ITEMPM+C(ISE,I)-1)
 2523     CONTINUE
        ENDIF
C
C TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------
C
        IPOIDS=IFF   +NPG1*(1+3*NNO)
        IVF   =IPOIDS+NPG2
        IDFDE =IVF   +NPG2*NNO
        IDFDK =IDFDE +NPG2*NNO
C
        DO 2520 , KP = 1 , NPG2
C
          K = (KP-1)*NNO
C
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     >                  COORSE,DFDX,DFDY,POIDS )
C
C 2.5.2.1. ==> EN TRANSITOIRE, CALCUL DU GRADIENT DE LA DERIVEE DE T
C              A L'INSTANT PRECEDENT
C
          IF ( TRANSI ) THEN
            GRADDT(1) = 0.D0
            GRADDT(2) = 0.D0
            DO 2524 , I = 1 , NNO
              GRADDT(1) = GRADDT(1) + ZR(IDLAGT+C(ISE,I)-1)*DFDX(I)
              GRADDT(2) = GRADDT(2) + ZR(IDLAGT+C(ISE,I)-1)*DFDY(I)
 2524       CONTINUE
          ENDIF
C
C 2.5.2.2. ==> CALCUL DES GRADIENTS DE TEMPERATURE,
C             DU GRADIENT ET DE LA DIVERGENCE DE THETA AU POINT DE GAUSS
C   GRADTH(I,K) = D THETA I / D X K
C   DIVTHT = D THETA X / DX  +  D THETA Y / DY
C         = D THETA R / DR  +  D THETA Z / DZ  +  THETA R / R
C              SI LE CHAMP THETA EST NUL, IL EST INUTILE DE CALCULER
C              LES GRADIENTS DE THETA
C
          GRADT(1) = 0.D0
          GRADT(2) = 0.D0
          IF ( THTNUL ) THEN
            DO 25221 , I = 1 , NNO
              GRADT(1)    = GRADT(1)    + TEMPNO(I)*DFDX(I)
              GRADT(2)    = GRADT(2)    + TEMPNO(I)*DFDY(I)
25221       CONTINUE
          ELSE
            GRADTH(1,1) = 0.D0
            GRADTH(1,2) = 0.D0
            GRADTH(2,1) = 0.D0
            GRADTH(2,2) = 0.D0
            DO 25222 , I = 1 , NNO
              GRADT(1) = GRADT(1) + TEMPNO(I)*DFDX(I)
              GRADT(2) = GRADT(2) + TEMPNO(I)*DFDY(I)
              GRADTH(1,1) = GRADTH(1,1)+ZR(ITHETA+2*C(ISE,I)-2)*DFDX(I)
              GRADTH(1,2) = GRADTH(1,2)+ZR(ITHETA+2*C(ISE,I)-2)*DFDY(I)
              GRADTH(2,1) = GRADTH(2,1)+ZR(ITHETA+2*C(ISE,I)-1)*DFDX(I)
              GRADTH(2,2) = GRADTH(2,2)+ZR(ITHETA+2*C(ISE,I)-1)*DFDY(I)
25222       CONTINUE
            DIVTHT = GRADTH(1,1) + GRADTH(2,2)
          ENDIF
C
C 2.5.2.3. ==>
C EN 2D-AXI, MODIFICATION DU POIDS ET TERME
C COMPLEMENTAIRE SUR LA DIVERGENCE EN THETAR/R
C LES POINTS DE GAUSS ETANT TOUJOURS STRICTEMENT INTERIEURS
C A L'ELEMENT, R NE PEUT PAS ETRE NUL, DONC ON PEUT DIVISER PAR R.
C
          IF ( AXI ) THEN
            R = 0.D0
            IF ( THTNUL ) THEN
              DO 2526 , I = 1 , NNO
                R      = R      +  COORSE(2*I-1)*ZR(IVF+K+I-1)
 2526         CONTINUE
            ELSE
              THETAR = 0.D0
              DO 2527 , I = 1 , NNO
                R      = R      +  COORSE(2*I-1)*ZR(IVF+K+I-1)
                THETAR = THETAR + ZR(ITHETA+2*C(ISE,I)-2)*ZR(IVF+K+I-1)
 2527         CONTINUE
              DIVTHT = DIVTHT + THETAR / R
            ENDIF
            POIDS = POIDS*R
          ENDIF
C
C 2.5.2.4. ==> CALCUL DES TERMES DE FLUX DE TEMPERATURE, FLUGLO, OU DE
C              DERIVEE DE TEMPERATURE, FLUGLD.
C
C FLUX GLOBAL : LAMBDA.GRAD(T) --> FLUGLO
C               LAMBDA.GRAD(DLAGT) --> FLUGLD
C
          IF ( .NOT.GLOBAL .AND. ANISO ) THEN
C
            POINT(1)=0.D0
            POINT(2)=0.D0
            DO 2528 , NUNO = 1 , NNO
              POINT(1)= POINT(1)+ZR(IVF+K+NUNO-1)*COORSE(2*(NUNO-1)+1)
              POINT(2)= POINT(2)+ZR(IVF+K+NUNO-1)*COORSE(2*(NUNO-1)+2)
 2528       CONTINUE
C
            XU = ORIG(1) - POINT(1)
            YU = ORIG(2) - POINT(2)
            XNORM = SQRT( XU**2 + YU**2 )
            XU = XU / XNORM
            YU = YU / XNORM
            P(1,1) =  XU
            P(2,1) =  YU
            P(1,2) = -YU
            P(2,2) =  XU
C
          ENDIF
C
          IF ( .NOT.ANISO ) THEN
            IF ( .NOT.THTNUL ) THEN
              FLUGLO(1) = LAMBDA*GRADT(1)
              FLUGLO(2) = LAMBDA*GRADT(2)
            ENDIF
            IF ( TRANSI ) THEN
              FLUGLD(1) = LAMBDA*GRADDT(1)
              FLUGLD(2) = LAMBDA*GRADDT(2)
            ENDIF
          ELSE
          IF ( .NOT.THTNUL ) THEN
              FLULOC(1) = P(1,1)*GRADT(1) + P(2,1)*GRADT(2)
              FLULOC(2) = P(1,2)*GRADT(1) + P(2,2)*GRADT(2)
              FLULOC(1) = LAMBOR(1)*FLULOC(1)
              FLULOC(2) = LAMBOR(2)*FLULOC(2)
              FLUGLO(1) = P(1,1)*FLULOC(1) + P(1,2)*FLULOC(2)
              FLUGLO(2) = P(2,1)*FLULOC(1) + P(2,2)*FLULOC(2)
            ENDIF
            IF ( TRANSI ) THEN
              FLULOC(1) = P(1,1)*GRADDT(1) + P(2,1)*GRADDT(2)
              FLULOC(2) = P(1,2)*GRADDT(1) + P(2,2)*GRADDT(2)
              FLULOC(1) = LAMBOR(1)*FLULOC(1)
              FLULOC(2) = LAMBOR(2)*FLULOC(2)
              FLUGLD(1) = P(1,1)*FLULOC(1) + P(1,2)*FLULOC(2)
              FLUGLD(2) = P(2,1)*FLULOC(1) + P(2,2)*FLULOC(2)
            ENDIF
          ENDIF
C
C PRODUIT CONTRACTE : FLUX.GRADIENT(THETA)
C
          IF ( .NOT.THTNUL ) THEN
            FLUGRM(1) = FLUGLO(1)*GRADTH(1,1) + FLUGLO(2)*GRADTH(2,1)
            FLUGRM(2) = FLUGLO(2)*GRADTH(1,2) + FLUGLO(2)*GRADTH(2,2)
          ENDIF
C
C 2.5.2.5. ==> CONTRIBUTION AU SECOND MEMBRE :
C      - (1-THTIMP) LAMBDA ( GRAD(DLAGTE-) . GRAD(T*) )
C      + LAMBDA ( GRAD(T).GRAD(THETA) ) . GRAD(T*)
C      + LAMBDA(GRAD(T)) . ( GRAD(T*).GRAD(THETA) )
C      - DIV(THETA) . ( LAMBDA(GRAD(T)).GRAD(T*) )
C
C         . TERMES SPECIFIQUES DU TRANSITOIRE :
C
          IF ( TRANSI ) THEN
C
          DO 25251 , I = 1 , NNO
            VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS * (
     >            - UNMTHE*(FLUGLD(1)*DFDX(I) + FLUGLD(2)*DFDY(I))
     >            )
25251     CONTINUE
C
          ENDIF
C
C         . TERMES APPARAISSANT SI THETA N'EST PAS NUL
C
          IF ( .NOT.THTNUL ) THEN
C
          DO 25252 , I = 1 , NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS * (
     >              FLUGRM(1)*DFDX(I) + FLUGRM(2)*DFDY(I)
     >            + FLUGLO(1)*(DFDX(I)*GRADTH(1,1)+DFDY(I)*GRADTH(1,2))
     >            + FLUGLO(2)*(DFDX(I)*GRADTH(2,1)+DFDY(I)*GRADTH(2,2))
     >            - DIVTHT*(FLUGLO(1)*DFDX(I) + FLUGLO(2)*DFDY(I))
     >            )
25252     CONTINUE
C
          ENDIF
C
 2520     CONTINUE
C
C ------------ TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------
C
          IF ( TRANSI ) THEN
C
          IPOIDS=IFF   +(NPG1+NPG2)*(1+3*NNO)
          IVF   =IPOIDS+NPG3
          IDFDE =IVF   +NPG3*NNO
          IDFDK =IDFDE +NPG3*NNO
C
          DO 2530 , KP = 1 , NPG3
C
          K = (KP-1)*NNO
C
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     >                  COORSE,DFDX,DFDY,POIDS )
C
C 2.5.3.1. ==>  CALCUL DES TEMPERATURES AUX DEUX INSTANTS,
C              ET DE LA DERIVEE DE T A L'INSTANT PRECEDENT
C ET DE LA DIVERGENCE DE THETA AU POINT DE GAUSS
C   DIVTHT = D THETA X / DX  +  D THETA Y / DY
C         = D THETA R / DR  +  D THETA Z / DZ  +  THETA R / R
C              SI LE CHAMP THETA EST NUL, IL EST INUTILE DE CALCULER
C              LA DIVERGENCE DE THETA
C
          TMO    = 0.D0
          TPL    = 0.D0
          DLAGTG = 0.D0
          IF ( THTNUL ) THEN
            DO 25311 , I = 1 , NNO
              R8AUX = ZR(IVF+K+I-1)
              TMO    = TMO    + ZR(ITEMPM+C(ISE,I)-1)*R8AUX
              TPL    = TPL    + ZR(ITEMPP+C(ISE,I)-1)*R8AUX
              DLAGTG = DLAGTG + ZR(IDLAGT+C(ISE,I)-1)*R8AUX
25311       CONTINUE
          ELSE
            DIVTHT  = 0.D0
            DO 25312 , I = 1 , NNO
              R8AUX = ZR(IVF+K+I-1)
              TMO    = TMO    + ZR(ITEMPM+C(ISE,I)-1)*R8AUX
              TPL    = TPL    + ZR(ITEMPP+C(ISE,I)-1)*R8AUX
              DLAGTG = DLAGTG + ZR(IDLAGT+C(ISE,I)-1)*R8AUX
              DIVTHT = DIVTHT + ZR(ITHETA+2*C(ISE,I)-2)*DFDX(I)
     >                        + ZR(ITHETA+2*C(ISE,I)-1)*DFDY(I)
25312       CONTINUE
          ENDIF
C
C 2.5.3.2. ==>
C EN 2D-AXI, MODIFICATION DU POIDS ET TERME
C COMPLEMENTAIRE SUR LA DIVERGENCE EN THETAR/R
C LES POINTS DE GAUSS ETANT TOUJOURS STRICTEMENT INTERIEURS
C A L'ELEMENT, R NE PEUT PAS ETRE NUL, DONC ON PEUT DIVISER PAR R.
C
          IF ( AXI ) THEN
            R = 0.D0
            IF ( THTNUL ) THEN
              DO 2532 , I = 1 , NNO
                R      = R      +  COORSE(2*I-1)*ZR(IVF+K+I-1)
 2532         CONTINUE
            ELSE
              THETAR = 0.D0
              DO 2533 , I = 1 , NNO
                R      = R      +  COORSE(2*I-1)*ZR(IVF+K+I-1)
                THETAR = THETAR + ZR(ITHETA+2*C(ISE,I)-2)*ZR(IVF+K+I-1)
 2533         CONTINUE
              DIVTHT = DIVTHT + THETAR / R
            ENDIF
            POIDS = POIDS*R
          ENDIF
C
C 2.5.3.3. ==> CONTRIBUTION AU SECOND MEMBRE :
C        ( RHO . CP / DELTAT ) (DLAGTE-) . T*
C      - RHO . CP . DIVTHT . ((T+)-(T-))/DELTAT . T*
C
            R8AUX = CP/DELTAT*DLAGTG*POIDS
C
CCDIR$ IVDEP
            DO 25331 , I = 1 , NNO
              VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + R8AUX * ZR(IVF+K+I-1)
25331       CONTINUE
C
C         . TERMES APPARAISSANT SI THETA N'EST PAS NUL
C
          IF ( .NOT.THTNUL ) THEN
C
            R8AUX = DIVTHT*CP/DELTAT*(TPL-TMO)*POIDS
C
CCDIR$ IVDEP
            DO 25332 , I = 1 , NNO
              VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + R8AUX * ZR(IVF+K+I-1)
25332       CONTINUE
C
          ENDIF
C
 2530     CONTINUE
C
        ENDIF
C
  252   CONTINUE
C
C BASCULE DANS LE VECTEUR GENERAL
C
        DO 253 , I = 1 , NNOP2
          ZR(IVECTT-1+I) = VECTT(I)
  253   CONTINUE
C
      ENDIF
C
      ENDIF
C
      END
