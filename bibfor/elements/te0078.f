      SUBROUTINE TE0078 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2008   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_THER_EVOL'
C                          OPTION : 'CHAR_SENS_EVOL'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       25/01/02 (OB): MODIFICATIONS POUR INSERER LES ARGUMENTS OPTION
C        NELS PERMETTANT D'UTILISER CETTE ROUTINE POUR CALCULER LA
C        SENSIBILITE PAR RAPPORT AUX CARACTERISTIQUES MATERIAU.
C        + MODIFS FORMELLES: IMPLICIT NONE, IDENTATION...
C       08/03/02 (OB): CORRECTION BUG EN STATIONNAIRE SI RHO_CP ABSENT
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

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

      INTEGER       NBRES
      PARAMETER     (NBRES=3)
      CHARACTER*2   CODRET(NBRES)
      CHARACTER*8   NOMRES(NBRES),ELREFE
      CHARACTER*16  PHENOM,OPTION,NOMTE,PHESEN,VALK(2)
      REAL*8        VALRES(NBRES),DFDX(9),DFDY(9),POIDS,R,TPG,THETA,CP,
     &              ORIG(2),LAMBOR(2),LAMBDA,FLUGLO(2),FLULOC(2),P(2,2),
     &              POINT(2),COORSE(18),VECTT(9),PREC,R8PREM,CPS,LAMBS,
     &              LAMBOS(2),TRACE,DTEMPX,DTEMPY,DTEMMX,DTEMMY,TEMS,
     &              FLUGLS(2),FLULOS(2),DELTAT,ALPHA,R8DGRD,DTPGDX,
     &              DTPGDY,XNORM,XU,YU
      INTEGER       NDIM,NNO,NNOS,KP,NPG,I,J,K,ITEMPS,IVECTT,JGANO,
     &              NNOP2,C(6,9),ISE,NSE,NUNO,IPOIDS,IVF,IDFDE,
     &              IGEOM,IMATE,IMATSE,IVAPRI,IVAPRM,TETYPS,ITEMP,
     &              ICAMAS,IRET,NPG2,IPOID2,IVF2,IDFDE2
      LOGICAL       ANISO,GLOBAL,LSENS,LSTAT,LTEATT

      PREC = R8PREM()

C====
C 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
C====
C
      CALL ELREF1(ELREFE)
      IF (NOMTE(5:7).EQ.'QL9') ELREFE='QU4'
      IF (NOMTE(5:7).EQ.'TL6') ELREFE='TR3'
C
      CALL ELREF4(ELREFE,'NOEU',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,IDFDE2,
     &            JGANO)
      CALL ELREF4(ELREFE,'MASS',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &            JGANO)

C====
C 1.2 PREALABLES LIES AUX CALCULS DE SENSIBILITE
C====
C CALCUL DE SENSIBILITE PART I
      LSTAT = .FALSE.
      IF (OPTION(6:9).EQ.'SENS') THEN
        LSENS = .TRUE.
        CALL JEVECH('PMATSEN','L',IMATSE)
        CALL JEVECH('PVAPRIN','L',IVAPRI)
        CALL TECACH('ONN','PVAPRMO',1,IVAPRM,IRET)
C DANS LE CAS DES DERIVEES MATERIAUX:
C L'ABSENCE DE CE CHAMP DETERMINE LE CRITERE STATIONNAIRE OU PAS
C ON "TRUANDE" ALORS DE MANIERE PEU OPTIMALE MAIS FACILE A MAINTE
C NIR: CP ET/OU CPS SONT ANNULES ET ON CREE UN CHAMP T- BIDON.
        IF (IVAPRM.EQ.0) THEN
          LSTAT = .TRUE.
          IVAPRM = IVAPRI
        ENDIF
      ELSE
        LSENS = .FALSE.
      ENDIF

C====
C 1.3 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
C====
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPER','L',ITEMP )
      CALL JEVECH('PVECTTR','E',IVECTT)
      DELTAT = ZR(ITEMPS+1)
      THETA  = ZR(ITEMPS+2)
      CALL RCCOMA ( ZI(IMATE), 'THER', PHENOM, CODRET )

C CALCUL DE SENSIBILITE PART II. TEST DE COHERENCE PHENOM STD/
C PHENOM MAT DERIVEE
      IF (LSENS) THEN
        CALL RCCOMA ( ZI(IMATSE), 'THER', PHESEN, CODRET )
        IF (PHESEN.NE.PHENOM) THEN
          VALK(1)=PHESEN
          VALK(2)=PHENOM
          CALL U2MESK('F','ELEMENTS_38',2,VALK)
        ENDIF
      ENDIF
C====
C 1.4 PREALABLES LIES A LA RECUPERATION DES DONNEES MATERIAUX
C====
      IF ( PHENOM .EQ. 'THER') THEN
        NOMRES(1) = 'LAMBDA'
        NOMRES(2) = 'RHO_CP'
        ANISO = .FALSE.
        IF (LSTAT) THEN
C EN SENSIBILITE, CAS STATIONNAIRE OU PREMIER PAS DE TEMPS: ON A
C JUSTE BESOIN DE LAMBDA
          CALL RCVALA(ZI(IMATE),' ',PHENOM,1,'INST',ZR(ITEMPS),1,
     &                NOMRES,VALRES,CODRET,'FM')
          LAMBDA = VALRES(1)
          CP     = 0.D0
        ELSE
C IDEM CAS TRANSITOIRE A PARTIR DU SECOND PAS DE TEMPS
          CALL RCVALA(ZI(IMATE),' ',PHENOM,1,'INST',ZR(ITEMPS),2,
     &                NOMRES,VALRES,CODRET,'FM')
          LAMBDA = VALRES(1)
          CP     = VALRES(2)
        ENDIF

C CALCUL DE SENSIBILITE PART III (ISOTROPE)
        IF (LSENS) THEN
          IF (LSTAT) THEN
            CALL RCVALA(ZI(IMATSE),' ',PHENOM,1,'INST',ZR(ITEMPS),1,
     &                  NOMRES,VALRES,CODRET,'FM')
            CPS   = 0.D0
            LAMBS = VALRES(1)
          ELSE
            CALL RCVALA(ZI(IMATSE),' ',PHENOM,1,'INST',ZR(ITEMPS),2,
     &                  NOMRES,VALRES,CODRET,'FM')
            CPS   = VALRES(2)
            LAMBS = VALRES(1)
          ENDIF
          IF ((ABS(CPS).LT.PREC).AND.(ABS(LAMBS).LT.PREC)) THEN
C PAS DE TERME DE SENSIBILITE SUPPLEMENTAIRE, CALCUL INSENSIBLE
            TETYPS = 0
          ELSE IF (ABS(CPS).LT.PREC) THEN
C SENSIBILITE PAR RAPPORT A LAMBDA
            TETYPS = 1
          ELSE IF (ABS(LAMBS).LT.PREC) THEN
C SENSIBILITE PAR RAPPORT A CP
            TETYPS = 2
          ENDIF
        ELSE
C CALCUL STD
          TETYPS = 0
        ENDIF
      ELSEIF ( PHENOM .EQ. 'THER_ORTH') THEN
        NOMRES(1) = 'LAMBDA_L'
        NOMRES(2) = 'LAMBDA_T'
        NOMRES(3) = 'RHO_CP'
        ANISO     = .TRUE.
        IF (LSTAT) THEN
          CALL RCVALA(ZI(IMATE),' ',PHENOM,1,'INST',ZR(ITEMPS),2,
     &                NOMRES,VALRES,CODRET,'FM')
          LAMBOR(1) = VALRES(1)
          LAMBOR(2) = VALRES(2)
          CP        = 0.D0
        ELSE
          CALL RCVALA(ZI(IMATE),' ',PHENOM,1,'INST',ZR(ITEMPS),3,
     &                NOMRES,VALRES,CODRET,'FM')
          LAMBOR(1) = VALRES(1)
          LAMBOR(2) = VALRES(2)
          CP        = VALRES(3)
        ENDIF
C CALCUL DE SENSIBILITE PART III BIS (ANISOTROPE)
        IF (LSENS) THEN
          IF (LSTAT) THEN
            CALL RCVALA(ZI(IMATSE),' ',PHENOM,1,'INST',ZR(ITEMPS),2,
     &                  NOMRES,VALRES,CODRET,'FM')
            LAMBOS(1) = VALRES(1)
            LAMBOS(2) = VALRES(2)
            CPS       = 0.D0
          ELSE
            CALL RCVALA(ZI(IMATSE),' ',PHENOM,1,'INST',ZR(ITEMPS),3,
     &                  NOMRES,VALRES,CODRET,'FM')
            LAMBOS(1) = VALRES(1)
            LAMBOS(2) = VALRES(2)
            CPS       = VALRES(3)
          ENDIF
          TRACE = LAMBOS(1) + LAMBOS(2)
          IF ((ABS(CPS).LT.PREC).AND.(ABS(TRACE).LT.PREC)) THEN
            TETYPS = 0
          ELSE IF (ABS(CPS).LT.PREC) THEN
            TETYPS = 1
          ELSE IF (ABS(TRACE).LT.PREC) THEN
            TETYPS = 2
          ENDIF
        ELSE
          TETYPS = 0
        ENDIF
      ELSE
        CALL U2MESS('F','ELEMENTS2_63')
      ENDIF
C====
C 1.5 PREALABLES LIES A L'ANISOTROPIE
C====
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
          GLOBAL  = .FALSE.
          ORIG(1) = ZR(ICAMAS+4)
          ORIG(2) = ZR(ICAMAS+5)
        ENDIF
       ENDIF
C====
C 2. CALCULS TERMES DE RIGIDITE ET DE MASSE (STD ET/OU SENSIBLE)
C    POUR LES ELEMENTS NON LUMPES
C====

      IF (NOMTE(6:6).NE.'L') THEN

        DO 101 KP=1,NPG
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
          R      = 0.D0
          TPG    = 0.D0
          DTPGDX = 0.D0
          DTPGDY = 0.D0
          DO 102 I=1,NNO
C CALCUL DE T- (OU (DT/DS)- EN SENSI) ET DE SON GRADIENT
            R      = R      + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
            TPG    = TPG    + ZR(ITEMP+I-1)    *ZR(IVF+K+I-1)
            DTPGDX = DTPGDX + ZR(ITEMP+I-1)    *DFDX(I)
            DTPGDY = DTPGDY + ZR(ITEMP+I-1)    *DFDY(I)
102       CONTINUE
          IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R

C CALCUL DE SENSIBILITE PART IV
          IF (TETYPS.EQ.1) THEN
            DTEMPX = 0.D0
            DTEMPY = 0.D0
            DTEMMX = 0.D0
            DTEMMY = 0.D0
            DO 41 I=1,NNO
C CALCUL DE GRAD(T+) ET DE GRAD(T-) POUR TERME DE RIGIDITE
              DTEMPX = DTEMPX + ZR(IVAPRI+I-1) * DFDX(I)
              DTEMPY = DTEMPY + ZR(IVAPRI+I-1) * DFDY(I)
              DTEMMX = DTEMMX + ZR(IVAPRM+I-1) * DFDX(I)
              DTEMMY = DTEMMY + ZR(IVAPRM+I-1) * DFDY(I)
41          CONTINUE
          ELSE IF (TETYPS.EQ.2) THEN
            TEMS = 0.D0
            DO 42 I=1,NNO
C CALCUL DE (T- - T+) POUR TERME DE MASSE
              TEMS=TEMS+(ZR(IVAPRM+I-1)-ZR(IVAPRI+I-1))*ZR(IVF+K+I-1)
42          CONTINUE
          ENDIF

          IF (.NOT.ANISO) THEN
            FLUGLO(1) = LAMBDA*DTPGDX
            FLUGLO(2) = LAMBDA*DTPGDY
C CALCUL DE SENSIBILITE PART V (SENSIBILITE / LAMBDA EN ISOTROPE)
            IF (TETYPS.EQ.1) THEN
              FLUGLS(1)=LAMBS*(THETA*DTEMPX+(1.D0-THETA)*DTEMMX)
              FLUGLS(2)=LAMBS*(THETA*DTEMPY+(1.D0-THETA)*DTEMMY)
            ENDIF
          ELSE
            IF (.NOT.GLOBAL) THEN
              POINT(1)=0.D0
              POINT(2)=0.D0
              DO 104 NUNO=1,NNO
                POINT(1) = POINT(1) +
     &                     ZR(IVF+K+NUNO-1)*ZR(IGEOM+2*NUNO-2)
                POINT(2) = POINT(2) +
     &                     ZR(IVF+K+NUNO-1)*ZR(IGEOM+2*NUNO-1)
 104          CONTINUE
              XU = ORIG(1) - POINT(1)
              YU = ORIG(2) - POINT(2)
              XNORM = SQRT( XU**2 + YU**2 )
              XU = XU / XNORM
              YU = YU / XNORM
              P(1,1) =  XU
              P(2,1) =  YU
              P(1,2) = -YU
              P(2,2) =  XU
            ENDIF
            FLUGLO(1) = DTPGDX
            FLUGLO(2) = DTPGDY
            FLULOC(1) = P(1,1)*DTPGDX + P(2,1)*DTPGDY
            FLULOC(2) = P(1,2)*DTPGDX + P(2,2)*DTPGDY
            FLULOC(1) = LAMBOR(1)*FLULOC(1)
            FLULOC(2) = LAMBOR(2)*FLULOC(2)
            FLUGLO(1) = P(1,1)*FLULOC(1) + P(1,2)*FLULOC(2)
            FLUGLO(2) = P(2,1)*FLULOC(1) + P(2,2)*FLULOC(2)

C CALCUL DE SENSIBILITE PART V BIS(SENSIBILITE / LAMBDA EN ANISOTROPE)
            IF (TETYPS.EQ.1) THEN
              FLUGLS(1) = THETA*DTEMPX+(1.D0-THETA)*DTEMMX
              FLUGLS(2) = THETA*DTEMPY+(1.D0-THETA)*DTEMMY
              FLULOS(1) = P(1,1)*FLUGLS(1) + P(2,1)*FLUGLS(2)
              FLULOS(2) = P(1,2)*FLUGLS(1) + P(2,2)*FLUGLS(2)
              FLULOS(1) = LAMBOS(1)*FLULOS(1)
              FLULOS(2) = LAMBOS(2)*FLULOS(2)
              FLUGLS(1) = P(1,1)*FLULOS(1) + P(1,2)*FLULOS(2)
              FLUGLS(2) = P(2,1)*FLULOS(1) + P(2,2)*FLULOS(2)
            ENDIF
          ENDIF
          DO 103 I=1,NNO
            K=(KP-1)*NNO
            ZR(IVECTT+I-1) = ZR(IVECTT+I-1) + POIDS *
     &            ( CP/DELTAT*ZR(IVF+K+I-1)*TPG - (1.0D0-THETA)*
     &            ( FLUGLO(1)*DFDX(I) + FLUGLO(2)*DFDY(I) ) )
103       CONTINUE
C CALCUL DE SENSIBILITE PART VI (SENSIBILITE / LAMBDA).
          IF (TETYPS.EQ.1) THEN
            DO 51 I=1,NNO
              ZR(IVECTT+I-1) = ZR(IVECTT+I-1) - POIDS*(DFDX(I)*
     &                         FLUGLS(1)+DFDY(I)*FLUGLS(2))
51          CONTINUE
C SENSIBILITE / CP
          ELSE IF (TETYPS.EQ.2) THEN
            DO 52 I=1,NNO
              K=(KP-1)*NNO
              ZR(IVECTT+I-1) = ZR(IVECTT+I-1) + POIDS*CPS/DELTAT*
     &                         ZR(IVF+K+I-1)*TEMS
52          CONTINUE
          ENDIF
101     CONTINUE

      ELSE

C====
C 3.1 CALCULS TERMES DE RIGIDITE (STD ET/OU SENSIBLE)
C    POUR LES ELEMENTS LUMPES
C====

C  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES
        CALL CONNEC ( NOMTE, NSE, NNOP2, C )
        DO 10 I=1,NNOP2
          VECTT(I)=0.D0
10      CONTINUE

C ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------
C BOUCLE SUR LES SOUS-ELEMENTS

        DO 200 ISE=1,NSE
          DO 205 I=1,NNO
            DO 205 J=1,2
              COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
205       CONTINUE
          DO 201 KP=1,NPG
            K=(KP-1)*NNO
            CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,COORSE,DFDX,DFDY,POIDS )
            R      = 0.D0
            TPG    = 0.D0
            DTPGDX = 0.D0
            DTPGDY = 0.D0
            DO 202 I=1,NNO
C CALCUL DE T- ET DE GRAD(T-)(OU DT/DS- ET SON GRADIENT EN SENSIBILITE)
              R      = R      + COORSE(2*(I-1)+1)    * ZR(IVF+K+I-1)
              DTPGDX = DTPGDX + ZR(ITEMP-1+C(ISE,I)) * DFDX(I)
              DTPGDY = DTPGDY + ZR(ITEMP-1+C(ISE,I)) * DFDY(I)
202         CONTINUE
            IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R

C CALCUL DE SENSIBILITE PART VII (SENSIBILITE / LAMBDA)
            IF (TETYPS.EQ.1) THEN
              DTEMPX = 0.D0
              DTEMPY = 0.D0
              DTEMMX = 0.D0
              DTEMMY = 0.D0
              DO 231 I=1,NNO
C CALCUL DE GRAD(T+) ET DE GRAD(T-)
                DTEMPX = DTEMPX + ZR(IVAPRI-1+C(ISE,I)) * DFDX(I)
                DTEMPY = DTEMPY + ZR(IVAPRI-1+C(ISE,I)) * DFDY(I)
                DTEMMX = DTEMMX + ZR(IVAPRM-1+C(ISE,I)) * DFDX(I)
                DTEMMY = DTEMMY + ZR(IVAPRM-1+C(ISE,I)) * DFDY(I)
231           CONTINUE
            ENDIF
            IF ( .NOT.ANISO ) THEN
              FLUGLO(1) = LAMBDA*DTPGDX
              FLUGLO(2) = LAMBDA*DTPGDY
C CALCUL DE SENSIBILITE PART VIII (SENSIBILITE / LAMBDA EN ISOTROPE)
              IF (TETYPS.EQ.1) THEN
                FLUGLS(1)=LAMBS*(THETA*DTEMPX+(1.D0-THETA)*DTEMMX)
                FLUGLS(2)=LAMBS*(THETA*DTEMPY+(1.D0-THETA)*DTEMMY)
              ENDIF
            ELSE
              IF (.NOT.GLOBAL) THEN
                POINT(1)=0.D0
                POINT(2)=0.D0
                DO 204 NUNO=1,NNO
                  POINT(1)= POINT(1)+
     &                      ZR(IVF+K+NUNO-1)*COORSE(2*(NUNO-1)+1)
                  POINT(2)= POINT(2)+
     &                      ZR(IVF+K+NUNO-1)*COORSE(2*(NUNO-1)+2)
 204            CONTINUE
                XU = ORIG(1) - POINT(1)
                YU = ORIG(2) - POINT(2)
                XNORM = SQRT( XU**2 + YU**2 )
                XU = XU / XNORM
                YU = YU / XNORM
                P(1,1) =  XU
                P(2,1) =  YU
                P(1,2) = -YU
                P(2,2) =  XU
              ENDIF
              FLUGLO(1) = DTPGDX
              FLUGLO(2) = DTPGDY
              FLULOC(1) = P(1,1)*DTPGDX + P(2,1)*DTPGDY
              FLULOC(2) = P(1,2)*DTPGDX + P(2,2)*DTPGDY
              FLULOC(1) = LAMBOR(1)*FLULOC(1)
              FLULOC(2) = LAMBOR(2)*FLULOC(2)
              FLUGLO(1) = P(1,1)*FLULOC(1) + P(1,2)*FLULOC(2)
              FLUGLO(2) = P(2,1)*FLULOC(1) + P(2,2)*FLULOC(2)
C CALCUL DE SENSIBILITE PART VIII BIS(SENSIBILITE/LAMBDA EN ANISO)
             IF (TETYPS.EQ.1) THEN
               FLUGLS(1) = THETA*DTEMPX+(1.D0-THETA)*DTEMMX
               FLUGLS(2) = THETA*DTEMPY+(1.D0-THETA)*DTEMMY
               FLULOS(1) = P(1,1)*FLUGLS(1) + P(2,1)*FLUGLS(2)
               FLULOS(2) = P(1,2)*FLUGLS(1) + P(2,2)*FLUGLS(2)
               FLULOS(1) = LAMBOS(1)*FLULOS(1)
               FLULOS(2) = LAMBOS(2)*FLULOS(2)
               FLUGLS(1) = P(1,1)*FLULOS(1) + P(1,2)*FLULOS(2)
               FLUGLS(2) = P(2,1)*FLULOS(1) + P(2,2)*FLULOS(2)
             ENDIF
           ENDIF
           DO 203 I=1,NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS *
     &          (THETA-1.0D0)*( FLUGLO(1)*DFDX(I) + FLUGLO(2)*DFDY(I) )
203        CONTINUE
C CALCUL DE SENSIBILITE PART IX (SENSIBILITE / LAMBDA).
           IF (TETYPS.EQ.1) THEN
              DO 241 I=1,NNO
                VECTT(C(ISE,I)) = VECTT(C(ISE,I)) - POIDS*(DFDX(I)*
     &                            FLUGLS(1)+DFDY(I)*FLUGLS(2))
241           CONTINUE
            ENDIF
201       CONTINUE

C====
C 3.2 CALCULS TERMES DE MASSE (STD ET/OU SENSIBLE)
C    POUR LES ELEMENTS LUMPES
C====

          DO 305 I=1,NNO
            DO 305 J=1,2
              COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
305       CONTINUE
          DO 301 KP=1,NPG2
            K=(KP-1)*NNO
            CALL DFDM2D(NNO,KP,IPOID2,IDFDE2,COORSE,DFDX,DFDY,POIDS )
            R      = 0.D0
            TPG    = 0.D0
            DO 302 I=1,NNO
C CALCUL DE T- (OU (DT/DS)- EN SENSI)
              R      = R      + COORSE(2*(I-1)+1)    * ZR(IVF2+K+I-1)
              TPG    = TPG    + ZR(ITEMP-1+C(ISE,I)) * ZR(IVF2+K+I-1)
302         CONTINUE
C CALCUL DE SENSIBILITE PART X (SENSIBILITE / CP).
            IF (TETYPS.EQ.2) THEN
              TEMS = 0.D0
              DO 261 I=1,NNO
C CALCUL DE (T- - T+)
                TEMS=TEMS+(ZR(IVAPRM-1+C(ISE,I))-ZR(IVAPRI-1+C(ISE,I)))
     &                *ZR(IVF2+K+I-1)
261           CONTINUE
            ENDIF

            IF ( LTEATT(' ','AXIS','OUI') ) THEN
              POIDS = POIDS*R
              IF (R.EQ.0.D0) THEN
                CALL U2MESS('F','ELEMENTS3_10')
              ENDIF
            ENDIF

            DO 303 I=1,NNO
              VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS
     &                      * ( CP/DELTAT*ZR(IVF2+K+I-1)*TPG )
303         CONTINUE
C CALCUL DE SENSIBILITE PART XI (SENSIBILITE / CP)
            IF (TETYPS.EQ.2) THEN
              DO 271 I=1,NNO
                VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS*
     &                            CPS/DELTAT*ZR(IVF2+K+I-1)*TEMS
271           CONTINUE
            ENDIF

301       CONTINUE
200     CONTINUE

C MISE SOUS FORME DE VECTEUR

        DO 306 I=1,NNOP2
          ZR(IVECTT-1+I)=VECTT(I)
306     CONTINUE

      ENDIF
      END
