      SUBROUTINE TE0036(OPTION,NOMTE)
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/07/2007   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.......................................................................
C
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN CHARGEMENT EN PRESSION REPARTIE
C          SUR DES FACES D'ELEMENTS X-FEM
C          (LA PRESSION PEUT ETRE DONNEE SOUS FORME D'UNE FONCTION)
C
C          OPTIONS : 'CHAR_MECA_PRES_R'
C                    'CHAR_MECA_PRES_F'
C                    'CHAR_MECA_FR2D3D'
C                    'CHAR_MECA_FR1D2D'
C                    'CHAR_MECA_FF2D3D'
C                    'CHAR_MECA_FF1D2D'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
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

      CHARACTER*8   NOMPAR(4),NOMPA2(3),NOMA,ELREFP,ELRESE(2)
      CHARACTER*16  NOMTE,OPTION
      CHARACTER*24  COORLO,GEOMLO,COORSE
      INTEGER       JPINTT,JCNSET,JHEAVT,JLONCH,JCOORS,JLSN,JLST,K
      INTEGER       IBID,IER,NDIM,NNO,NNOP,NPG,NNOS,JGANO,KPG,KDEC,J
      INTEGER       IPOIDS,IVF,IDFDE,JDFD2,IGEOM,IPRES,ITEMPS,IRES,I
      INTEGER       DDLH,NFE,DDLC,NNOM,DDL,NIT,CPT,IT,NSE,ISE
      INTEGER       IN,INO,JCORLO,IGEOLO,IADZI,IAZK24
      INTEGER       NSEMAX(3),IFORC,IRET,IG,POS,NDIME,NDDL,JDIM
      REAL*8        Y(3),XG(4),RBID,FE(4),XE(2),LSNG,LSTG,RG,TG
      REAL*8        PRES,MATR(6561),FF(27),A(3),B(3),C(3),AB(3),AC(3)
      REAL*8        ND(3),NORME,NAB,RB1(3),RB2(3),GLOC(2),N(3),CISA
      REAL*8        AN(3),G(3),HE,POIDS,DDOT,PADIST,FORREP(3),VF
      DATA          ELRESE /'SE2','TR3'/
      DATA          NSEMAX /2,3,6/
C
      CALL JEMARQ()
C-----------------------------------------------------------------------
C     INITIALISATIONS
C-----------------------------------------------------------------------

C     ELEMENT DE REFERENCE PARENT
      CALL ELREF1(ELREFP)
      CALL ELREF4(' ','RIGI',NDIME,NNOP,IBID,IBID,IBID,IBID,IBID,IBID)
      CALL ASSERT(NDIME.EQ.1.OR.NDIME.EQ.2)

C     SOUS-ELEMENT DE REFERENCE
      CALL ELREF4(ELRESE(NDIME),'RIGI',IBID,NNO,NNOS,NPG,
     &                                          IPOIDS,IVF,IDFDE,IBID)

C     DIMENSION DE L'ESPACE
      CALL TECAEL(IADZI,IAZK24)
      NOMA=ZK24(IAZK24)
      CALL JEVEUO(NOMA//'.DIME','L',JDIM)
      NDIM=ZI(JDIM-1+6)

C     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
C     NDIM EST LA DIMENSION DU MAILLAGE
C     NDIME EST DIMENSION DE L'ELEMENT FINI
C     SUR UN ELET DE BORD, ON A :  NDIM = NDIME + 1

C     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
C     IL NE FAUT PAS APPELER XTEINI CAR IL NE GERE PAS LES ELEMENTS 
C     DE BORD   
C      CALL XTEINI(NOMTE,DDLH,NFE,IBID,IBID,IBID,IBID,IBID)
      IF (NOMTE(1:12).EQ.'MECA_XH_FACE'.OR.
     &    NOMTE.EQ.'MEPLSE2_XH'.OR.
     &    NOMTE.EQ.'MEPLSE3_XH') THEN
        DDLH=NDIM
        NFE=0
      ELSEIF (NOMTE(1:12).EQ.'MECA_XT_FACE'.OR.
     &        NOMTE.EQ.'MEPLSE2_XT'.OR.
     &        NOMTE.EQ.'MEPLSE3_XT') THEN
        DDLH=0
        NFE=4
      ELSEIF (NOMTE(1:13).EQ.'MECA_XHT_FACE'.OR.
     &        NOMTE.EQ.'MEPLSE2_XHT'.OR.
     &        NOMTE.EQ.'MEPLSE3_XHT') THEN
        DDLH=NDIM
        NFE=4
      ELSE
        CALL U2MESS('F','ELEMENTS4_59')
      ENDIF

C-----------------------------------------------------------------------
C     RECUPERATION DES ENTREES / SORTIE
C-----------------------------------------------------------------------

      CALL JEVECH('PGEOMER','L',IGEOM)

      IF (OPTION.EQ.'CHAR_MECA_PRES_R') THEN

C       SI LA PRESSION N'EST CONNUE SUR AUCUN NOEUD, ON LA PREND=0.
        CALL JEVECD('PPRESSR',IPRES,0.D0)

      ELSEIF (OPTION.EQ.'CHAR_MECA_PRES_F') THEN

        CALL JEVECH('PPRESSF','L',IPRES)
        CALL JEVECH('PTEMPSR','L',ITEMPS)

      ELSEIF (OPTION.EQ.'CHAR_MECA_FR2D3D'.OR.
     &        OPTION.EQ.'CHAR_MECA_FR1D2D') THEN

        CALL TECACH('ONN','PNFORCER',1,IFORC,IRET)
        IF ( IFORC .NE. 0 ) THEN
          CALL JEVECH('PNFORCER','L',IFORC)
        ELSE
          IF (NDIM.EQ.3) CALL JEVECH('PFR2D3D', 'L',IFORC)
          IF (NDIM.EQ.2) CALL JEVECH('PFR1D2D', 'L',IFORC)
        ENDIF

      ELSEIF (OPTION.EQ.'CHAR_MECA_FF2D3D'.OR.
     &        OPTION.EQ.'CHAR_MECA_FF1D2D') THEN

        IF (NDIM.EQ.3) CALL JEVECH('PFF2D3D','L',IFORC)
        IF (NDIM.EQ.2) CALL JEVECH('PFF1D2D','L',IFORC)
        CALL JEVECH('PTEMPSR','L',ITEMPS)

      ENDIF

C     PARAMETRES PROPRES A X-FEM
      CALL JEVECH('PLSN','L',JLSN)
      CALL JEVECH('PLST','L',JLST)
      CALL JEVECH('PPINTTO','L',JPINTT)
      CALL JEVECH('PCNSETO','L',JCNSET)
      CALL JEVECH('PHEAVTO','L',JHEAVT)
      CALL JEVECH('PLONCHA','L',JLONCH)

      CALL JEVECH('PVECTUR','E',IRES)

C     RECUPERATION DE LA SUBDIVISION L'ELEMENT PARENT EN NIT TRI
      NIT=ZI(JLONCH-1+1)

      CPT=0
C     BOUCLE SUR LES NIT TRI
      DO 100 IT=1,NIT

C       RECUPERATION DU DECOUPAGE EN NSE SOUS-ELEMENTS
        NSE=ZI(JLONCH-1+1+IT)

C       BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
        DO 110 ISE=1,NSE

          CPT=CPT+1

C         COORD DU SOUS-ELT EN QUESTION
          COORSE='&&TE0036.COORSE'
          CALL WKVECT(COORSE,'V V R',NDIM*(NDIM+1),JCOORS)

C         BOUCLE SUR LES SOMMETS DU SOUS-TRIA (DU SOUS-SEG)
          DO 111 IN=1,NNO
            INO=ZI(JCNSET-1+(NDIME+1)*(CPT-1)+IN)
            DO 112 J=1,NDIM 
              IF (INO.LT.1000) THEN
                ZR(JCOORS-1+NDIM*(IN-1)+J)=ZR(IGEOM-1+NDIM*(INO-1)+J)
              ELSE
                ZR(JCOORS-1+NDIM*(IN-1)+J)=
     &                               ZR(JPINTT-1+NDIM*(INO-1000-1)+J)
              ENDIF
 112        CONTINUE
 111      CONTINUE

C         ON RENOMME LES SOMMETS DU SOUS-ELEMENT
          CALL LCINVN(3,0.D0,A)
          CALL LCINVN(3,0.D0,B)
          CALL LCINVN(3,0.D0,AB)
          DO 113 J=1,NDIM
            A(J)=ZR(JCOORS-1+NDIM*(1-1)+J)
            B(J)=ZR(JCOORS-1+NDIM*(2-1)+J)
            AB(J)=B(J)-A(J)
            IF (NDIM.EQ.3) C(J)=ZR(JCOORS-1+NDIM*(3-1)+J)
            IF (NDIM.EQ.3) AC(J)=C(J)-A(J)
 113      CONTINUE 

          IF (NDIME.EQ.2) THEN
C           CREATION DU REPERE LOCAL 2D : (AB,Y)
            CALL PROVEC(AB,AC,ND)
            CALL NORMEV(ND,NORME)
            CALL NORMEV(AB,NAB)
            CALL PROVEC(ND,AB,Y)
          ELSEIF (NDIME.EQ.1) THEN
C           CREATION DU REPERE LOCAL 1D : AB/NAB
            CALL NORMEV(AB,NAB)
            CALL LCINVN(3,0.D0,ND) 
            ND(1) = AB(2)
            ND(2) = -AB(1)
          ENDIF

C         COORDONNÉES DES SOMMETS DE LA FACETTE DANS LE REPÈRE LOCAL
          COORLO='&&TE0036.COORLO'
          CALL WKVECT(COORLO,'V V R',NNO*NDIME,JCORLO)
          IF (NDIME.EQ.2) THEN
            ZR(JCORLO-1+1)=0.D0
            ZR(JCORLO-1+2)=0.D0
            ZR(JCORLO-1+3)=NAB
            ZR(JCORLO-1+4)=0.D0
            ZR(JCORLO-1+5)=DDOT(3,AC,1,AB,1)
            ZR(JCORLO-1+6)=DDOT(3,AC,1,Y ,1)
          ELSEIF (NDIME.EQ.1) THEN
            ZR(JCORLO-1+1)=0.D0
            ZR(JCORLO-1+2)=NAB
          ENDIF

C         COORDONNÉES DES NOEUDS DE L'ELREFP DANS LE REPÈRE LOCAL 
          GEOMLO='&&TE0036.GEOMLO'
          CALL WKVECT(GEOMLO,'V V R',NNOP*NDIME,IGEOLO)
          DO 114 INO=1,NNOP
            DO 115 J=1,NDIM
              N(J)=ZR(IGEOM-1+NDIM*(INO-1)+J)
              AN(J)=N(J)-A(J)
 115        CONTINUE
            ZR(IGEOLO-1+NDIME*(INO-1)+1)=DDOT(NDIM,AN,1,AB,1)
            IF (NDIME.EQ.2) 
     &        ZR(IGEOLO-1+NDIME*(INO-1)+2)=DDOT(NDIM,AN,1,Y ,1)
 114      CONTINUE

C         FONCTION HEAVYSIDE CSTE SUR LE SS-ELT
          HE=ZI(JHEAVT-1+NSEMAX(NDIME)*(IT-1)+ISE)

C-----------------------------------------------------------------------
C         BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
C-----------------------------------------------------------------------

          DO 200 KPG=1,NPG

C           CALCUL DU POIDS : POIDS = POIDS DE GAUSS * DET(J)
            IF     (NDIME.EQ.2) THEN
              CALL DFDM2D(NNO,KPG,IPOIDS,IDFDE,ZR(JCORLO),RB1,RB2,POIDS)
            ELSEIF (NDIME.EQ.1) THEN
               POIDS = ZR(IPOIDS-1+KPG) * NAB/2.D0
            ENDIF

C           COORDONNÉES RÉELLES LOCALES DU POINT DE GAUSS
            CALL LCINVN(NDIME,0.D0,GLOC)
            DO 210 J=1,NNO
              VF=ZR(IVF-1+NNO*(KPG-1)+J)
              DO 211 K=1,NDIME
                GLOC(K)=GLOC(K)+VF*ZR(JCORLO-1+NDIME*(J-1)+K)
 211          CONTINUE
 210        CONTINUE
                         
C           JUSTE POUR CALCULER LES FF AUX NOEUDS DE L'ELREFP
            CALL REEREF(ELREFP,NNOP,IGEOLO,GLOC,RBID,.FALSE.,NDIME,RBID,
     &         IBID,IBID,IBID,RBID,RBID,'NON',XE,FF,RBID,RBID,RBID,RBID)

C           COORDONNES REELLES DU POINT DE GAUSS              
            CALL LCINVN(4,0.D0,XG)
            DO 220 I=1,NDIM
              DO 221 IN=1,NNO
                XG(I) = XG(I) + ZR(IVF-1+NNO*(KPG-1)+IN)
     &                        * ZR(JCOORS-1+NDIM*(IN-1)+I)
 221          CONTINUE
 220        CONTINUE

C           2EME METHODE POUR CALCULER LES COORDONNÉES RÉELLES 
C           DU POINT DE GAUSS
C            G(1)=A(1)+AB(1)*GLOC(1)+Y(1)*GLOC(2)
C            G(2)=A(2)+AB(2)*GLOC(1)+Y(2)*GLOC(2)
C            G(3)=A(3)+AB(3)*GLOC(1)+Y(3)*GLOC(2)


C           CALCUL DES FONCTIONS D'ENRICHISSEMENT
C           -------------------------------------

            IF (NFE.GT.0) THEN
C             LEVEL SETS AU POINT DE GAUSS
              LSNG = 0.D0
              LSTG = 0.D0
              DO 230 INO=1,NNOP
                LSNG = LSNG + ZR(JLSN-1+INO) * FF(INO)
                LSTG = LSTG + ZR(JLST-1+INO) * FF(INO)
 230          CONTINUE

C             COORDONNÉES POLAIRES DU POINT
              RG=SQRT(LSNG**2+LSTG**2)
              TG = HE * ABS(ATAN2(LSNG,LSTG))
C
C             FONCTIONS D'ENRICHISSEMENT
              FE(1)=SQRT(RG)*SIN(TG/2.D0)
              FE(2)=SQRT(RG)*COS(TG/2.D0)
              FE(3)=SQRT(RG)*SIN(TG/2.D0)*SIN(TG)
              FE(4)=SQRT(RG)*COS(TG/2.D0)*SIN(TG)
            ENDIF

C           CALCUL DES FORCES REPARTIES SUIVANT LES OPTIONS
C           -----------------------------------------------

            CALL LCINVN(3,0.D0,FORREP)
            NOMPAR(1)='X'
            NOMPAR(2)='Y'
            IF (NDIM.EQ.3) NOMPAR(3)='Z'
            IF (NDIM.EQ.3) NOMPAR(4)='INST'
            IF (NDIM.EQ.2) NOMPAR(3)='INST'

            IF (OPTION.EQ.'CHAR_MECA_PRES_R') THEN

C             CALCUL DE LA PRESSION AUX POINTS DE GAUSS
              PRES = 0.D0
              CISA = 0.D0
              DO 240 INO = 1,NNOP
                IF (NDIM.EQ.3) PRES = PRES +  ZR(IPRES-1+INO) * FF(INO)
                IF (NDIM.EQ.2) THEN
                  PRES = PRES + ZR(IPRES-1+2*(INO-1)+1) * FF(INO)
                  CISA = CISA + ZR(IPRES-1+2*(INO-1)+2) * FF(INO)
                ENDIF
 240          CONTINUE
C             ATTENTION AU SIGNE : POUR LES PRESSIONS, IL FAUT UN - DVT
C             CAR LE SECOND MEMBRE SERA ECRIT AVEC UN + (VOIR PLUS BAS)
              DO 250 J=1,NDIM
                FORREP(J) = -PRES * ND(J)
 250          CONTINUE
              IF (NDIM.EQ.2) THEN
                 FORREP(1) = FORREP(1)- CISA * ND(2)
                 FORREP(2) = FORREP(2)+ CISA * ND(1)
              ENDIF

            ELSEIF (OPTION.EQ.'CHAR_MECA_PRES_F') THEN

C             VALEUR DE LA PRESSION
              XG(NDIM+1) = ZR(ITEMPS)
              CALL FOINTE('FM',ZK8(IPRES),NDIM+1,NOMPAR,XG,PRES,IER)
              IF(NDIM.EQ.2) 
     &          CALL FOINTE('FM',ZK8(IPRES+1),NDIM+1,NOMPAR,XG,CISA,IER)
              DO 260 J=1,NDIM
                FORREP(J) = -PRES * ND(J)
 260          CONTINUE
              IF (NDIM.EQ.2) THEN
                 FORREP(1) = FORREP(1)-CISA * ND(2)
                 FORREP(2) = FORREP(2)+CISA * ND(1)
              ENDIF

            ELSEIF (OPTION.EQ.'CHAR_MECA_FR2D3D'.OR.
     &              OPTION.EQ.'CHAR_MECA_FR1D2D') THEN

              CALL LCINVN(NDIM,0.D0,FORREP)
              DO 270 INO = 1,NNOP
                DO 271 J=1,NDIM
                  FORREP(J)=FORREP(J)+ZR(IFORC-1+NDIM*(INO-1)+J)*FF(INO)
 271            CONTINUE
 270          CONTINUE

            ELSEIF (OPTION.EQ.'CHAR_MECA_FF2D3D'.OR.
     &              OPTION.EQ.'CHAR_MECA_FF1D2D') THEN

              XG(NDIM+1) = ZR(ITEMPS)
              DO 280 J=1,NDIM
                CALL FOINTE('FM',ZK8(IFORC-1+J),NDIM+1,NOMPAR,XG,
     &                                                    FORREP(J),IER)
  280         CONTINUE

            ENDIF

C           CALCUL EFFECTIF DU SECOND MEMBRE
C           --------------------------------
            POS=0
            DO 290 INO = 1,NNOP

C             TERME CLASSIQUE
              DO 291 J=1,NDIM
                POS=POS+1
                ZR(IRES-1+POS) = ZR(IRES-1+POS)
     &                           + FORREP(J) * POIDS * FF(INO)
 291          CONTINUE

C             TERME HEAVISIDE
              DO 292 J=1,DDLH
                POS=POS+1
                ZR(IRES-1+POS) = ZR(IRES-1+POS)
     &                           + HE * FORREP(J) * POIDS * FF(INO)  
 292          CONTINUE


C             TERME SINGULIER   
              DO 293 IG=1,NFE
                DO 294 J=1,NDIM
                  POS=POS+1
                  ZR(IRES-1+POS) = ZR(IRES-1+POS) +
     &                             FE(IG) * FORREP(J) * POIDS * FF(INO)
 294            CONTINUE
 293          CONTINUE 
 290        CONTINUE 
 200      CONTINUE

C-----------------------------------------------------------------------
C         FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
C-----------------------------------------------------------------------

          CALL JEDETR(COORSE)
          CALL JEDETR(GEOMLO)
          CALL JEDETR(COORLO)

 110    CONTINUE
 100  CONTINUE

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------

      CALL JEDEMA()
      END
