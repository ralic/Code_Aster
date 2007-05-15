      SUBROUTINE TE0036(OPTION,NOMTE)
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/05/2007   AUTEUR GENIAUT S.GENIAUT 
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
C          SUR DES FACES D'ELEMENTS X-FEM 3D
C          (LA PRESSION PEUT ETRE DONNEE SOUS FORME D'UNE FONCTION)
C
C          OPTIONS : 'CHAR_MECA_PRES_R'
C                    'CHAR_MECA_PRES_F'
C                    'CHAR_MECA_FR2D3D'
C                    'CHAR_MECA_FF2D3D'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
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

      CHARACTER*8   NOMPAR(4),NOMPA2(3),NOMA,ELREFP
      CHARACTER*16  NOMTE,OPTION
      CHARACTER*24  COOR1D,COOR2D,GEOM1D,GEOM2D,COORSE
      INTEGER       JPINTT,JCNSET,JHEAVT,JLONCH,JCOORS,JLSN,JLST
      INTEGER       IBID,IER,NDIM,NNO,NNOP,NPG,NNOS,JGANO,KPG,KDEC,J
      INTEGER       IPOIDS,IVF,IDFDE,JDFD2,IGEOM,IPRES,ITEMPS,IRES,I
      INTEGER       DDLH,NFE,DDLC,NNOM,DDL,NIT,CPT,IT,NSE,ISE
      INTEGER       IN,INO,JCOR1D,JCOR2D,IGEO1D,IGEO2D,IADZI,IAZK24
      INTEGER       NSEMAX,IFORC,IRET,IG,POS,NDIME,NDDL,JDIM
      REAL*8        Y(3),XG(4),RBID,FE(4),XE(2),LSNG,LSTG,RG,TG
      REAL*8        PRES,MATR(6561),FF(27),A(3),B(3),C(3),AB(3),AC(3)
      REAL*8        ND(3),NORME,NAB,RB1(3),RB2(3),GLOC(2),N(3),CISA
      REAL*8        AN(3),G(3),HE,POIDS,DDOT,PADIST,FORREP(3)
      DATA          NOMPAR  /'X','Y','Z','INST'/
      DATA          NOMPA2 /'X','Y','INST'/
C
      CALL JEMARQ()

C-----------------------------------------------------------------------
C     INITIALISATIONS
C-----------------------------------------------------------------------

C     ELEMENT DE REFERENCE PARENT
      CALL ELREF1(ELREFP)
      NDIME=1
      IF (ELREFP.NE.'SE2') THEN
        CALL ELREF4(' ','RIGI',NDIME,NNOP,IBID,IBID,IBID,
     &           IBID,IBID,IBID)
        CALL ASSERT(NDIME.EQ.2)
      ENDIF

C     SOUS-ELEMENT DE REFERENCE SCHEMA IDENTIQUE PARENT
      IF (NDIME.EQ.2) THEN
        CALL ELREF4('TR3','FPG3',NDIME,NNO,NNOS,NPG,IPOIDS,IVF,
     &             IDFDE,IBID)
      ELSEIF (NDIME.EQ.1) THEN
        CALL ELREF4(' ','RIGI',NDIME,NNOP,NNOS,NPG,IPOIDS,IVF,
     &              IDFDE,IBID)
         NNO=NNOP
      ENDIF


C     DIMENSION DE L'ESPACE
      CALL TECAEL(IADZI,IAZK24)
      NOMA=ZK24(IAZK24)
      CALL JEVEUO(NOMA//'.DIME','L',JDIM)
      NDIM=ZI(JDIM-1+6)

C     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
C     NDIM EST LA DIMENSION DU MAILLAGE
C     NDIME EST DIMENSION DE L'ELEMENT FINI


C     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
      IF (NOMTE(1:12).EQ.'MECA_XH_FACE'.OR.
     &    NOMTE(1:12).EQ.'MEPSE3_XH') THEN
        DDLH=NDIM
        NFE=0
      ELSEIF (NOMTE(1:12).EQ.'MECA_XT_FACE'.OR.
     &       NOMTE(1:12).EQ.'MEPSE3_XT') THEN
        DDLH=0
        NFE=4
      ELSEIF (NOMTE(1:13).EQ.'MECA_XHT_FACE'.OR.
     &        NOMTE(1:13).EQ.'MEPSE3_XHT') THEN
        DDLH=NDIM
        NFE=4
      ELSE
        CALL U2MESS('F','ELEMENTS2_78')
      ENDIF

      IF (NDIME.EQ. 3) THEN
         NSEMAX=6
      ELSEIF (NDIME.EQ. 2) THEN
         NSEMAX=3
      ELSEIF (NDIME.EQ. 1) THEN
         NSEMAX=2
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

C         BOUCLE SUR LES 3 SOMMETS DU SOUS-TRIA
C         OU SUR LES 2 SOMMETS DU SOUS-SEG
          DO 112 IN=1,NNO
            INO=ZI(JCNSET-1+(NDIME+1)*(CPT-1)+IN)
            IF (INO.LT.1000) THEN
              ZR(JCOORS-1+NDIM*(IN-1)+1)=ZR(IGEOM-1+NDIM*(INO-1)+1)
              ZR(JCOORS-1+NDIM*(IN-1)+2)=ZR(IGEOM-1+NDIM*(INO-1)+2)
              IF (NDIM.EQ.3) THEN
                 ZR(JCOORS-1+NDIM*(IN-1)+3)=ZR(IGEOM-1+NDIM*(INO-1)+3)
              ENDIF
            ELSE
             ZR(JCOORS-1+NDIM*(IN-1)+1)=ZR(JPINTT-1+NDIM*(INO-1000-1)+1)
             ZR(JCOORS-1+NDIM*(IN-1)+2)=ZR(JPINTT-1+NDIM*(INO-1000-1)+2)
             IF (NDIM.EQ.3) THEN
                ZR(JCOORS-1+NDIM*(IN-1)+3)=ZR(JPINTT-1+NDIM*(INO-1000-1)
     &           +3)
             ENDIF
            ENDIF
 112      CONTINUE

C         ON RENOMME LES SOMMETS DU SOUS-ELEMENT
          CALL LCINVN(3,0.D0,A)
          CALL LCINVN(3,0.D0,B)
          CALL LCINVN(3,0.D0,AB)
          DO 113 J=1,NDIM
            A(J)=ZR(JCOORS-1+NDIM*(1-1)+J)
            B(J)=ZR(JCOORS-1+NDIM*(2-1)+J)
            AB(J)=B(J)-A(J)
            IF (NDIM.EQ.3) THEN
               C(J)=ZR(JCOORS-1+NDIM*(3-1)+J)
               AC(J)=C(J)-A(J)
            ENDIF
 113      CONTINUE 

          IF (NDIM.EQ.3) THEN
C         CREATION DU REPERE LOCAL 2D : (AB,Y)
            CALL PROVEC(AB,AC,ND)
            CALL NORMEV(ND,NORME)
            CALL NORMEV(AB,NAB)
            CALL PROVEC(ND,AB,Y)
          ELSEIF (NDIM.EQ.2) THEN
C         CREATION DU REPERE LOCAL 1D : AB/NAB
            CALL NORMEV(AB,NAB)
            CALL LCINVN(3,0.D0,ND) 
              ND(1) = AB(2)
              ND(2) = -AB(1)
          ENDIF


          IF (NDIM.EQ.3) THEN
C         COORDONNÉES DES SOMMETS DE LA FACETTE DANS LE REPÈRE LOCAL 2D
            COOR2D='&&TE0036.COOR2D'
            CALL WKVECT(COOR2D,'V V R',NNO*NDIME,JCOR2D)
            ZR(JCOR2D-1+1)=0.D0
            ZR(JCOR2D-1+2)=0.D0
            ZR(JCOR2D-1+3)=NAB
            ZR(JCOR2D-1+4)=0.D0
            ZR(JCOR2D-1+5)=DDOT(3,AC,1,AB,1)
            ZR(JCOR2D-1+6)=DDOT(3,AC,1,Y ,1)
          ELSEIF (NDIM.EQ.2) THEN
            COOR1D='&&TE0036.COOR1D'
            CALL WKVECT(COOR1D,'V V R',NNO*NDIME,JCOR1D)
            ZR(JCOR1D-1+1)=0.D0
            ZR(JCOR1D-1+2)=NAB
          ENDIF

          IF (NDIM.EQ.3) THEN
C         COORDONNÉES DES NOEUDS DE L'ELREFP DANS LE REPÈRE LOCAL 2D
            GEOM2D='&&TE0036.GEOM2D'
            CALL WKVECT(GEOM2D,'V V R',NNOP*NDIME,IGEO2D)
            DO 114 INO=1,NNOP
              DO 115 J=1,NDIM
                N(J)=ZR(IGEOM-1+NDIM*(INO-1)+J)
                AN(J)=N(J)-A(J)
 115          CONTINUE
              ZR(IGEO2D-1+2*(INO-1)+1)=DDOT(NDIM,AN,1,AB,1)
              ZR(IGEO2D-1+2*(INO-1)+2)=DDOT(NDIM,AN,1,Y ,1)
 114        CONTINUE
          ELSEIF (NDIM.EQ.2) THEN
C         COORDONNÉES DES NOEUDS DE L'ELREFP DANS LE REPÈRE LOCAL 1D
            GEOM1D='&&TE0036.GEOM1D'
            CALL WKVECT(GEOM1D,'V V R',NNOP*NDIME,IGEO1D)
            DO 116 INO=1,NNOP
              DO 117 J=1,NDIM
                N(J)=ZR(IGEOM-1+NDIM*(INO-1)+J)
                AN(J)=N(J)-A(J)
 117          CONTINUE
              ZR(IGEO1D-1+INO)=DDOT(NDIM,AN,1,AB,1)
 116        CONTINUE
          ENDIF

C         FONCTION HEAVYSIDE CSTE SUR LE SS-ELT
          HE=ZI(JHEAVT-1+NSEMAX*(IT-1)+ISE)

C-----------------------------------------------------------------------
C         BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
C-----------------------------------------------------------------------

          DO 200 KPG=1,NPG

            IF (NDIM.EQ.3) THEN
C           CALCUL DU POIDS EN 2D : POIDS = POIDS DE GAUSS * DET(J)
              CALL DFDM2D(NNO,KPG,IPOIDS,IDFDE,ZR(JCOR2D),RB1,RB2,POIDS)

C           COORDONNÉES RÉELLES LOCALES 2D DU POINT DE GAUSS
            CALL LCINVN(2,0.D0,GLOC)
            DO 210 J=1,NNO
              GLOC(1)=GLOC(1)+ZR(IVF-1+NNO*(KPG-1)+J)*ZR(JCOR2D-1+2*J-1)
              GLOC(2)=GLOC(2)+ZR(IVF-1+NNO*(KPG-1)+J)*ZR(JCOR2D-1+2*J)
 210        CONTINUE
            
            ELSEIF (NDIM.EQ.2) THEN
C           CALCUL DU POIDS EN 2D : POIDS = POIDS DE GAUSS * DET(J)
               POIDS = ZR(IPOIDS-1+KPG) * NAB/2.D0

C         COORDONNEES LOCALES 1D DU POINT DE GAUSS IPG
               CALL LCINVN(2,0.D0,GLOC)
               DO 205 J=1,NNO
                 GLOC(1)=GLOC(1)+ZR(IVF-1+NNO*(KPG-1)+J)*ZR(JCOR1D-1+J)
 205           CONTINUE
              ENDIF
                         
C           JUSTE POUR CALCULER LES FF AUX NOEUDS DE L'ELREFP
            IF (NDIM.EQ.3)  CALL REEREF(ELREFP,NNOP,IGEO2D,GLOC,RBID,
     &         .FALSE.,NDIME,RBID,IBID,IBID,IBID,RBID,RBID,'NON',XE,
     &        FF,RBID,RBID,RBID,RBID)
            IF (NDIM.EQ.2)  CALL REEREF(ELREFP,NNOP,IGEO1D,GLOC,RBID,
     &          .FALSE.,NDIME,RBID,IBID,IBID,IBID,RBID,RBID,'NON',XE,
     &       FF,RBID,RBID,RBID,RBID)


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
C            IF (NDIM.EQ.3) THEN
C            G(1)=A(1)+AB(1)*GLOC(1)+Y(1)*GLOC(2)
C            G(2)=A(2)+AB(2)*GLOC(1)+Y(2)*GLOC(2)
C            G(3)=A(3)+AB(3)*GLOC(1)+Y(3)*GLOC(2)
C              IF (PADIST(3,G,XG).GT.1.D-12) THEN
C                CALL U2MESS('A','ELEMENTS2_79')
C              ENDIF
            

C           CALCUL DES FONCTIONS D'ENRICHISSEMENT
C           -------------------------------------

            IF (NFE.GT.0) THEN
C             LEVEL SETS AU POINT DE GAUSS
              LSNG = 0.D0
              LSTG = 0.D0
              DO 222 INO=1,NNOP
                LSNG = LSNG + ZR(JLSN-1+INO) * FF(INO)
                LSTG = LSTG + ZR(JLST-1+INO) * FF(INO)
 222          CONTINUE

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

            IF (OPTION.EQ.'CHAR_MECA_PRES_R') THEN

C             CALCUL DE LA PRESSION AUX POINTS DE GAUSS
              PRES = 0.D0
              CISA = 0.D0
              DO 212 INO = 1,NNOP
                PRES = PRES +  ZR(IPRES+2*INO-2) * FF(INO)
                IF (NDIM.EQ.2) THEN
                   CISA = CISA +  ZR(IPRES+2*INO-1) * FF(INO)
                ENDIF
 212          CONTINUE
C             ATTENTION AU SIGNE : POUR LES PRESSIONS, IL FAUT UN - DVT
C             CAR LE SECOND MEMBRE SERA ECRIT AVEC UN + (VOIR PLUS BAS)
              FORREP(1) = -PRES * ND(1)
              FORREP(2) = -PRES * ND(2)
              IF (NDIM.EQ.2) THEN
                 FORREP(1) = FORREP(1)- CISA * ND(2)
                 FORREP(2) = FORREP(2)+ CISA * ND(1)
              ENDIF
              IF (NDIM.EQ.3)  FORREP(3) = -PRES * ND(3)

            ELSEIF (OPTION.EQ.'CHAR_MECA_PRES_F') THEN

C             VALEUR DE LA PRESSION
              IF(NDIM.EQ.2) THEN
                XG(3) = ZR(ITEMPS)
                CALL FOINTE('FM',ZK8(IPRES),3,NOMPA2,XG,PRES,IER)
                CALL FOINTE('FM',ZK8(IPRES+1),3,NOMPA2,XG,CISA,IER)
              ELSEIF (NDIM.EQ.3) THEN
                XG(4) = ZR(ITEMPS)
                CALL FOINTE('FM',ZK8(IPRES),4,NOMPAR,XG,PRES,IER)
                FORREP(3) = -PRES * ND(3)
              ENDIF  
              FORREP(1) = -PRES * ND(1)
              FORREP(2) = -PRES * ND(2)
              IF (NDIM.EQ.2) THEN
                 FORREP(1) = FORREP(1)- CISA * ND(2)
                 FORREP(2) = FORREP(2)+CISA * ND(1)
              ENDIF

            ELSEIF (OPTION.EQ.'CHAR_MECA_FR2D3D') THEN

              CALL LCINVN(NDIM,0.D0,FORREP)
              DO 213 INO = 1,NNOP
                FORREP(1) = FORREP(1) + ZR(IFORC-1+3*(INO-1)+1)* FF(INO)
                FORREP(2) = FORREP(2) + ZR(IFORC-1+3*(INO-1)+2)* FF(INO)
                FORREP(3) = FORREP(3) + ZR(IFORC-1+3*(INO-1)+3)* FF(INO)
 213          CONTINUE

            ELSEIF (OPTION.EQ.'CHAR_MECA_FF2D3D') THEN

              XG(4) = ZR(ITEMPS)
              CALL FOINTE('FM',ZK8(IFORC)  ,4,NOMPAR,XG,FORREP(1),IER)
              CALL FOINTE('FM',ZK8(IFORC+1),4,NOMPAR,XG,FORREP(2),IER)
              CALL FOINTE('FM',ZK8(IFORC+2),4,NOMPAR,XG,FORREP(3),IER)
              
            ELSEIF (OPTION.EQ.'CHAR_MECA_FR1D2D') THEN
            
              CALL LCINVN(NDIM,0.D0,FORREP)
              DO 214 INO = 1,NNOP
               FORREP(1) = FORREP(1) + ZR(IFORC-1+2*(INO-1)+1)* FF(INO)
               FORREP(2) = FORREP(2) + ZR(IFORC-1+2*(INO-1)+2)* FF(INO)
 214          CONTINUE
            ELSEIF (OPTION.EQ.'CHAR_MECA_FF1D2D') THEN
            
              XG(3) = ZR(ITEMPS)
              CALL FOINTE('FM',ZK8(IFORC)  ,3,NOMPA2,XG,FORREP(1),IER)
              CALL FOINTE('FM',ZK8(IFORC+1),3,NOMPA2,XG,FORREP(2),IER)

            ENDIF

C           CALCUL EFFECTIF DU SECOND MEMBRE
C           --------------------------------
            POS=0
            DO 230 INO = 1,NNOP

C             TERME CLASSIQUE
              DO 231 J=1,NDIM
                POS=POS+1

                ZR(IRES-1+POS) = ZR(IRES-1+POS)
     &                           + FORREP(J) * POIDS * FF(INO)
 231          CONTINUE

C             TERME HEAVISIDE
              DO 232 J=1,DDLH
                POS=POS+1

                ZR(IRES-1+POS) = ZR(IRES-1+POS)
     &                           + HE * FORREP(J) * POIDS * FF(INO)  
 232          CONTINUE

C             TERME SINGULIER   
              DO 233 IG=1,NFE
                DO 234 J=1,NDIM
                  POS=POS+1
                  ZR(IRES-1+POS) = ZR(IRES-1+POS) +
     &                             FE(IG) * FORREP(J) * POIDS * FF(INO)
 234            CONTINUE
 233          CONTINUE 
 230        CONTINUE 
 200      CONTINUE

C-----------------------------------------------------------------------
C         FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
C-----------------------------------------------------------------------

          CALL JEDETR(COORSE)
          CALL JEDETR(GEOM2D)
          CALL JEDETR(COOR2D)
          CALL JEDETR(GEOM1D)
          CALL JEDETR(COOR1D)
 110    CONTINUE
 100  CONTINUE

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
