      SUBROUTINE TE0037(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/05/2009   AUTEUR MAZET S.MAZET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT

C.......................................................................
C
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN CHARGEMENT EN PRESSION REPARTIE
C          SUR LES LEVRES DES FISSURES X-FEM
C          (LA PRESSION PEUT ETRE DONNEE SOUS FORME D'UNE FONCTION)
C
C          OPTIONS : 'CHAR_MECA_PRES_R'
C                    'CHAR_MECA_PRES_F'
C
C           RESTE A DVP : 
C                    'CHAR_MECA_FR2D3D'
C                    'CHAR_MECA_FR1D2D'
C                    'CHAR_MECA_FF2D3D'
C                    'CHAR_MECA_FF1D2D'
C
C  ENTREES  ---> OPTION : OPTION DE CALCUL
C           ---> NOMTE  : NOM DU TYPE ELEMENT
C
C.......................................................................
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

      CHARACTER*8   ELREF,TYPMA,FPG,ELC,NOMPAR(4)
      INTEGER NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO
      INTEGER DDLH,NFE,SINGU,DDLC,NNOM,DDLS,NDDL,IER
      INTEGER IGEOM,IPRES,ITEMPS,IFORC,IRET,IRES,IADZI,IAZK24
      INTEGER JLST,JPTINT,JAINT,JCFACE,JLONCH,JSTNO
      INTEGER I,J,NINTER,NFACE,CFACE(5,3),IFA,NLI,IN(3),IG
      INTEGER AR(12,2),NBAR,FAC(6,4),NBF,IBID2(12,3),IBID,CPT,INO,ILEV
      INTEGER NNOF,NPGF,IPOIDF,IVFF,IDFDEF,IPGF,POS,ZXAIN,XXMMVD
      REAL*8  MULT,PRES,CISA, FORREP(3,2),FF(27),G(3),JAC,ND(3),HE(2)
      REAL*8  RR(2),LST,XG(4),RBID
      LOGICAL LBID
      DATA    HE / -1.D0 , 1.D0/

      CALL JEMARQ()
C
C     PAR CONVENTION :
C     LEVRE INFERIEURE (HE=-1) EST LA LEVRE 1, DE NORMALE SORTANTE  ND
C     LEVRE SUPERIEURE (HE=+1) EST LA LEVRE 2, DE NORMALE SORTANTE -ND

C-----------------------------------------------------------------------
C     INITIALISATIONS
C-----------------------------------------------------------------------
      ZXAIN = XXMMVD('ZXAIN')
      CALL ELREF1(ELREF)
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
C     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
      CALL XTEINI(NOMTE,DDLH,NFE,SINGU,DDLC,NNOM,DDLS,NDDL)
C
      CALL TECAEL(IADZI,IAZK24)
      TYPMA=ZK24(IAZK24-1+3+ZI(IADZI-1+2)+3)

      IF (NDIM .EQ. 3) THEN
        CALL CONFAC(TYPMA,IBID2,IBID,FAC,NBF)
        ELC='TR3'
        FPG='XCON'
      ELSEIF (NDIM.EQ.2) THEN
        CALL CONARE(TYPMA,AR,NBAR)
        ELC='SE2'
        FPG='MASS'
      ENDIF

C-----------------------------------------------------------------------
C     RECUPERATION DES ENTREES / SORTIE
C-----------------------------------------------------------------------

C     PARAMETRES PROPRES A X-FEM
      CALL JEVECH('PLST'   ,'L',JLST)
      CALL JEVECH('PPINTER','L',JPTINT)
      CALL JEVECH('PAINTER','L',JAINT)
      CALL JEVECH('PCFACE' ,'L',JCFACE)
      CALL JEVECH('PLONGCO','L',JLONCH)
      CALL JEVECH('PSTANO' ,'L',JSTNO)

C     RÉCUPÉRATIONS DES DONNÉES SUR LA TOPOLOGIE DES FACETTES
      NINTER=ZI(JLONCH-1+1)
      IF (NINTER.LT.NDIM) GOTO 9999
      NFACE=ZI(JLONCH-1+2)
      DO 11 I=1,NFACE
        DO 12 J=1,NDIM
          CFACE(I,J)=ZI(JCFACE-1+NDIM*(I-1)+J)
 12     CONTINUE
 11   CONTINUE

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

      CALL JEVECH('PVECTUR','E',IRES)

C-----------------------------------------------------------------------
C     BOUCLE SUR LES FACETTES
C-----------------------------------------------------------------------

      DO 100 IFA=1,NFACE
C
C       PETIT TRUC EN PLUS POUR LES FACES EN DOUBLE
        MULT=1.D0
        DO 101 I=1,NDIM
          NLI=CFACE(IFA,I)
          IN(I)=NINT(ZR(JAINT-1+ZXAIN*(NLI-1)+2))
101     CONTINUE
C       SI LES 2/3 SOMMETS DE LA FACETTE SONT DES NOEUDS DE L'ELEMENT
        IF (NDIM .EQ. 3) THEN
          IF (IN(1).NE.0.AND.IN(2).NE.0.AND.IN(3).NE.0) THEN
            DO 102 I=1,NBF
              CPT=0
              DO 103 INO=1,4
                IF (IN(1).EQ.FAC(I,INO).OR.IN(2).EQ.FAC(I,INO).OR.
     &            IN(3).EQ.FAC(I,INO))    CPT=CPT+1
 103          CONTINUE
              IF (CPT.EQ.3) THEN
                 MULT=0.5D0
                 GOTO 104
              ENDIF
 102        CONTINUE
          ENDIF
        ELSEIF (NDIM .EQ. 2) THEN
          IF (IN(1).NE.0.AND.IN(2).NE.0) THEN
            DO 1021 I=1,NBAR
              CPT=0
              DO 1031 INO=1,2
                IF (IN(1).EQ.AR(I,INO).OR.IN(2).EQ.AR(I,INO))
     &          CPT=CPT+1
 1031         CONTINUE
              IF (CPT.EQ.2) THEN
                MULT=0.5D0
                GOTO 104
              ENDIF
 1021       CONTINUE
          ENDIF
        ENDIF
 104    CONTINUE
C
        CALL ELREF4(ELC,FPG,IBID,NNOF,IBID,NPGF,IPOIDF,IVFF,IDFDEF,IBID)

C       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
        DO 110 IPGF=1,NPGF

C         CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
C         ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
C         ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
C         ET DE XG : COORDONNEES REELLES DU POINT DE GAUSS              

          IF (NDIM .EQ. 3) THEN
            CALL XJACFF(ELREF,FPG,JPTINT,IFA,CFACE,IPGF,NNO,IGEOM,XG,
     &                                          'NON',JAC,FF,RBID,ND)
          ELSEIF (NDIM.EQ.2) THEN
            CALL XJACF2(ELREF,FPG,JPTINT,IFA,CFACE,IPGF,NNO,IGEOM,XG,
     &                                          'NON',JAC,FF,RBID,ND)
          ENDIF

C         CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
          IF (SINGU.EQ.1) THEN
            LST=0.D0
            DO 112 I=1,NNO
              LST=LST+ZR(JLST-1+I)*FF(I)
 112        CONTINUE
            CALL ASSERT(LST.LT.0.D0)
            RR(1)=-SQRT(-LST)
            RR(2)= SQRT(-LST)
          ENDIF


C         CALCUL DES FORCES REPARTIES SUIVANT LES OPTIONS
C         -----------------------------------------------

          CALL LCINVN(3*2,0.D0,FORREP)
          NOMPAR(1)='X'
          NOMPAR(2)='Y'
          IF (NDIM.EQ.3) NOMPAR(3)='Z'
          IF (NDIM.EQ.3) NOMPAR(4)='INST'
          IF (NDIM.EQ.2) NOMPAR(3)='INST'


          IF (OPTION.EQ.'CHAR_MECA_PRES_R') THEN

C           CALCUL DE LA PRESSION AUX POINTS DE GAUSS
            PRES = 0.D0
            CISA = 0.D0
            DO 240 INO = 1,NNO
              IF (NDIM.EQ.3) PRES = PRES +  ZR(IPRES-1+INO) * FF(INO)
              IF (NDIM.EQ.2) THEN
                PRES = PRES + ZR(IPRES-1+2*(INO-1)+1) * FF(INO)
                CISA = CISA + ZR(IPRES-1+2*(INO-1)+2) * FF(INO)
              ENDIF
 240        CONTINUE
C           ATTENTION AU SIGNE : POUR LES PRESSIONS, IL FAUT UN - DVT
C           CAR LE SECOND MEMBRE SERA ECRIT AVEC UN + (VOIR PLUS BAS)
C           ON CALCULE FORREP POUR LES DEUX LEVRES  : 1 = INF ET 2 = SUP
            DO 250 J=1,NDIM
              FORREP(J,1) = -PRES * ND(J)
              FORREP(J,2) = -PRES * (-ND(J))
 250        CONTINUE
            IF (NDIM.EQ.2) THEN
               FORREP(1,1) = FORREP(1,1)- CISA * ND(2)
               FORREP(2,1) = FORREP(2,1)+ CISA * ND(1)
               FORREP(1,2) = FORREP(1,2)- CISA * (-ND(2))
               FORREP(2,2) = FORREP(2,2)+ CISA * (-ND(1))
            ENDIF

          ELSEIF (OPTION.EQ.'CHAR_MECA_PRES_F') THEN

C           VALEUR DE LA PRESSION
            XG(NDIM+1) = ZR(ITEMPS)
            CALL FOINTE('FM',ZK8(IPRES),NDIM+1,NOMPAR,XG,PRES,IER)
            IF(NDIM.EQ.2) 
     &        CALL FOINTE('FM',ZK8(IPRES+1),NDIM+1,NOMPAR,XG,CISA,IER)
            DO 260 J=1,NDIM
              FORREP(J,1) = -PRES * ND(J)
              FORREP(J,2) = -PRES * (-ND(J))
 260        CONTINUE
            IF (NDIM.EQ.2) THEN
               FORREP(1,1) = FORREP(1,1)- CISA * ND(2)
               FORREP(2,1) = FORREP(2,1)+ CISA * ND(1)
               FORREP(1,2) = FORREP(1,2)- CISA * (-ND(2))
               FORREP(2,2) = FORREP(2,2)+ CISA * (-ND(1))
            ENDIF

          ELSE
            CALL U2MESS('F','XFEM_15')
          ENDIF

C         CALCUL EFFECTIF DU SECOND MEMBRE SUR LES DEUX LEVRES
          DO 300 ILEV = 1,2

            POS=0
            DO 290 INO = 1,NNO

C             TERME CLASSIQUE
              DO 291 J=1,NDIM
                POS=POS+1
                ZR(IRES-1+POS) = ZR(IRES-1+POS)
     &                           + FORREP(J,ILEV) * JAC * FF(INO) * MULT
 291          CONTINUE

C             TERME HEAVISIDE
              DO 292 J=1,DDLH
                POS=POS+1
                ZR(IRES-1+POS) = ZR(IRES-1+POS)
     &                + HE(ILEV) * FORREP(J,ILEV) * JAC * FF(INO) * MULT
 292          CONTINUE

C             TERME SINGULIER   
              DO 293 J=1,SINGU*NDIM
                POS=POS+1
                ZR(IRES-1+POS) = ZR(IRES-1+POS)
     &            +     RR(ILEV) * FORREP(J,ILEV) * JAC * FF(INO) * MULT
 293          CONTINUE 

C             ON SAUTE LES POSITIONS DES DDLS ASYMPTOTIQUES E2, E3, E4
              POS = POS + (NFE-1) * NDIM * SINGU

C             ON SAUTE LES POSITIONS DES LAG DE CONTACT FROTTEMENT
              POS = POS + DDLC

 290        CONTINUE 
 
 300      CONTINUE

 110    CONTINUE
 100  CONTINUE


C     SUPPRESSION DES DDLS SUPERFLUS
      CALL XTEDDL(NDIM,DDLH,NFE,DDLC,DDLS,NDDL,NNO,NNOS,ZI(JSTNO),
     &                  LBID,OPTION,NOMTE,
     &                  RBID,ZR(IRES))


 9999 CONTINUE
      CALL JEDEMA()
      END
