      SUBROUTINE TE0096(OPTION,NOMTE)
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C
C FONCTION REALISEE:
C
C   TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE EN ELASTICITE
C                                             EN ELASTICITE NON LINEAIRE
C                                             EN GRANDS DEPLACEMENTS
C
C   CALCUL DE SA DERIVEE PAR RAPPORT A UNE VARIATION DE DOMAINE (EN
C   ELASTIQUE ISOTROPE LINEAIRE PETIT OU GRAND DEPLACEMENTS). LES CHARGE
C   MENTS SONT SUPPOSES INDEPENDANTS DE LA VARIATION DE DOMAINE:
C   DERIVEE LAGRANGIENNE(F) = GRADIENT(F) * THETA SENSIBILITE CAR LA
C   DERIVEE PARTIELLE DF/DN = 0, EN NOTANT N LE PARAMETRE PILOTANT LA
C   VARIATION DE DOMAINE (M = P + N * THETA SENSIBILITE(P)).
C
C   ELEMENTS ISOPARAMETRIQUES 2D
C
C   OPTION : 'CALC_G'     (G AVEC CHARGES REELLES)
C            'CALC_G_F'   (G AVEC CHARGES FONCTIONS)
C            'CALC_DG'    (G + DG AVEC CHARGES REELLES)
C            'CALC_DG_F'  (G + DG AVEC CHARGES FONCTIONS)
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       JEVEUX AND CO: JEMARQ, JEDEMA, JEVETE, JEVECH, TECACH.
C       MSG: UTMESS.
C       ENVIMA: R8PREM
C       MATERIAUX: RCCOMA, RCVALA.
C       ELEMENTS FINIS: NMGEOM, PPGANO, D2GEOM.
C       DIVERS: FOINTE, NMELNL, NMPLRU.
C
C     FONCTIONS INTRINSEQUES:
C       SQRT, ABS.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): DEPLACEMENT DU TEST DE LA NULLITE DU THETAFISS,
C                      TOILETTAGE FORTRAN,
C                      MISE EN PLACE DE LA DERIVEE DE G.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16      OPTION,NOMTE

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

C DECLARATION VARIABLES LOCALES
      CHARACTER*2   CODRET
      CHARACTER*8   NOMPAR(3),TYPMOD(2),ELREFE
      CHARACTER*16  COMPOR(4),OPRUPT,PHENOM
      CHARACTER*24  CHVAL,CHCTE

      REAL*8   EPSI,RAC2,R8PREM,CRIT(3)
      REAL*8   DFDI(27),F(3,3),SR(3,3),SIGL(6),SIGIN(6),DSIGIN(6,3)
      REAL*8   EPS(6),EPSIN(6),DEPSIN(6,3),EPSP(6),DEPSP(6,3)
      REAL*8   EPSINO(36),EPSIPG(36),FNO(18),EPSNO(36)
      REAL*8   THET,TREF,TG,TGDM(3),PROD,PROD1,PROD2,DIVT,VALPAR(3)
      REAL*8   TCLA,TTHE,TFOR,TPLAS,TINI,POIDS,R,RBID
      REAL*8   P,PPG,DPDM(3),RP,ENERGI(2),RHO,OM,OMO
      REAL*8   DTDM(3,5),DER(6),DFDM(3,5),DUDM(3,4)
      REAL*8   GRADDU(3,4),GRADTH(3,4),DLAGTG,DDLAGT(3),
     &         D2DFDM(3,6),D2EPSI(6,6),D2SIGI(6,6),
     &         DLDFDM(3,3),DLDTDM(3,3),DLEPSI(6,3),DLSIGI(6,3),
     &         TAUX(3,3),DDUDM(3,3),DEPSTA(3,3),DEPS(6),DLF(3,3),
     &         DSR(3,3),DSIGL(6),DENERG(2),
     &         DDEPSI(6),DDSIGI(6),DFD2DI(27),
     &         DPROD1,DPROD2,DPROD3,DPROD4,TMP,RAUX,RAUX1,RAUX2,
     &         DIVTS,DLDIVT,
     &         DTCLA,DTTHE,DTFOR,DTPLAS,DTINI

      INTEGER  IPOIDS,IVF,IDFDE,IDFDK,IPOI1,IVF1,IDFDE1,IDFDK1
      INTEGER  ICOMP,IGEOM,ITEMPS,IDEPL,ITREF,ITEMP,IMATE
      INTEGER  IEPSR,IEPSF,ISIGI,IDEPI,ISIGM,IEPSP,IVARI
      INTEGER  IFORC,IFORF,ITHET,IGTHET,IROTA,IPESA,IER
      INTEGER  NNO,NNOS,NPG,NPG1,NPG2,NPG3,NPG4,NCMP
      INTEGER  I,J,K,KK,L,M,KP,NDIM,COMPT,JIN,JVAL,NBVARI
      INTEGER  IDLAGD,ITHETA,IJ,IJ1,MATCOD,IDLAGT,IDGTHE,IDFD2E,
     &         IDFD2K,IDFDEK,NBDIV2,IDEB,IFIN,I1

      LOGICAL  GRAND,AXI,CP,FONC,INCR,EPSINI,
     &         TSENUL,DERIVL

C =====================================================================
C INITIALISATIONS
C =====================================================================

      CALL ELREF1(ELREFE)
      CALL JEMARQ()
      EPSI   = R8PREM()
      RAC2   = SQRT(2.D0)
      OPRUPT = 'RUPTURE'
      AXI    = .FALSE.
      CP     = .FALSE.
      EPSINI = .FALSE.


      IF (NOMTE(3:4).EQ.'AX') THEN
        TYPMOD(1) = 'AXIS    '
        AXI = .TRUE.
      ELSEIF (NOMTE(3:4).EQ.'CP') THEN
        TYPMOD(1) = 'C_PLAN  '
        CP  = .TRUE.
      ELSEIF (NOMTE(3:4).EQ.'DP') THEN
        TYPMOD(1) = 'D_PLAN  '
      ENDIF

C RECUPERATION DES DONNEES GEOMETRIQUES LIEES AU CALCUL ELEMENTAIRE
      CHCTE = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CHCTE,' ',JIN)
      NNO   = ZI(JIN)
      NPG1  = ZI(JIN+2)
      NPG2  = ZI(JIN+3)
      NPG3  = ZI(JIN+4)
      NPG4  = ZI(JIN+5)

C INITIALISATION EN DUR (NDIM: DIMENSION DE L'ESPACE DE TRAVAIL)
      NDIM  = 2

C NOMBRE DE DERIVEES SECONDES
      IF (NDIM.EQ.2) THEN
        NBDIV2 = 3
      ELSE IF (NDIM.EQ.3) THEN
        NBDIV2 = 6
      ENDIF

C NOMBRE DE COMPOSANTES DES TENSEURS
      NCMP = 2*NDIM

C INIT. POUR LE CALCUL DE G
      TCLA  = 0.D0
      TTHE  = 0.D0
      TFOR  = 0.D0
      TPLAS = 0.D0
      TINI  = 0.D0
      CALL JEVECH('PGTHETA','E',IGTHET)
      CALL JEVECH('PTHETAR','L',ITHET)

C TEST SUR LA NULLITE DE THETA_FISSURE
      DERIVL = .FALSE.
      COMPT = 0
      DO 3 I=1,NNO
        THET = 0.D0
        DO 2 J=1,NDIM
          THET = THET + ABS(ZR(ITHET+NDIM*(I-1)+J-1))
2      CONTINUE
        IF (THET.LT.EPSI) COMPT = COMPT+1
3     CONTINUE
      IF(COMPT.EQ.NNO) GOTO 9999

C INIT. COMPLEMENTAIRES POUR LA DERIVATION DE G
      IF (OPTION(1:7).EQ.'CALC_DG') THEN
        DERIVL = .TRUE.
        DTCLA  = 0.D0
        DTTHE  = 0.D0
        DTFOR  = 0.D0
        DTPLAS = 0.D0
        DTINI  = 0.D0
        CALL JEVECH('PDGTHET','E',IDGTHE)
        CALL JEVECH('PVECTTH','L',ITHETA)
      ENDIF

C POINTEURS POUR LES FONCTIONS DE FORME ET LEURS DERIVEES PREMIERES
C SUR L'ELEMENT DE REFERENCE
      CHVAL = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(CHVAL,' ',JVAL)
      IPOIDS = JVAL
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
      IDFDK  = IDFDE  + NPG1*NNO

C =====================================================================
C RECUPERATION DES CHAMPS LOCAUX
C =====================================================================

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PTEREF' ,'L',ITREF)
      CALL JEVECH('PTEMPER','L',ITEMP)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      MATCOD = ZI(IMATE)
      DO 10 I = 1,4
        COMPOR(I)= ZK16(ICOMP+I-1)
10    CONTINUE

C TEST SUR LA LOI DE COMPORTEMENT
C RECUPERATION DES CHAMPS LOCAUX (CARTE) ASSOCIES AU CALCUL DE LA
C DERIV.: PVECTTH (THETA SENSIBILITE), PDLAGDE (DERIV. DEPLACEMENT)
C PDLAGTE (DERIV. TEMPERATURE)
      IF (DERIVL) THEN
        IF (COMPOR(1)(1:4).NE.'ELAS')
     &    CALL UTMESS('F','TE0096 '//OPTION,'SEULE UNE LOI DE  '//
     &    'COMPORTEMENT ELASTIQUE ISOTROPE EST VALIDE POUR LE '//
     &    'CALCUL DE DG !')
        CALL JEVECH('PDLAGDE','L',IDLAGD)
        CALL JEVECH('PDLAGTE','L',IDLAGT)

C TEST DE LA NULLITE DU THETA SENSIBILITE
        IDEB = ITHETA
        IFIN = ITHETA + 2*NNO - 1
        TSENUL = .TRUE.
        DO 11 I = IDEB , IFIN
          IF (ABS(ZR(I)).GT.EPSI) THEN
            TSENUL = .FALSE.
          ENDIF
11      CONTINUE

C POINTEURS SUR LEURS DERIVEES SECONDES
C (ATTENTION QU'EN NPG1 ET NNO=6, 8 OU 9, CF. INIT099)
        IDFD2E = JVAL + (NPG1+NPG2+NPG3+NPG4)*(1+NNO*(NDIM+1))
        IDFD2K = IDFD2E + NPG1*NNO
        IDFDEK = IDFD2K + NPG1*NNO
      ENDIF

C RECUPERATION DU CHAMP LOCAL (CARTE) ASSOCIE AU EPS INIT
      IF ((OPTION.EQ.'CALC_G_F').OR.(OPTION.EQ.'CALC_DG_F')) THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'INST'
        VALPAR(3) = ZR(ITEMPS)
        CALL TECACH(.TRUE.,.FALSE.,'PEPSINF',1,IEPSF)
        IF (IEPSF.NE.0) EPSINI = .TRUE.
      ELSE
        FONC = .FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
        CALL TECACH(.TRUE.,.FALSE.,'PEPSINR',1,IEPSR)
        IF (IEPSR.NE.0) EPSINI = .TRUE.
      ENDIF

C LOI DE COMPORTEMENT
      GRAND = COMPOR(3)(1:5).EQ.'GREEN'
      INCR  = COMPOR(4)(1:9).EQ.'COMP_INCR'
      READ(ZK16(ICOMP+1),'(I16)') NBVARI
      IF (INCR) THEN
        CALL JEVECH('PCONTRR','L',ISIGM)
        CALL JEVECH('PDEFOPL','L',IEPSP)
        CALL JEVECH('PVARIPR','L',IVARI)
      ENDIF
      CALL TECACH(.TRUE.,.FALSE.,'PPESANR',1,IPESA)
      CALL TECACH(.TRUE.,.FALSE.,'PROTATR',1,IROTA)
      CALL TECACH(.TRUE.,.FALSE.,'PSIGINR',1,ISIGI)
      CALL TECACH(.TRUE.,.FALSE.,'PDEPINR',1,IDEPI)

C TREF PAR MAILLE ET DEFORMATION INITIALE PAR NOEUD
      TREF  = ZR(ITREF)
      DO 20 I=1,NCMP*NNO
        EPSINO(I) = 0.D0
20    CONTINUE

C =====================================================================
C MESSAGES D'ERREURS
C =====================================================================

C ON NE PEUT AVOIR SIMULTANEMENT DEFORMATIONS ET CONTRAINTES INIT.
      IF ((ISIGI.NE.0).AND.EPSINI) THEN
        CALL UTMESS('F',OPTION,'UNE DEFORMATION INITIALE EST '//
     &  'PRESENTE DANS LA CHARGE : INCOMPATIBLE AVEC LA CONTRAINTE'//
     &  ' INITIALE SIGMA_INIT')
      ENDIF

C FLAG AVEC L'OPTION 'CALC_DG' OU 'CALC_DG_F'
      IF (DERIVL.AND.INCR)
     &  CALL UTMESS('F','TE0096 '//OPTION,'LE CALCUL DE DG N''A '//
     &  'PAS ETE ETENDU A LA PLASTICITE !')

C =====================================================================
C RECUPERATION DES CHARGES ET DEFORMATIONS INITIALES
C =====================================================================

      IF (FONC) THEN
        DO 50 I=1,NNO
          I1 = I-1
          IJ = IGEOM+NDIM*I1-1
          DO 30 J=1,NDIM
            VALPAR(J) = ZR(IJ+J)
30        CONTINUE
          DO 40 J=1,NDIM
            KK = NDIM*I1+J
            CALL FOINTE('FM',ZK8(IFORF+J-1),3,NOMPAR,VALPAR,FNO(KK),IER)
40        CONTINUE
          IF (EPSINI) THEN
            DO 45 J=1,NCMP
              KK = NCMP*I1+J
              CALL FOINTE('FM',ZK8(IEPSF+J-1),3,NOMPAR,VALPAR,
     +                     EPSINO(KK),IER)
45          CONTINUE
          ENDIF
50      CONTINUE
      ELSE
        DO 80 I=1,NNO
          I1 = I-1
          IJ = NDIM*I1
          IJ1 = IFORC+IJ-1
          DO 60 J=1,NDIM
            FNO(IJ+J)= ZR(IJ1+J)
60        CONTINUE
          IF (EPSINI) THEN
            IJ = NCMP*I1
            IJ1 = IEPSR+IJ-1
            DO 70 J=1,3
              EPSINO(IJ+J) = ZR(IJ1+J)
70          CONTINUE
            EPSINO(IJ+4) = ZR(IJ1+4)*RAC2
          ENDIF
80      CONTINUE
      ENDIF

C CORRECTION DES FORCES VOLUMIQUES
      IF ((IPESA.NE.0).OR.(IROTA.NE.0)) THEN
        CALL RCCOMA(MATCOD,'ELAS',PHENOM,CODRET)
        CALL RCVALA(MATCOD,PHENOM,1,' ',RBID,1,'RHO',RHO,CODRET,'FM')
        IF (IPESA.NE.0) THEN
          DO 95 I=1,NNO
            IJ = NDIM*(I-1)
            DO 90 J=1,NDIM
              KK = IJ + J
              FNO(KK)=FNO(KK)+RHO*ZR(IPESA)*ZR(IPESA+J)
90          CONTINUE
95        CONTINUE
        ENDIF
        IF (IROTA.NE.0) THEN
          OM = ZR(IROTA)
          DO 105 I=1,NNO
            OMO = 0.D0
            IJ =  NDIM*(I-1)
            DO 100 J=1,NDIM
              OMO = OMO + ZR(IROTA+J)* ZR(IGEOM+IJ+J-1)
100         CONTINUE
            DO 103 J=1,NDIM
              KK = IJ + J
              FNO(KK)=FNO(KK)+RHO*OM*OM*(ZR(IGEOM+KK-1)-OMO*ZR(IROTA+J))
103         CONTINUE
105       CONTINUE
        ENDIF
      ENDIF

C =====================================================================
C TRAITEMENTS PARTICULIERS LIES A UN DEPLACEMENT INITIAL
C =====================================================================

      IF (IDEPI.NE.0) THEN
        IF(NOMTE(5:8).EQ.'TR3 '.OR. NOMTE(5:8).EQ.'QU4 ') THEN
          NNOS   = NNO
          IPOI1  = JVAL  + NPG1*(1+3*NNO)
          IVF1   = IPOI1 + NPG2
          IDFDE1 = IVF1  + NPG2*NNO
          IDFDK1 = IDFDE1+ NPG2*NNO
          NPG    = NPG2
        ELSE
          NNOS   = NNO/2
          IPOI1  = JVAL  + (NPG1+NPG2+NPG3)*(1+3*NNO)
          IVF1   = IPOI1 + NPG4
          IDFDE1 = IVF1  + NPG4*NNO
          IDFDK1 = IDFDE1+ NPG4*NNO
          NPG    = NPG4
        ENDIF
        DO 120 KP=1,NPG
          L    = (KP-1)*NNO
          CALL NMGEOM (NDIM,NNO,AXI,GRAND,ZR(IGEOM),KP,ZR(IPOI1+KP-1),
     &                 ZR(IVF1+L),ZR(IDFDE1),RBID,ZR(IDFDK1),ZR(IDEPI),
     &                 RBID,DFDI,F,EPS,RBID)
          DO 110 I=1,NCMP
            EPSIPG((KP-1)*NCMP+I)= EPS(I)
110       CONTINUE
120     CONTINUE
C
        CALL PPGANO (NNOS,NPG,NCMP,EPSIPG,EPSNO)
        DO 121 I=1,NNO*NCMP
          EPSINO(I) = EPSINO(I)+EPSNO(I)
121     CONTINUE
      ENDIF

C ======================================================================
C BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ======================================================================

      DO 800 KP=1,NPG1

C INITIALISATIONS
        L   = (KP-1)*NNO
        TG  = 0.D0
        PPG = 0.D0
        DLAGTG = 0.D0
        DO 220 I=1,3
          TGDM(I) = 0.D0
          DPDM(I) = 0.D0
          DDLAGT(I) = 0.D0
          DO 200 J=1,3
            SR(I,J) = 0.D0
            DSR(I,J) = 0.D0
            DDUDM(I,J) = 0.D0
            TAUX(I,J) = 0.D0
            DLF(I,J) = 0.D0
            DLDTDM(I,J) = 0.D0
            DLDFDM(I,J) = 0.D0
200       CONTINUE
          DO 210 J=1,4
            DUDM(I,J) = 0.D0
            DTDM(I,J) = 0.D0
            DFDM(I,J) = 0.D0
            GRADDU(I,J) = 0.D0
            GRADTH(I,J) = 0.D0
210       CONTINUE
          DFDM(I,5) = 0.D0
          DTDM(I,5) = 0.D0
220     CONTINUE
        DO 240 I=1,6
          SIGL (I) = 0.D0
          SIGIN(I) = 0.D0
          EPSIN(I) = 0.D0
          EPSP(I)  = 0.D0
          EPS (I)  = 0.D0
          DEPS(I)  = 0.D0
          DSIGL (I) = 0.D0
          DDSIGI(I) = 0.D0
          DDEPSI(I) = 0.D0
          DO 230 J=1,3
            DSIGIN(I,J) = 0.D0
            DEPSIN(I,J) = 0.D0
            DEPSP(I,J)  = 0.D0
            D2DFDM(J,I) = 0.D0
            DLEPSI(I,J) = 0.D0
            DLSIGI(I,J) = 0.D0
230       CONTINUE
          DO 231 J=1,6
            D2EPSI(I,J) = 0.D0
            D2SIGI(I,J) = 0.D0
231       CONTINUE
240     CONTINUE

C ===========================================
C CALCUL DES ELEMENTS GEOMETRIQUES
C ===========================================

        CALL NMGEOM (NDIM,NNO,AXI,GRAND,ZR(IGEOM),KP,ZR(IPOIDS+KP-1),
     &               ZR(IVF+L),ZR(IDFDE),RBID,ZR(IDFDK),ZR(IDEPL),
     &               POIDS,DFDI,F,EPS,R)
C - CALCULS DES GRADIENTS DE U (DUDM), DE THETA FISSURE (DTDM) ET DE
C   LA FORCE VOLUMIQUE (DFDM),
C   DE LA TEMPERATURE AUX POINTS DE GAUSS (TG) ET SON GRADIENT (TGDM)
        DO 320 I=1,NNO
          I1 = I-1
          DER(1) = DFDI(I)
          DER(2) = DFDI(I+NNO)
          DER(4) = ZR(IVF+L+I1)
          TG    = TG + ZR(ITEMP+I1)*DER(4)
          DO 310 J=1,NDIM
            IJ1 = NDIM*I1+J
            IJ = IJ1 - 1
            TGDM(J)     = TGDM(J)   + ZR(ITEMP+I1)*DER(J)
            DO 300 K=1,NDIM
              DUDM(J,K) = DUDM(J,K) + ZR(IDEPL+IJ)*DER(K)
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET+IJ)*DER(K)
              DFDM(J,K) = DFDM(J,K) + FNO(IJ1)*DER(K)
300         CONTINUE
              DUDM(J,4) = DUDM(J,4) + ZR(IDEPL+IJ)*DER(4)
              DTDM(J,4) = DTDM(J,4) + ZR(ITHET+IJ)*DER(4)
              DFDM(J,4) = DFDM(J,4) + FNO(IJ1)*DER(4)
310       CONTINUE
320     CONTINUE

C ===========================================
C 1 CALCULS COMPLEMENTAIRES POUR DG
C ===========================================

        IF (DERIVL) THEN

C CALCUL DES DERIVEES SECONDES DES FONCTIONS DE FORMES
          CALL D2GEOM(NDIM,NNO,ZR(IGEOM),KP,ZR(IDFDE),ZR(IDFDK),
     &                ZR(IDFD2E),ZR(IDFD2K),ZR(IDFDEK),DFD2DI)

C CALCULS DE LA DERIVEE LAGRANGIENNE DU DEPLACEMENT (GRADDU(.,4)) ET
C DE SON GRADIENT (GRADDU),
C DU THETA SENSIBILITE (GRADTH(.,4)) ET DE SON GRADIENT (GRADTH),
C DE LA DERIVEE LAGRANGIENNE DE LA TEMPERATURE (DLAGTG) ET
C DE SON GRADIENT (DDLAGT).
          DO 323 I=1,NNO
            I1 = I-1
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(4) = ZR(IVF+L+I1)
            DLAGTG = DLAGTG + ZR(IDLAGT+I1)*DER(4)
            DO 322 J=1,NDIM
              IJ1 = NDIM*I1+J
              IJ = IJ1 - 1
              DDLAGT(J) = DDLAGT(J) + ZR(IDLAGT+I1)*DER(J)
              DO 321 K=1,NDIM
                GRADDU(J,K) = GRADDU(J,K) + ZR(IDLAGD+IJ)*DER(K)
                GRADTH(J,K) = GRADTH(J,K) + ZR(ITHETA+IJ)*DER(K)
321           CONTINUE
              GRADDU(J,4) = GRADDU(J,4) + ZR(IDLAGD+IJ)*DER(4)
              GRADTH(J,4) = GRADTH(J,4) + ZR(ITHETA+IJ)*DER(4)
322         CONTINUE
323       CONTINUE

C ====================================
C SI LE THETA SENSIBILITE EST NON NUL
C ====================================
C CALCUL DES DERIVEES SECONDES DES FORCES VOLUMIQUES (D2DFDM) ET DU
C THETA FISSURE (D2DTDM). ON EN DEDUIT LES DERIVEES LAGRANGIENNES DU
C GRADIENT DE THETA FISSURE (DLDTDM(I,J)) ET DES FORCES VOLUMIQUES
C (DLDFDM(I,J)).
C LA DERIVEE LAGRANGIENNE DU THETA FISSURE (DTDM(.,5)) EST NULLE,
C ON CALCULE CELLES DES FORCES VOLUMIQUES (DFDM(.,5)) ET DU TERME
C AUXILIAIRE TAUX POUR LA DERIVEE LAGRANGIENNE DU GRADIENT DU
C DEPLACEMENT (DDUDM).
C ON TRANSFORME GRAD(DL(T)) EN DL(GRAD(T)) (DDLAGT).

          IF (.NOT.TSENUL) THEN
            DO 502 I=1,NDIM
              DO 501 J=1,NDIM
                DDLAGT(I) = DDLAGT(I) - TGDM(J)*GRADTH(J,I)
501           CONTINUE
502         CONTINUE
            DO 353 I=1,NNO
              DER(1) = DFD2DI(I)
              DER(2) = DFD2DI(I+NNO)
              DER(3) = DFD2DI(I+2*NNO)
              DO 352 J=1,NDIM
                IJ1 = NDIM *(I-1)+J
                IJ = IJ1 - 1
                DO 351 K=1,NBDIV2
                  D2DFDM(J,K) = D2DFDM(J,K) + FNO(IJ1)*DER(K)
351             CONTINUE
352           CONTINUE
353         CONTINUE
            RAUX1 = GRADTH(1,4)
            RAUX2 = GRADTH(2,4)
            DO 357 I=1,NDIM
              DLDFDM(I,1)=D2DFDM(I,1)*RAUX1+D2DFDM(I,3)*RAUX2
              DLDFDM(I,2)=D2DFDM(I,3)*RAUX1+D2DFDM(I,2)*RAUX2
357         CONTINUE
            DO 355 I=1,NDIM
              DO 354 J=1,NDIM
                DFDM(I,5) = DFDM(I,5) + DFDM(I,J)*GRADTH(J,4)
354           CONTINUE
355         CONTINUE

C CALCUL DE TERMES AUXILIAIRES POUR LA DERIVATION DE EPS (TAUX) ET DE
C CELLE DU THETA FISSURE (DLDTDM)
          DO 326 I=1,NDIM
            DO 325 J=1,NDIM
              DO 324 K=1,NDIM
                TAUX(I,J) = TAUX(I,J) + DUDM(I,K)*GRADTH(K,J)
C DL(THETAFISSURE)=0 C'EST UNE SORTE DE FONCTION TEST
                DLDTDM(I,J) = DLDTDM(I,J) - DTDM(I,K)*GRADTH(K,J)
324           CONTINUE
325        CONTINUE
326       CONTINUE
        ENDIF
C =======================================
C FIN DU IF THETA SENSIBILITE EST NON NUL
C =======================================

C CALCUL DE LA DERIVEE LAGRANGIENNE DE EPS (DEPS)
        DO 328 I=1,NDIM
          DO 327 J=1,NDIM
            DDUDM(I,J) = GRADDU(I,J) - TAUX(I,J)
327       CONTINUE
328     CONTINUE

        DO 331 I=1,NDIM
          DO 330 J=1,I
            TMP = DDUDM(I,J) + DDUDM(J,I)
            IF (GRAND) THEN
              DO 329 K=1,NDIM
                TMP = TMP + (DDUDM(K,I)* DUDM(K,J)+
     &                        DUDM(K,I)*DDUDM(K,J))
329           CONTINUE
            ENDIF
            DEPSTA(I,J) = 0.5D0*TMP
330       CONTINUE
331     CONTINUE

C TRAITEMENTS PARTICULIERS LIES A LA REPRESENTATION DES TENSEURS
C (RAC2), A L'INCIDENCE DE L'AXISYMETRIE SUR LES GRADIENTS ET
C LEURS DERIVEES LAGRANGIENNES.
        DEPS(1) = DEPSTA(1,1)
        DEPS(2) = DEPSTA(2,2)
        DEPS(4) = DEPSTA(2,1)*RAC2
        IF (NDIM.EQ.3) THEN
          DEPS(3) = DEPSTA(3,3)
          DEPS(5) = DEPSTA(3,1)*RAC2
          DEPS(6) = DEPSTA(3,2)*RAC2
        ELSE IF (AXI) THEN
          GRADTH(3,3)= GRADTH(1,4)/R
          RAUX = (GRADDU(1,4)-(DUDM(1,4)*GRADTH(1,4)/R))/R
          DDUDM(3,3) = RAUX
          DEPS(3) = RAUX
          IF (GRAND) DEPS(3) = DEPS(3) + RAUX*DUDM(1,4)/R
          DLDTDM(3,3) = (DTDM(1,5) -
     &                  (DTDM(1,4)*GRADTH(1,4)/R))/R
          DLDFDM(3,3) = (DFDM(1,5) -
     &                  (DFDM(1,4)*GRADTH(1,4)/R))/R
        ENDIF

C CALCUL DE LA DERIVEE LAGRANGIENNE DE LA TRANSFORMATION (EN GRANDES
C DEFORMATIONS ET/OU AXI) (DLF(I,J))
        IF (GRAND) THEN
          DO 349 I=1,3
            DO 348 J=1,3
              DLF(I,J) = DLF(I,J) + DDUDM(I,J)
348         CONTINUE
349       CONTINUE
          IF (AXI) DLF(3,3) = RAUX
        ENDIF
      ENDIF
C ===========================================
C FIN DES CALCULS COMPLEMENTAIRES 1 POUR DG
C ===========================================

C =======================================================
C PLASTICITE
C =======================================================

C CALCULS DES GRADIENTS DE P (DPDM) ET EPSP (DEPSP) EN PLASTICITE

        IF (INCR) THEN
          DO 380 I=1,NNO
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = 0.D0
            DER(4) = ZR(IVF+L+I-1)
            P   = ZR(IVARI+(I-1)*NBVARI)
            PPG = PPG + P*DER(4)
            DO 350 J=1,NCMP
              EPSP(J) = EPSP(J)+ ZR(IEPSP+NCMP*(I-1)+J-1)*DER(4)
350         CONTINUE
            IF (P.GE.EPSI) THEN
              DO 360 J=1,NDIM
                DPDM(J)= DPDM(J) + ZR(IVARI+(I-1)*NBVARI)*DER(J)
360           CONTINUE
              DO 370 K=1,NDIM
                DO 365 J=1,NCMP
                  DEPSP(J,K)=DEPSP(J,K)+ZR(IEPSP+NCMP*(I-1)+J-1)*DER(K)
365             CONTINUE
370           CONTINUE
            ENDIF
380       CONTINUE
          DO 382 I=4,NCMP
            EPSP(I)=EPSP(I)*RAC2
            DO  381 J=1,NDIM
              DEPSP(I,J)=DEPSP(I,J)*RAC2
381         CONTINUE
382       CONTINUE
          IF (PPG.LT.EPSI)THEN
            PPG = 0.D0
            DO 390 J=1,NCMP
              EPSP(J) =  0.D0
390         CONTINUE
          ENDIF
        ENDIF

C =======================================================
C DEFORMATIONS INITIALES
C =======================================================

        IF ((IDEPI.NE.0).OR.EPSINI) THEN
          DO 420 I=1,NNO
            I1 = I-1
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = 0.D0
            DER(4) = ZR(IVF+L+I1)
            IJ = NCMP*I1
            DO 400 J=1,NCMP
              EPSIN(J) = EPSIN(J)+ EPSINO(IJ+J)*DER(4)
400         CONTINUE
            DO 415 J=1,NCMP
              IJ1 = IJ+J
              DO 410 K=1,NDIM
                DEPSIN(J,K) = DEPSIN(J,K)+EPSINO(IJ1)*DER(K)
410           CONTINUE
415         CONTINUE
420       CONTINUE
          DO 430 I=1,NCMP
            EPS(I) = EPS(I)-EPSIN(I)
430       CONTINUE

C ===========================================
C CALCULS COMPLEMENTAIRES 2 POUR DG
C ===========================================

C CALCUL DES DERIVEES LAGRANGIENNES DE EPSINI (DDEPSI), DE
C EPS - EPSINI (DEPS) ET DES DERIVEES SECONDES DE EPSINI (D2EPSI)
          IF (DERIVL.AND..NOT.TSENUL) THEN
            DO 432 I=1,NCMP
              DO 431 J=1,NDIM
                DDEPSI(I) = DDEPSI(I) + DEPSIN(I,J)*GRADTH(J,4)
431           CONTINUE
              DEPS(I) = DEPS(I) - DDEPSI(I)
432         CONTINUE
            DO 428 I=1,NNO
              DER(1) = DFD2DI(I)
              DER(2) = DFD2DI(I+NNO)
              DER(3) = DFD2DI(I+2*NNO)
              IJ = NCMP*(I-1)
              DO 427 J=1,NCMP
                IJ1 = IJ+J
                DO 426 K=1,NBDIV2
                  D2EPSI(J,K) = D2EPSI(J,K) + EPSINO(IJ1)*DER(K)
426             CONTINUE
427           CONTINUE
428         CONTINUE

C CALCUL DE DL(EPSII,J) (DLEPSI)
            RAUX1 = GRADTH(1,4)
            RAUX2 = GRADTH(2,4)
            DO 429 I=1,NDIM
              DLEPSI(I,1)=D2EPSI(I,1)*RAUX1+D2EPSI(I,3)*RAUX2
              DLEPSI(I,2)=D2EPSI(I,3)*RAUX1+D2EPSI(I,2)*RAUX2
429         CONTINUE
          ENDIF
        ENDIF

C =======================================================
C CALCUL DES CONTRAINTES, DE L'ENERGIE LIBRE ET DE LEURS
C DERIVEES LAGRANGIENNE
C =======================================================

        IF (INCR) THEN

C EN PLASTICITE
          CALL NMPLRU(NDIM,TYPMOD,MATCOD,COMPOR,TG,TREF,PPG,
     &                EPS,EPSP,RP,ENERGI)
          DO 435 I = 1,3
            SIGL(I)= ZR(ISIGM+NCMP*(KP-1)+I-1)
435       CONTINUE
          SIGL(4)= ZR(ISIGM+NCMP*(KP-1)+3)*RAC2
        ELSE

C EN ELASTICITE
C SI DERIVL, CALCUL DE LA DERIVEE LAGRANGIENNE DE SIGMA (DSIGL)
C ET DE CELLE DE L'ENERGIE LIBRE (DENERG).
          CRIT(1) = 300
          CRIT(2) = 0.D0
          CRIT(3) = 1.D-3
          CALL NMELNL(NDIM,TYPMOD,MATCOD,COMPOR,CRIT,TG,TREF,
     &                OPRUPT,EPS,SIGL,RBID,RBID,ENERGI,DERIVL,
     &                DLAGTG,DEPS,DENERG,DSIGL)
        ENDIF

C =======================================================
C DIVERS (DIVERGENCE, MODELISATION...)
C =======================================================

C TRAITEMENTS DEPENDANT DE LA MODELISATION
        IF(CP) THEN
          DUDM(3,3)= EPS(3)
          IF (DERIVL) GRADDU(3,3) = DEPS(3)
        ENDIF
        IF (AXI) THEN
          DUDM(3,3)= DUDM(1,4)/R
          DTDM(3,3)= DTDM(1,4)/R
          DFDM(3,3)= DFDM(1,4)/R
        ENDIF

C CALCUL DE LA DIVERGENCE DU THETA FISSURE (DIVT), DE SA DERIVEE
C LAGRANGIENNE ET DE LA DIVERGENCE DU THETA SENSIBILITE (DIVTS)
        DIVT = 0.D0
        DIVTS = 0.D0
        DLDIVT = 0.D0
        DO 437 I=1,3
          DIVT  = DIVT + DTDM(I,I)
          IF (DERIVL.AND..NOT.TSENUL) THEN
            DIVTS  = DIVTS + GRADTH(I,I)
            DLDIVT = DLDIVT + DLDTDM(I,I)
          ENDIF
437     CONTINUE

C =======================================================
C CONTRAINTES INITIALES
C =======================================================

        IF (ISIGI.NE.0) THEN
          DO 460 I=1,NNO
            I1 = I-1
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = 0.D0
            DER(4) = ZR(IVF+L+I1)

C CALCUL DE SIGMA INITIAL
            IJ = ISIGI+NCMP*I1-1
            DO 440 J=1,NCMP
              SIGIN(J) = SIGIN(J)+ ZR(IJ+J)*DER(4)
440         CONTINUE

C CALCUL DU GRADIENT DE SIGMA INITIAL
            DO 455 J=1,NCMP
              DO 450 K=1,NDIM
                DSIGIN(J,K)=DSIGIN(J,K)+ZR(IJ+J)*DER(K)
450           CONTINUE
455         CONTINUE
460       CONTINUE

C TRAITEMENTS PARTICULIERS DES TERMES CROISES
          DO 463 I=4,NCMP
            SIGIN(I) = SIGIN(I)*RAC2
            DO  462 J=1,NDIM
              DSIGIN(I,J) = DSIGIN(4,1)*RAC2
462         CONTINUE
463       CONTINUE

C CORRECTION DE SIGMA ET DE L'ENERGIE LIBRE TOTALE
          DO 464 I=1,NCMP
            SIGL(I) = SIGL(I)+ SIGIN(I)
464       CONTINUE
          DO 465 I=1,NCMP
            ENERGI(1) = ENERGI(1) + (EPS(I)+0.5D0*EPSIN(I))*SIGIN(I)
465       CONTINUE

C ===========================================
C CALCULS COMPLEMENTAIRES 3 POUR DG
C ===========================================
C CALCUL DE DL(SIGMAINI) (DDSIGI), MISE A JOUR DE DL(SIGMA+SIGMAINI)
C (DSIGL) ET DE DL(ENER) (DENERG). CALCUL DES DERIVEES SECONDES DE
C SIGMAINI (D2SIGI)
          IF (DERIVL) THEN
            DO 459 I=1,NCMP
              DENERG(1)=DENERG(1)+(DEPS(I)+0.5D0*DDEPSI(I))*SIGIN(I)
459         CONTINUE
            IF (.NOT.TSENUL) THEN
              DO 467 I=1,NCMP
                DO 466 J=1,NDIM
                  DDSIGI(I) = DDSIGI(I) + DSIGIN(I,J)*GRADTH(J,4)
466             CONTINUE
                DSIGL(I) = DSIGL(I)+ DDSIGI(I)
467           CONTINUE
              DO 468 I=1,NCMP
                DENERG(1)=DENERG(1)+(EPS(I)+0.5D0*EPSIN(I))*DDSIGI(I)
468           CONTINUE
              DO 473 I=1,NNO
                DER(1) = DFD2DI(I)
                DER(2) = DFD2DI(I+NNO)
                DER(3) = DFD2DI(I+2*NNO)
                IJ = ISIGI+NCMP*(I-1)-1
                DO 472 J=1,NCMP
                  IJ1 = IJ+J
                  DO 471 K=1,NBDIV2
                    D2SIGI(J,K) = D2SIGI(J,K) + ZR(IJ1)*DER(K)
471               CONTINUE
472             CONTINUE
473           CONTINUE

C TRAITEMENTS PARTICULIERS DES TERMES CROISES
              DO 477 J=4,NCMP
                DO  476 K=1,NBDIV2
                  D2SIGI(J,K) = D2SIGI(4,1)*RAC2
476             CONTINUE
477           CONTINUE

C CALCUL DE DL(SIGINI,J) (DLSIGI)
              RAUX1 = GRADTH(1,4)
              RAUX2 = GRADTH(2,4)
              DO 479 I=1,2
                DLSIGI(I,1)=D2SIGI(I,1)*RAUX1+D2SIGI(I,3)*RAUX2
                DLSIGI(I,2)=D2SIGI(I,3)*RAUX1+D2SIGI(I,2)*RAUX2
479           CONTINUE
            ENDIF
          ENDIF
        ENDIF
C =======================================================
C STOCKAGE DE SIGMA - SIGMAINI ET TRAITEMENTS DES TERMES CROISES
C =======================================================
        SR(1,1)= SIGL(1)
        SR(2,2)= SIGL(2)
        SR(3,3)= SIGL(3)
        SR(1,2)= SIGL(4)/RAC2
        SR(2,1)= SR(1,2)
        SR(1,3)= SIGL(5)/RAC2
        SR(3,1)= SR(1,3)
        SR(2,3)= SIGL(6)/RAC2
        SR(3,2)= SR(2,3)
        IF (DERIVL) THEN
          DSR(1,1)= DSIGL(1)
          DSR(2,2)= DSIGL(2)
          DSR(3,3)= DSIGL(3)
          DSR(1,2)= DSIGL(4)/RAC2
          DSR(2,1)= DSR(1,2)
          DSR(1,3)= DSIGL(5)/RAC2
          DSR(3,1)= DSR(1,3)
          DSR(2,3)= DSIGL(6)/RAC2
          DSR(3,2)= DSR(2,3)
        ENDIF

C CALCUL DE G ET DE LA DERIVEE DE G PAR RAPPORT A UNE VARIATION DE
C DOMAINE SI DERIVL

C =======================================================
C TERME THERMOELASTIQUE CLASSIQUE F.SIG:(GRAD(U).GRAD(THET))-ENER*DIVT
C =======================================================

        PROD  = 0.D0
        PROD2 = 0.D0
        DO 490 I=1,3
          DO 480 J=1,3
            DO 475 K=1,3
              DO 470 M=1,3
                  PROD =PROD+F(I,J)*SR(J,K)*DUDM(I,M)*DTDM(M,K)
470           CONTINUE
475         CONTINUE
480       CONTINUE
490     CONTINUE
        PROD2 = POIDS*( PROD - ENERGI(1)*DIVT)
        TCLA  = TCLA + PROD2

C ===========================================
C CALCUL DE LA DERIVEE DU TERME TCLA POUR DG
C ===========================================
        IF (DERIVL) THEN
C LE TERME DPROD1 CORRESPOND A DL(SIGMAIK*UI,M)*THFM,K
C LE TERME DPROD2 CORRESPOND A SIGMAIK*UI,M*DL(THFM,K)
C LE TERME DENERG(1) A DL(ENERGIE LIBRE)
          DPROD1 = 0.D0
          DPROD2 = 0.D0
          DPROD3 = 0.D0
          DO 494 I=1,3
            DO 493 J=1,3
              DO 492 K=1,3
                DO 491 M=1,3

                  DPROD1 = DPROD1 + F(I,J) * DTDM(M,K) *
     &            (DSR(J,K) * DUDM(I,M) + SR(J,K) * DDUDM(I,M))
                  IF (GRAND)
     &              DPROD3 = DPROD3 + DLF(I,J) * DTDM(M,K) *
     &                       SR(J,K) * DUDM(I,M)
                  IF (.NOT.TSENUL)
     &              DPROD2 = DPROD2 + F(I,J) * DLDTDM(M,K) *
     &                       SR(J,K) * DUDM(I,M)
491             CONTINUE
492           CONTINUE
493         CONTINUE
494       CONTINUE
          DTCLA  = DTCLA + POIDS * (DPROD1 - DENERG(1)*DIVT)
          IF (GRAND) DTCLA = DTCLA + POIDS * DPROD3
          IF (.NOT.TSENUL) DTCLA = DTCLA +
     &       (DPROD2 - ENERGI(1)*DLDIVT) * POIDS + PROD2 * DIVTS
        ENDIF

C =======================================================
C TERME THERMIQUE :   -(D(ENER)/DT)(GRAD(T).THETA)
C =======================================================

        PROD = 0.D0
        PROD2 = 0.D0
        DO 500 I=1,NDIM
          PROD = PROD + TGDM(I)*DTDM(I,4)
500     CONTINUE
        PROD2 = - POIDS*PROD*ENERGI(2)
        TTHE = TTHE + PROD2

C ===========================================
C CALCUL DE LA DERIVEE DU TERME TTHE POUR DG
C ===========================================

        IF (DERIVL) THEN
C LE TERME DPROD1 CORRESPOND A DL(T,I)*THFI
C LE TERME DPROD2 CORRESPOND A T,I*DL(THFI)
C LE TERME DENERG(2) A DL(DERIVEE EN T DE L'ENERGIE LIBRE)
          DPROD1 = 0.D0
          DPROD2 = 0.D0
          DO 503 I=1,NDIM
            DPROD1 = DPROD1 + DDLAGT(I)*DTDM(I,4)
            IF (.NOT.TSENUL) DPROD2 = DPROD2 + TGDM(I)*DTDM(I,5)
503       CONTINUE
          DTTHE = DTTHE - POIDS*(DPROD1*ENERGI(2)+PROD*DENERG(2))
          IF (.NOT.TSENUL) DTTHE = DTTHE - POIDS*DPROD2*ENERGI(2) +
     &                             PROD2 * DIVTS
        ENDIF

C =======================================================
C TERME FORCE VOLUMIQUE
C =======================================================

        PROD2 = 0.D0
        DO 520 I=1,NDIM
          PROD=0.D0
          DO 510 J=1,NDIM
            PROD = PROD + DFDM(I,J)*DTDM(J,4)
510       CONTINUE
          PROD2 = PROD2 + DUDM(I,4)*(PROD+DFDM(I,4)*DIVT)*POIDS
520     CONTINUE
        TFOR = TFOR + PROD2

C ===========================================
C CALCUL DE LA DERIVEE DU TERME TFOR POUR DG
C ===========================================

        IF (DERIVL) THEN
C LE TERME DPROD1 CORRESPOND A DL(UI)*(FI,J*THFJ + FI*DIV(THF))
C LE TERME DPROD2 A UI*(DL(FI,J)*THFJ+DL(FI)*DIV(THF))
C LE TERME DPROD3 A UI*(FI,J*DL(THFJ) + FI*DL(DIV(THF)))
          DPROD1 = 0.D0
          DPROD2 = 0.D0
          DPROD3 = 0.D0
          DO 517 I=1,NDIM
            DPROD1 = DPROD1 + GRADDU(I,4)*DFDM(I,4)*DIVT
            IF (.NOT.TSENUL) THEN
              DPROD2 = DPROD2 + DUDM(I,4)*DFDM(I,5)*DIVT
              DPROD3 = DPROD3 + DUDM(I,4)*DFDM(I,4)*DLDIVT
            ENDIF
            DO 516 J=1,NDIM
              DPROD1 = DPROD1 + GRADDU(I,4)*DFDM(I,J)*DTDM(J,4)
              IF (.NOT.TSENUL) THEN
                DPROD2 = DPROD2 + DUDM(I,4)*DLDFDM(I,J)*DTDM(J,4)
                DPROD3 = DPROD3 + DUDM(I,4)*DFDM(I,J)*DTDM(J,5)
              ENDIF
516         CONTINUE
517       CONTINUE
          DTFOR = DTFOR + DPROD1*POIDS
          IF (.NOT.TSENUL) DTFOR = DTFOR + (DPROD2+DPROD3)*POIDS +
     &                             PROD2*DIVTS
        ENDIF

C =======================================================
C TERME PLASTIQUE :   SIG:(GRAD(EPSP).THETA)- R(P).GRAD(P).THETA
C =======================================================

        IF (INCR) THEN
          PROD1=0.D0
          PROD2=0.D0
          DO 620 I=1,NCMP
            DO 610 J=1,NDIM
                PROD1 = PROD1 + SIGL(I)*DEPSP(I,J)*DTDM(J,4)
610         CONTINUE
620       CONTINUE
          DO 650 I=1,NDIM
            PROD2 = PROD2 + RP*DPDM(I)*DTDM(I,4)
650       CONTINUE
          TPLAS = TPLAS + (PROD1-PROD2)*POIDS
        ENDIF

C =======================================================
C TERME INITIAL:  SIG:GRAD(EPSIN).THETA-(EPS-EPSIN):GRAD(SIGIN).THETA
C =======================================================

        IF ((ISIGI.NE.0).OR.(IDEPI.NE.0).OR.EPSINI) THEN
          PROD1=0.D0
          PROD2=0.D0
          DO 670 I=1,NCMP
            DO 660 J=1,NDIM
              PROD1=PROD1+(SIGL(I)-0.5D0*SIGIN(I))*DEPSIN(I,J)*DTDM(J,4)
              PROD2=PROD2+(EPS(I) +0.5D0*EPSIN(I))*DSIGIN(I,J)*DTDM(J,4)
660         CONTINUE
670       CONTINUE
          TINI = TINI + (PROD1-PROD2)*POIDS

C ===========================================
C CALCUL DE LA DERIVEE DU TERME DTINI POUR DG
C ===========================================

          IF (DERIVL) THEN
C DPROD1 = THETAFJ*EPSINII,J*(DL(SIGI)-0.5D0DL(SIGINI))
C DPROD2 = THETAFJ*SIGINII,J*(DL(EPSI)-0.5D0DL(EPSINI))
C DPROD3 = THETAFJ * DL(EPSINII)*(DL(SIGI)-0.5D0DL(SIGINI)) -
C          THETAFJ * DL(SIGINII)*(DL(EPSI)-0.5D0DL(EPSINI))
C DPROD4 = DL(THETAFJ) * EPSINII*(DL(SIGI)-0.5D0DL(SIGINI)) -
C          DL(THETAFJ) * SIGINII*(DL(EPSI)-0.5D0DL(EPSINI))
            DPROD1 = 0.D0
            DPROD2 = 0.D0
            DPROD3 = 0.D0
            DPROD4 = 0.D0
            DO 673 I=1,NCMP
              DO 672 J=1,NDIM
                DPROD1 = DPROD1 + DEPSIN(I,J)*
     &                    (DSIGL(I)-0.5D0*DDSIGI(I))*DTDM(J,4)
                DPROD2 = DPROD2 + DSIGIN(I,J)*
     &                    (DEPS(I) +0.5D0*DDEPSI(I))*DTDM(J,4)
                IF (.NOT.TSENUL) THEN
                  DPROD3 = DPROD3 + DTDM(J,4)*(
     &                     DLEPSI(I,J)*(SIGL(I)-0.5D0*SIGIN(I))  -
     &                     DLSIGI(I,J)*(EPS(I) +0.5D0*EPSIN(I)))
                  DPROD4 = DPROD4 + DTDM(J,5)*(
     &                     DEPSIN(I,J)*(SIGL(I)-0.5D0*SIGIN(I))  -
     &                     DSIGIN(I,J)*(EPS(I) +0.5D0*EPSIN(I)))
                ENDIF
672           CONTINUE
673         CONTINUE
            DTINI = DTINI + (DPROD1-DPROD2)*POIDS
            IF (.NOT.TSENUL) DTINI = DTINI + DPROD3*POIDS +
     &                               (PROD1-PROD2)*POIDS*DIVTS
          ENDIF
        ENDIF

C ==================================================================
C FIN DE BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
C ==================================================================
800   CONTINUE

C EXIT EN CAS DE THETA FISSURE NUL PARTOUT
9999  CONTINUE

C ASSEMBLAGE FINAL DES TERMES DE G
       ZR(IGTHET) = TTHE+TCLA+TFOR+TPLAS+TINI

C ASSEMBLAGE FINAL DES TERMES DE DLAG(G)
       IF (DERIVL) ZR(IDGTHE) = DTTHE+DTCLA+DTFOR+DTPLAS+DTINI

      CALL JEDEMA()
      END
