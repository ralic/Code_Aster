      SUBROUTINE TE0600(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C =====================================================================
C MODIF ELEMENTS  DATE 03/07/2006   AUTEUR MEUNIER S.MEUNIER 
C RESPONSABLE UFBHHLL C.CHAVANT
C =====================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C =====================================================================
C TOLE CRP_20
C TOLE CRP_21
C =====================================================================
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          ELEMENTS THHM  ET HM
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C =====================================================================
      INTEGER JGANO,NNO,IMATUU,NDIM,IMATE,IINSTM,IFORC,JCRET
      INTEGER IPOID2,IVF2
      INTEGER IDFDE2,NPI,NPG,NVIM
C
      INTEGER RETLOI,IRET,IRETP,IRETM
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IDEFO
      INTEGER IINSTP,IDEPLM,IDEPLP,IDEPLA,ICOMPO,ICARCR,IPESA
      INTEGER ICONTM,IVARIP,IVARIM,ITREF,IVECTU,ICONTP
C =====================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C =====================================================================
      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMUEL
      INTEGER DIMDEP,DIMDEF,DIMCON,NBVARI,NDDLS,NDDLM,II,INO
      INTEGER NMEC,NP1,NP2,I,NCMP,NNOS,ICHG,ICHN
      INTEGER JTAB(7),IGAU,ISIG,NNOM
C     REMARQUE : CES DIMENSIONS DOIVENT ETRE LES MEMES QUE DANS TE0492
      REAL*8 DEFGEP(21),DEFGEM(21)
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),POIDS
      REAL*8 DFDI(20,3),DFDI2(20,3),B(21,120),EPSM(405),EPSNO(405)
      REAL*8 DRDS(22,31),DRDSR(21,31),DSDE(31,21)
      REAL*8 R(22),SIGBAR(21),C(21),CK(21),CS(21)
      CHARACTER*3 MODINT
      CHARACTER*8 TYPMOD(2)
      CHARACTER*16 PHENOM
C =====================================================================
      INTEGER     ISMAEM,LI,KP,J,L,K
      REAL*8      R8BID,RHO,COEF,RX
      CHARACTER*2 CODRET(1)
      LOGICAL     AXI, PERMAN
C =====================================================================
C  CETTE ROUTINE FAIT UN CALCUL EN THHM , HM , HHM , THH
C  21 = 9 DEF MECA + 4 POUR P1 + 4 POUR P2 + 4 POUR T
C  31 = 7 MECA + 2*5 POUR P1 + 2*5 POUR P2 + 4 POUR T
C =====================================================================
C  POUR LES TABLEAUX DEFGEP ET DEFGEM ON A DANS L'ORDRE :
C                                      DX DY DZ
C                                      EPXX EPYY EPZZ EPXY EPXZ EPYZ
C                                      PRE1 P1DX P1DY P1DZ
C                                      PRE2 P2DX P2DY P2DZ
C                                      TEMP TEDX TEDY TEDZ
C            EPSXY = RAC2/2*(DU/DY+DV/DX)
C =====================================================================
C    POUR LES CHAMPS DE CONTRAINTE
C                                      SIXX SIYY SIZZ SIXY SIXZ SIYZ
C                                      SIP
C                                      M11 FH11X FH11Y FH11Z
C                                      ENT11
C                                      M12 FH12X FH12Y FH12Z
C                                      ENT12
C                                      M21 FH21X FH21Y FH21Z
C                                      ENT21
C                                      M22 FH22X FH22Y FH22Z
C                                      ENT22
C                                      QPRIM FHTX FHTY FHTZ
C        SIXY EST LE VRAI DE LA MECANIQUE DES MILIEUX CONTINUS
C        DANS EQUTHM ON LE MULITPLIERA PAR RAC2
C =====================================================================
C   POUR L'OPTION FORCNODA
C  SI LES TEMPS PLUS ET MOINS SONT PRESENTS
C  C'EST QUE L'ON APPELLE DEPUIS STAT NON LINE  : FNOEVO = VRAI
C  ET ALORS LES TERMES DEPENDANT DE DT SONT EVALUES
C  SI LES TEMPS PLUS ET MOINS NE SONT PAS PRESENTS
C  C'EST QUE L'ON APPELLE DEPUIS CALCNO  : FNOEVO = FAUX
C  ET ALORS LES TERMES DEPENDANT DE DT NE SONT PAS EVALUES
C =====================================================================
C AXI       AXISYMETRIQUE?
C TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
C MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
C NNO       NB DE NOEUDS DE L'ELEMENT
C NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
C NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
C NDDLS     NB DE DDL SUR LES SOMMETS
C NDDLM     NB DE DDL SUR LES MILIEUX
C NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
C NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
C                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
C                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
C NDIM      DIMENSION DE L'ESPACE
C DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
C DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
C DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
C IVF       FONCTIONS DE FORMES QUADRATIQUES
C IVF2      FONCTIONS DE FORMES LINEAIRES
C =====================================================================
      LOGICAL FNOEVO
      REAL*8  DT
C =====================================================================
C --- 1. INITIALISATIONS ----------------------------------------------
C --- SUIVANT ELEMENT, DEFINITION DES CARACTERISTIQUES : --------------
C --- CHOIX DU TYPE D'INTEGRATION -------------------------------------
C --- RECUPERATION DE LA GEOMETRIE ET POIDS DES POINTS D'INTEGRATION --
C --- RECUPERATION DES FONCTIONS DE FORME -----------------------------
C =====================================================================
      CALL CAETHM(NOMTE,AXI,PERMAN,
     >            TYPMOD,MODINT,MECANI,PRESS1,PRESS2,TEMPE,
     >            DIMDEP,DIMDEF,DIMCON,NMEC,NP1,NP2,NDIM,NNO,
     >            NNOS,NNOM,NPI,NPG,NDDLS,NDDLM,DIMUEL,
     >            IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,JGANO)
C =====================================================================
C --- DEBUT DES DIFFERENTES OPTIONS -----------------------------------
C =====================================================================
C --- 2. OPTIONS : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA -------------
C =====================================================================
      IF ((OPTION(1:9).EQ.'RIGI_MECA' ) .OR.
     +    (OPTION(1:9).EQ.'RAPH_MECA' ) .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA' )) THEN
C =====================================================================
C --- PARAMETRES EN ENTREE --------------------------------------------
C =====================================================================
         CALL JEVECH('PGEOMER','L',IGEOM )
         CALL JEVECH('PMATERC','L',IMATE )
         CALL JEVECH('PINSTMR','L',IINSTM)
         CALL JEVECH('PINSTPR','L',IINSTP)
         CALL JEVECH('PDEPLMR','L',IDEPLM)
         CALL JEVECH('PDEPLPR','L',IDEPLP)
         CALL JEVECH('PCOMPOR','L',ICOMPO)
         CALL JEVECH('PCARCRI','L',ICARCR)
         CALL JEVECH('PVARIMR','L',IVARIM)
         CALL JEVECH('PCONTMR','L',ICONTM)
         CALL JEVECH('PTEREF', 'L',ITREF )




C        UN EXEMPLE DE RECUPERATION DU MATERIAU AUX NOEUDS :
C        ----------------------------------------------------
C          CALL JEVECH('PMATERN','L',IMATN )
C          DO INO=1,6
C            TEMP=10*INO
C            WRITE(6,*) 'AJACOT INO,TEMP,=',INO,TEMP
C            CALL RCVALA(ZI(IMATE-1+1),' ','ELAS',
C    +             1,'TEMP',TEMP,
C    +             1,'ALPHA',ALPHA,CODRET,'FM')
C            WRITE(6,*) 'AJACOT MAILLE,ALPHA,CODRET=',INO,ALPHA,CODRET
C 
C            CALL RCVALA(ZI(IMATN-1+INO),' ','ELAS',
C    +             1,'TEMP',TEMP,
C    +             1,'ALPHA',ALPHA,CODRET,'FM')
C            WRITE(6,*) 'AJACOT INO,ALPHA,CODRET=',INO,ALPHA,CODRET
C          ENDDO
C          CALL ASSERT(.FALSE.)







         READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI
C =====================================================================
C --- PARAMETRES EN SORTIE ISMAEM? ------------------------------------
C =====================================================================
         IF (OPTION(1:9).EQ.'RIGI_MECA' .OR.
     +       OPTION(1:9).EQ.'FULL_MECA') THEN
            CALL JEVECH('PMATUNS','E',IMATUU)
         ELSE
            IMATUU = ISMAEM()
         END IF

         IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     +       OPTION(1:9).EQ.'FULL_MECA') THEN
            CALL JEVECH('PVECTUR','E',IVECTU)
            CALL JEVECH('PCONTPR','E',ICONTP)
            CALL JEVECH('PVARIPR','E',IVARIP)
            CALL JEVECH('PCODRET','E',JCRET)
            ZI(JCRET) = 0
         ELSE
            IVECTU = ISMAEM()
            ICONTP = ISMAEM()
            IVARIP = ISMAEM()
         END IF

         RETLOI = 0

         IF (OPTION(1:9).EQ.'RIGI_MECA') THEN

            CALL ASSTHM(NNO,NNOS,NNOM,NPG,NPI,IPOIDS,IPOID2,
     +                IVF,IVF2,IDFDE, IDFDE2,
     +                ZR(IGEOM),ZR(ICARCR),
     +                ZR(IDEPLM),ZR(IDEPLM),ZR(ICONTM),
     +                ZR(ICONTM),ZR(IVARIM),ZR(IVARIM),DEFGEM,DEFGEP,
     +                DRDS,DRDSR,DSDE,B,DFDI, DFDI2,R,SIGBAR,C,CK,CS,
     +                ZR(IMATUU),ZR(IVECTU),
     +                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     +                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,DIMUEL, 
     +                NBVARI,NDDLS,NDDLM,
     +                NMEC,NP1,NP2,NDIM,ZK16(ICOMPO),
     >                TYPMOD,AXI,PERMAN,MODINT,
     +                RETLOI)
         ELSE
            DO 30 LI = 1,DIMUEL
               ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
 30         CONTINUE

            CALL ASSTHM(NNO,NNOS,NNOM,NPG,NPI,IPOIDS,IPOID2,
     +                IVF,IVF2,IDFDE, IDFDE2,
     +                ZR(IGEOM),ZR(ICARCR),
     +                ZR(IDEPLM),ZR(IDEPLP),ZR(ICONTM),
     +                ZR(ICONTP),ZR(IVARIM),ZR(IVARIP),DEFGEM,DEFGEP,
     +                DRDS,DRDSR,DSDE,B,DFDI, DFDI2,R,SIGBAR,C,CK,CS,
     +                ZR(IMATUU),ZR(IVECTU),
     +                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     +                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,DIMUEL, 
     +                NBVARI,NDDLS,NDDLM,
     +                NMEC,NP1,NP2,NDIM,ZK16(ICOMPO),
     >                TYPMOD,AXI,PERMAN,MODINT,
     +                RETLOI)
            ZI(JCRET) = RETLOI
         END IF
      END IF
C =====================================================================
C --- 3. OPTION : CHAR_MECA_PESA_R ------------------------------------
C =====================================================================
      IF (OPTION.EQ.'CHAR_MECA_PESA_R') THEN
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL JEVECH('PMATERC','L',IMATE)
         CALL JEVECH('PPESANR','L',IPESA)
         CALL JEVECH('PVECTUR','E',IVECTU)

         CALL RCCOMA(ZI(IMATE),'THM_DIFFU',PHENOM,CODRET)
         CALL RCVALA(ZI(IMATE),' ',PHENOM,1,' ',R8BID,1,'RHO',RHO,
     +                                                    CODRET,'FM')
         IF (NDIM.EQ.3) THEN
C =====================================================================
C --- CAS 3D ----------------------------------------------------------
C =====================================================================
            DO 40 I = 1,DIMUEL
               ZR(IVECTU+I-1) = 0.0D0
 40         CONTINUE
C =====================================================================
C --- BOUCLE SUR LES POINTS DE GAUSS ----------------------------------
C =====================================================================
            DO 70 KP = 1,NPG
               L = (KP-1)*NNO
               CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     +                             ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
               COEF = RHO*POIDS*ZR(IPESA)
               DO 60 I = 1,NNOS
                  II = NDDLS* (I-1)
                  DO 50 J = 1,3
                     ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     +                                  COEF*ZR(IVF+L+I-1)*ZR(IPESA+J)
 50               CONTINUE
 60            CONTINUE
               DO 65 I = 1,NNOM
                  II = NNOS*NDDLS+NDDLM*(I-1)
                  DO 55 J = 1,3
                     ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     +                             COEF*ZR(IVF+L+I+NNOS-1)*ZR(IPESA+J)
 55               CONTINUE
 65            CONTINUE
 70         CONTINUE
         ELSE
C =====================================================================
C --- CAS 2D ----------------------------------------------------------
C =====================================================================
            DO 110 KP = 1,NPG
               K = (KP-1)*NNO
               CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,
     +                                                           POIDS)
               POIDS = POIDS*RHO*ZR(IPESA)
               IF (TYPMOD(1).EQ.'AXIS    ') THEN
                  RX = 0.D0
                  DO 80 I = 1,NNO
                     RX = RX + ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
 80               CONTINUE
                  POIDS = POIDS*RX
                  DO 90 I = 1,NNOS
                     K = (KP-1)*NNO
                     ZR(IVECTU+NDDLS*(I-1)+1)=ZR(IVECTU+NDDLS*(I-1)+1)
     +                                +POIDS*ZR(IPESA+2)*ZR(IVF+K+I-1)
 90               CONTINUE
                  DO 95 I = 1,NNOM
                     K = (KP-1)*NNO
                     ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1) =
     +               ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1) +
     +                            POIDS*ZR(IPESA+2)*ZR(IVF+K+I+NNOS-1)
 95               CONTINUE   
               ELSE

                  DO 100 I = 1,NNOS
                     K = (KP-1)*NNO
                     ZR(IVECTU+NDDLS*(I-1)) = ZR(IVECTU+NDDLS*(I-1))
     +                                +POIDS*ZR(IPESA+1)*ZR(IVF+K+I-1)
                     ZR(IVECTU+NDDLS*(I-1)+1)=ZR(IVECTU+NDDLS*(I-1)+1)
     +                                +POIDS*ZR(IPESA+2)*ZR(IVF+K+I-1)
 100              CONTINUE
                  DO 400 I = 1,NNOM
                     K = (KP-1)*NNO
                     ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1))=
     +               ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)) +
     +                            POIDS*ZR(IPESA+1)*ZR(IVF+K+I+NNOS-1)
                     ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1)=
     +               ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1) +
     +                            POIDS*ZR(IPESA+2)*ZR(IVF+K+I+NNOS-1)
  400             CONTINUE
               END IF
  110       CONTINUE
         END IF
      END IF
C =====================================================================
C --- 4. OPTION : CHAR_MECA_FR3D3D ------------------------------------
C =====================================================================
      IF (OPTION.EQ.'CHAR_MECA_FR3D3D') THEN
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL JEVECH('PVECTUR','E',IVECTU)
         CALL JEVECH('PFR3D3D','L',IFORC)

         DO 120 I = 1,DIMUEL
            ZR(IVECTU+I-1) = 0.0D0
 120     CONTINUE
C ======================================================================
C --- BOUCLE SUR LES POINTS DE GAUSS -----------------------------------
C ======================================================================
         DO 150 KP = 1,NPG
            L = (KP-1)*NNO
            CALL DFDM3D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,DFDZ,
     +                                                            POIDS)

            DO 140 I = 1,NNOS
               II = NDDLS* (I-1)
               DO 130 J = 1,3
                  ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     +                                 POIDS*ZR(IVF+L+I-1)*ZR(IFORC+J-1)
 130           CONTINUE
 140        CONTINUE
             DO 145 I = 1,NNOM
               II = NNOS*NDDLS+NDDLS*(I-1)
               DO 135 J = 1,3
                  ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     +                            POIDS*ZR(IVF+L+I+NNOS-1)*ZR(IFORC+J-1)
 135           CONTINUE
 145        CONTINUE
 150     CONTINUE
      END IF
C ======================================================================
C --- 5. OPTION : CHAR_MECA_FR2D2D -------------------------------------
C ======================================================================
      IF (OPTION.EQ.'CHAR_MECA_FR2D2D') THEN
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL JEVECH('PFR2D2D','L',IFORC)
         CALL JEVECH('PVECTUR','E',IVECTU)

         DO 180 KP = 1,NPG
            K = (KP-1)*NNO
            CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
            IF (TYPMOD(1).EQ.'AXIS    ') THEN
               RX = 0.D0
               DO 160 I = 1,NNO
                  RX = RX + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
 160           CONTINUE
               POIDS = POIDS*RX
            END IF
            DO 170 I = 1,NNOS
               K = (KP-1)*NNO
               L = (KP-1)*2
               ZR(IVECTU+NDDLS*(I-1)) = ZR(IVECTU+NDDLS*(I-1)) +
     +                                   POIDS*ZR(IFORC+L)*ZR(IVF+K+I-1)
               ZR(IVECTU+NDDLS*(I-1)+1) = ZR(IVECTU+NDDLS*(I-1)+1) +
     +                                 POIDS*ZR(IFORC+L+1)*ZR(IVF+K+I-1)
 170        CONTINUE
            DO 171 I = 1,NNOM
               K = (KP-1)*NNO
               L = (KP-1)*2
               ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1))=
     +         ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)) +
     +                              POIDS*ZR(IFORC+L)*ZR(IVF+K+I+NNOS-1)
               ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1) =
     +         ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1) +
     +                            POIDS*ZR(IFORC+L+1)*ZR(IVF+K+I+NNOS-1)
 171        CONTINUE 
 180     CONTINUE
      END IF
C ======================================================================
C --- 6. OPTION : FORC_NODA --------------------------------------------
C ======================================================================
      IF (OPTION.EQ.'FORC_NODA') THEN
C ======================================================================
C --- PARAMETRES EN ENTREE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PCONTMR','L',ICONTM)
        CALL JEVECH('PMATERC','L',IMATE)
C ======================================================================
C --- SI LES TEMPS PLUS ET MOINS SONT PRESENTS -------------------------
C --- C EST QUE L ON APPELLE DEPUIS STAT NON LINE ET -------------------
C --- ALORS LES TERMES DEPENDANT DE DT SONT EVALUES --------------------
C ======================================================================
        CALL TECACH('ONN','PINSTMR ',1,IINSTM,IRETM)
        CALL TECACH('ONN','PINSTPR ',1,IINSTP,IRETP)
        IF (IRETM.EQ.0 .AND. IRETP.EQ.0) THEN
           DT = ZR(IINSTP) - ZR(IINSTM)
           FNOEVO = .TRUE.
        ELSE
           FNOEVO = .FALSE.
           DT = 0.D0
        ENDIF
C ======================================================================
C --- PARAMETRES EN SORTIE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PVECTUR','E',IVECTU)

        CALL FNOTHM(FNOEVO,DT,PERMAN,NNO,NNOS,NNOM,NPI,
     +              NPG,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,
     +              ZR(IGEOM),ZR(ICONTM),B,DFDI,DFDI2,
     +              R,ZR(IVECTU),ZI(IMATE),MECANI,PRESS1,PRESS2,
     +              TEMPE,DIMDEF,DIMCON,NDDLS,NDDLM,DIMUEL,
     +              NMEC,NP1,NP2,NDIM,AXI)
      END IF
C ======================================================================
C --- 7. OPTION : REFE_FORC_NODA ---------------------------------------
C ======================================================================
      IF (OPTION.EQ.'REFE_FORC_NODA') THEN
C ======================================================================
C --- ON RAPPELLE QUE LES PARAMETRES DU CRITERE DE CONVERGENCE SONT ----
C --- STOCKES DE LA FACON SUIVANTE : (1) : SIGM_REFE -------------------
C ---------------------------------- (3) : FLUX_THER_REFE --------------
C ---------------------------------- (4) : FLUX_HYD1_REFE --------------
C ---------------------------------- (5) : FLUX_HYD2_REFE --------------
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
        DT = 1.0D0
        FNOEVO = .TRUE.
C ======================================================================
C --- PARAMETRES EN ENTREE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PREFCO','L',ICONTM)
C ======================================================================
C --- PARAMETRES EN SORTIE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PVECTUR','E',IVECTU)
C ======================================================================
C --- APPEL A LA ROUTINE SUR LES CRITERES DE CONVERGENCE ---------------
C ======================================================================
        CALL REFTHM(FNOEVO,DT,PERMAN,NNO,NNOS,NNOM,NPI,NPG,
     &              IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,ZR(IGEOM),
     &              B,DFDI,DFDI2,R,ZR(IVECTU),ZI(IMATE),MECANI,
     &              PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,DIMUEL,
     &              NDDLS,NDDLM,NMEC,NP1,NP2,NDIM,AXI,
     &              ZR(ICONTM))
      END IF
C ======================================================================
C --- 8. OPTION : SIEF_ELNO_ELGA ---------------------------------------
C ======================================================================
      IF (OPTION.EQ.'SIEF_ELNO_ELGA  ') THEN
         NCMP = DIMCON
         CALL JEVECH('PCONTRR', 'L',ICHG)
         CALL JEVECH('PSIEFNOR','E',ICHN)
         
         NVIM = MECANI(5)
         CALL POSTHM(OPTION,MODINT,JGANO,NCMP,NVIM,ZR(ICHG),ZR(ICHN))
      ENDIF
C ======================================================================
C --- 9. OPTION : VARI_ELNO_ELGA ---------------------------------------
C ======================================================================
      IF (OPTION.EQ.'VARI_ELNO_ELGA  ') THEN
         CALL JEVECH('PVARIGR','L',ICHG)
         CALL JEVECH('PVARINR','E',ICHN)
         
         CALL JEVECH('PCOMPOR','L',ICOMPO)
         READ (ZK16(ICOMPO+1),'(I16)') NCMP
         READ (ZK16(ICOMPO-1+7+9+4),'(I16)') NVIM
         CALL TECACH('OON','PVARIGR',7,JTAB,IRET)
         
         CALL POSTHM(OPTION,MODINT,JGANO,NCMP,NVIM,ZR(ICHG),ZR(ICHN))
      END IF
C ======================================================================
C --- 10. OPTION : EPSI_ELGA_DEPL OU EPSI_ELNO_DEPL --------------------
C ======================================================================
      IF ((OPTION.EQ.'EPSI_ELGA_DEPL') .OR.
     &    (OPTION.EQ.'EPSI_ELNO_DEPL')) THEN
     
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL JEVECH('PDEPLAR','L',IDEPLA)
         CALL JEVECH('PDEFORR','E',IDEFO)

         CALL EPSTHM ( NDDLS, NDDLM, NNO, NNOS, NNOM,NMEC,
     &                 DIMDEF, DIMUEL, NDIM, NPI,
     &                 IPOIDS, IPOID2, IVF, IVF2,
     &                 IDFDE, IDFDE2, DFDI, DFDI2, B,
     &                 ZR(IGEOM), ZR(IDEPLA),
     &                 MECANI, PRESS1, PRESS2, TEMPE,
     &                 NP1, NP2, AXI, EPSM )

         IF (OPTION(6:9).EQ.'ELGA') THEN
            DO 200 IGAU = 1,NPI
               DO 210 ISIG = 1,6
                  ZR(IDEFO+6*(IGAU-1)+ISIG-1) = EPSM(6*(IGAU-1)+ISIG)
  210          CONTINUE
  200       CONTINUE
         ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
            CALL PPGAN2(JGANO,6,EPSM,EPSNO)
            DO 300 INO = 1,NNO
               DO 310 ISIG = 1,6
                  ZR(IDEFO+6*(INO-1)+ISIG-1) = EPSNO(6*(INO-1)+ISIG)
  310          CONTINUE
  300       CONTINUE
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
      ENDIF
C ======================================================================
      END
