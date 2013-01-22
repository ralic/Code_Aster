      SUBROUTINE TE0600(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C =====================================================================
C MODIF ELEMENTS  DATE 21/01/2013   AUTEUR DELMAS J.DELMAS 
C RESPONSABLE GRANET S.GRANET
C =====================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          ELEMENTS THHM, HM ET HH
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C =====================================================================
      INTEGER JGANO,NNO,IMATUU,NDIM,IMATE,IINSTM,JCRET
      INTEGER IPOID2,IVF2
      INTEGER IDFDE2,NPI,NPG,NVIM
C
      INTEGER RETLOI,IRET,IRETP,IRETM
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IDEFO
      INTEGER IINSTP,IDEPLM,IDEPLP,IDEPLA,ICOMPO,ICARCR,IPESA
      INTEGER ICONTM,IVARIP,IVARIM,IVECTU,ICONTP
C =====================================================================
C =====================================================================
      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMUEL
      INTEGER DIMDEP,DIMDEF,DIMCON,NBVARI,NDDLS,NDDLM,II
      INTEGER NMEC,NP1,NP2,I,NCMP,NNOS,ICHG,ICHN
      INTEGER JTAB(7),IGAU,ISIG,NNOM
      REAL*8 DEFGEP(21),DEFGEM(21),DFDBID(27),POIDS
      REAL*8 DFDI(20,3),DFDI2(20,3),B(21,120),EPSM(405)
      REAL*8 DRDS(22,31),DRDSR(21,31),DSDE(31,21)
      REAL*8 R(22),SIGBAR(21),C(21),CK(21),CS(21)
      CHARACTER*3 MODINT
      CHARACTER*8 TYPMOD(2)
      CHARACTER*16 PHENOM
C =====================================================================
      INTEGER     ISMAEM,LI,KP,J,L,K,IBID,TYPVF
      REAL*8      R8BID,RHO,COEF,RX
      INTEGER ICODRE(1)
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
      LOGICAL FNOEVO,VF
      REAL*8  DT
C =====================================================================
C --- 1. INITIALISATIONS ----------------------------------------------
C --- SUIVANT ELEMENT, DEFINITION DES CARACTERISTIQUES : --------------
C --- CHOIX DU TYPE D'INTEGRATION -------------------------------------
C --- RECUPERATION DE LA GEOMETRIE ET POIDS DES POINTS D'INTEGRATION --
C --- RECUPERATION DES FONCTIONS DE FORME -----------------------------
C =====================================================================
      IBID = 0
      CALL CAETHM(NOMTE,AXI,PERMAN,VF,TYPVF,
     &            TYPMOD,MODINT,MECANI,PRESS1,PRESS2,TEMPE,
     &            DIMDEP,DIMDEF,DIMCON,NMEC,NP1,NP2,NDIM,NNO,
     &            NNOS,NNOM,IBID,
     &            NPI,NPG,NDDLS,NDDLM,IBID,IBID,DIMUEL,
     &            IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,IBID,JGANO)
C =====================================================================
C --- DEBUT DES DIFFERENTES OPTIONS -----------------------------------
C =====================================================================
C --- 2. OPTIONS : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA -------------
C =====================================================================
      IF ((OPTION(1:9).EQ.'RIGI_MECA' ) .OR.
     &    (OPTION(1:9).EQ.'RAPH_MECA' ) .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA' )) THEN
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
         READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI
C =====================================================================
C --- PARAMETRES EN SORTIE ISMAEM? ------------------------------------
C =====================================================================
         IF (OPTION(1:9).EQ.'RIGI_MECA' .OR.
     &       OPTION(1:9).EQ.'FULL_MECA') THEN
            CALL JEVECH('PMATUNS','E',IMATUU)
         ELSE
            IMATUU = ISMAEM()
         END IF
         IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &       OPTION(1:9).EQ.'FULL_MECA') THEN
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
     &                IVF,IVF2,IDFDE, IDFDE2,
     &                ZR(IGEOM),ZR(ICARCR),
     &                ZR(IDEPLM),ZR(IDEPLM),ZR(ICONTM),
     &                ZR(ICONTM),ZR(IVARIM),ZR(IVARIM),DEFGEM,DEFGEP,
     &                DRDS,DRDSR,DSDE,B,DFDI, DFDI2,R,SIGBAR,C,CK,CS,
     &                ZR(IMATUU),ZR(IVECTU),
     &                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     &                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,DIMUEL,
     &                NBVARI,NDDLS,NDDLM,
     &                NMEC,NP1,NP2,NDIM,ZK16(ICOMPO),
     &                TYPMOD,AXI,PERMAN,MODINT,
     &                RETLOI)
         ELSE
            DO 30 LI = 1,DIMUEL
               ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
 30         CONTINUE
            CALL ASSTHM(NNO,NNOS,NNOM,NPG,NPI,IPOIDS,IPOID2,
     &                IVF,IVF2,IDFDE, IDFDE2,
     &                ZR(IGEOM),ZR(ICARCR),
     &                ZR(IDEPLM),ZR(IDEPLP),ZR(ICONTM),
     &                ZR(ICONTP),ZR(IVARIM),ZR(IVARIP),DEFGEM,DEFGEP,
     &                DRDS,DRDSR,DSDE,B,DFDI, DFDI2,R,SIGBAR,C,CK,CS,
     &                ZR(IMATUU),ZR(IVECTU),
     &                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     &                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,DIMUEL,
     &                NBVARI,NDDLS,NDDLM,
     &                NMEC,NP1,NP2,NDIM,ZK16(ICOMPO),
     &                TYPMOD,AXI,PERMAN,MODINT,
     &                RETLOI)
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
         CALL RCCOMA(ZI(IMATE),'THM_DIFFU',1,PHENOM,ICODRE)
         CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ',PHENOM,0,' ',R8BID,
     &               1,'RHO',RHO,ICODRE,1)
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
     &                     ZR(IGEOM), DFDBID, DFDBID, DFDBID, POIDS )
               COEF = RHO*POIDS*ZR(IPESA)
               DO 60 I = 1,NNOS
                  II = NDDLS* (I-1)
                  DO 50 J = 1,3
                     ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     &                                  COEF*ZR(IVF+L+I-1)*ZR(IPESA+J)
 50               CONTINUE
 60            CONTINUE
               DO 65 I = 1,NNOM
                  II = NNOS*NDDLS+NDDLM*(I-1)
                  DO 55 J = 1,3
                     ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     &                             COEF*ZR(IVF+L+I+NNOS-1)*ZR(IPESA+J)
 55               CONTINUE
 65            CONTINUE
 70         CONTINUE
         ELSE
C =====================================================================
C --- CAS 2D ----------------------------------------------------------
C =====================================================================
            DO 110 KP = 1,NPG
               K = (KP-1)*NNO
               CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDBID,DFDBID,
     &                                                           POIDS)
               POIDS = POIDS*RHO*ZR(IPESA)
               IF (AXI) THEN
                  RX = 0.D0
                  DO 80 I = 1,NNO
                     RX = RX + ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
 80               CONTINUE
                  POIDS = POIDS*RX
                  DO 90 I = 1,NNOS
                     ZR(IVECTU+NDDLS*(I-1)+1)=ZR(IVECTU+NDDLS*(I-1)+1)
     &                                +POIDS*ZR(IPESA+2)*ZR(IVF+K+I-1)
 90               CONTINUE
                  DO 95 I = 1,NNOM
                     ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1) =
     &               ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1) +
     &                            POIDS*ZR(IPESA+2)*ZR(IVF+K+I+NNOS-1)
 95               CONTINUE
               ELSE

                  DO 100 I = 1,NNOS
                     ZR(IVECTU+NDDLS*(I-1)) = ZR(IVECTU+NDDLS*(I-1))
     &                                +POIDS*ZR(IPESA+1)*ZR(IVF+K+I-1)
                     ZR(IVECTU+NDDLS*(I-1)+1)=ZR(IVECTU+NDDLS*(I-1)+1)
     &                                +POIDS*ZR(IPESA+2)*ZR(IVF+K+I-1)
 100              CONTINUE
                  DO 400 I = 1,NNOM
                     ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1))=
     &               ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)) +
     &                            POIDS*ZR(IPESA+1)*ZR(IVF+K+I+NNOS-1)
                     ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1)=
     &               ZR(IVECTU+NDDLS*NNOS+NDDLM*(I-1)+1) +
     &                            POIDS*ZR(IPESA+2)*ZR(IVF+K+I+NNOS-1)
  400             CONTINUE
               END IF
  110       CONTINUE
         END IF
      END IF

C =====================================================================
C --- 4. OPTIONS : CHAR_MECA_FR2D2D OU CHAR_MECA_FR3D3D ---------------
C =====================================================================

      CALL THMEVC(OPTION,NOMTE,AXI  ,NNO,NPG,IPOIDS,
     &            IVF   ,IDFDE,NDDLS,
     &            NNOS  ,NDDLM,NNOM  )

C ======================================================================
C --- 5. OPTION : FORC_NODA --------------------------------------------
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
        CALL TECACH('ONN','PINSTMR','L',1,IINSTM,IRETM)
        CALL TECACH('ONN','PINSTPR','L',1,IINSTP,IRETP)
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
     &              NPG,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,
     &              ZR(IGEOM),ZR(ICONTM),B,DFDI,DFDI2,
     &              R,ZR(IVECTU),ZI(IMATE),MECANI,PRESS1,PRESS2,
     &              TEMPE,DIMDEF,DIMCON,NDDLS,NDDLM,DIMUEL,
     &              NMEC,NP1,NP2,NDIM,AXI)
      END IF
C ======================================================================
C --- 6. OPTION : REFE_FORC_NODA ---------------------------------------
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
     &              NDDLS,NDDLM,NMEC,NP1,NP2,NDIM,AXI)
      END IF
C ======================================================================
C --- 7. OPTION : SIEF_ELNO --------------------------------------------
C ======================================================================
      IF (OPTION.EQ.'SIEF_ELNO  ') THEN
         NCMP = DIMCON
         CALL JEVECH('PCONTRR', 'L',ICHG)
         CALL JEVECH('PSIEFNOR','E',ICHN)
         NVIM = MECANI(5)
         CALL POSTHM(OPTION,MODINT,JGANO,NCMP,NVIM,ZR(ICHG),ZR(ICHN))
      ENDIF
C ======================================================================
C --- 8. OPTION : VARI_ELNO --------------------------------------------
C ======================================================================
      IF (OPTION.EQ.'VARI_ELNO  ') THEN
         CALL JEVECH('PVARIGR','L',ICHG)
         CALL JEVECH('PVARINR','E',ICHN)

         CALL JEVECH('PCOMPOR','L',ICOMPO)
         READ (ZK16(ICOMPO+1),'(I16)') NCMP
         READ (ZK16(ICOMPO-1+7+9+4),'(I16)') NVIM
         CALL TECACH('OON','PVARIGR','L',7,JTAB,IRET)

         CALL POSTHM(OPTION,MODINT,JGANO,NCMP,NVIM,ZR(ICHG),ZR(ICHN))
      END IF
C ======================================================================
C --- 9. OPTION : EPSI_ELGA --------------------------------------------
C ======================================================================
      IF (OPTION.EQ.'EPSI_ELGA') THEN
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL JEVECH('PDEPLAR','L',IDEPLA)
         CALL JEVECH('PDEFOPG','E',IDEFO)
         CALL EPSTHM ( NDDLS, NDDLM, NNO, NNOS, NNOM,NMEC,
     &                 DIMDEF, DIMUEL, NDIM, NPI,
     &                 IPOIDS, IPOID2, IVF, IVF2,
     &                 IDFDE, IDFDE2, DFDI, DFDI2, B,
     &                 ZR(IGEOM), ZR(IDEPLA),
     &                 MECANI, PRESS1, PRESS2, TEMPE,
     &                 NP1, NP2, AXI, EPSM )

         DO 200 IGAU = 1,NPI
            DO 210 ISIG = 1,6
               ZR(IDEFO+6*(IGAU-1)+ISIG-1) = EPSM(6*(IGAU-1)+ISIG)
  210       CONTINUE
  200    CONTINUE
      ENDIF
C ======================================================================
      END
