      SUBROUTINE TE0047(OPTIOZ,NOMTEZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 OPTION,NOMTE
      CHARACTER*(*) OPTIOZ,NOMTEZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/10/2004   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C TOLE  CRP_20

C     ELEMENTS CONCERNES :  MECA_DIS_TR_L : SUR UNE MAILLE A 2 NOEUDS
C                           MECA_DIS_T_L  : SUR UNE MAILLE A 2 NOEUDS
C                           MECA_DIS_TR_N : SUR UNE MAILLE A 1 NOEUD
C                           MECA_DIS_T_N  : SUR UNE MAILLE A 1 NOEUD
C    ON CALCULE LES OPTIONS FULL_MECA
C                           RAPH_MECA
C                           RIGI_MECA_TANG
C    POUR LES COMPORTEMENTS ELAS
C                           ASSE_CORN
C                           ARME
C                           DIS_CONTACT
C     ELEMENTS CONCERNES :  MECA_2D_DIS_TR_L : SUR UNE MAILLE A 2 NOEUDS
C                           MECA_2D_DIS_T_L  : SUR UNE MAILLE A 2 NOEUDS
C                           MECA_2D_DIS_TR_N : SUR UNE MAILLE A 1 NOEUD
C                           MECA_2D_DIS_T_N  : SUR UNE MAILLE A 1 NOEUD
C    ON CALCULE LES OPTIONS FULL_MECA
C                           RAPH_MECA
C                           RIGI_MECA_TANG
C     POUR LE COMPORTEMENT  ELAS
C ----------------------------------------------------------------------
C IN  : OPTIOZ : NOM DE L'OPTION A CALCULER
C       NOMTEZ : NOM DU TYPE_ELEMENT
C ----------------------------------------------------------------------

C **************** DEBUT COMMUNS NORMALISES JEVEUX *********************

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

C ***************** FIN COMMUNS NORMALISES JEVEUX **********************


C *************** DECLARATION DES VARIABLES LOCALES ********************

      REAL*8 ANG(3),PGL(3,3),KLV(78),KLV2(78),XD(3)
      REAL*8 UGM(12),UGP(12),DUG(12),COORG(12)
      REAL*8 ULM(12),ULP(12),DUL(12),KTY2,DVL(12),DPE(12),DVE(12)
      REAL*8 VARMO(7),VARPL(7),IRRAP
      INTEGER NBT,NNO,NC,NEQ,IGEOM,ICONTM,IDEPLM,IDEPLP,ICOMPO,LORIEN
      INTEGER I,JDC,IREP,IMAT,IFONO,ICONTP,ILOGIC,IITER,IMATE,IVARIM
      INTEGER IRMETG,ITERAT,IVARIP,JTP,IDECOL,NDIM,JCRET
      INTEGER IADZI, IAZK24
      CHARACTER*8 NOMAIL

C *********** FIN DES DECLARATIONS DES VARIABLES LOCALES ***************


C ********************* DEBUT DE LA SUBROUTINE *************************

C --- DEFINITIONS DE PARAMETRES : NBT = NOMBRE DE COEFFICIENTS DANS K
C                                 NEQ = NOMBRE DE DDL EN DEPLACEMENT

      OPTION = OPTIOZ
      NOMTE = NOMTEZ

      NDIM = 3
      IF (NOMTE.EQ.'MECA_DIS_TR_L') THEN
        NBT = 78
        NNO = 2
        NC = 6
        NEQ = 12
      ELSE IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
        NBT = 21
        NNO = 1
        NC = 6
        NEQ = 6
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_L') THEN
        NBT = 21
        NNO = 2
        NC = 3
        NEQ = 6
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_N') THEN
        NBT = 6
        NNO = 1
        NC = 3
        NEQ = 3
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_N') THEN
        NBT = 3
        NNO = 1
        NC  = 2
        NEQ = 2
        NDIM = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
        NBT = 10
        NNO = 2
        NC = 2
        NEQ = 4
        NDIM = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
        NBT = 6
        NNO = 1
        NC  = 3
        NEQ = 3
        NDIM = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
        NBT = 21
        NNO = 2
        NC  = 3
        NEQ = 6
        NDIM = 2
      ELSE
        CALL UTMESS('F','ELEMENTS DISCRET (TE0047)',
     &              '"'//NOMTE//'"    NOM D''ELEMENT INCONNU.')
      END IF

C --- RECUPERATION DES ADRESSES JEVEUX

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCONTMR','L',ICONTM)
      CALL JEVECH('PDEPLMR','L',IDEPLM)
      CALL JEVECH('PDEPLPR','L',IDEPLP)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      IF (ZK16(ICOMPO+3).EQ.'COMP_ELAS') THEN
        CALL UTMESS('F','TE0047','COMP_ELAS NON VALIDE')
      END IF


C ======================================================================
C   ORIENTATION DE L'ELEMENT ET DEPLACEMENTS DANS LES REPERES G ET L
C ======================================================================

C --- RECUPERATION DES ORIENTATIONS (ANGLES NAUTIQUES -> VECTEUR ANG)

      CALL TECACH ( 'ONN','PCAORIE', 1, LORIEN ,IRET)
      IF ( IRET .NE. 0 ) THEN
         CALL TECAEL ( IADZI, IAZK24 )
         NOMAIL = ZK24(IAZK24-1+3)(1:8)
         CALL UTDEBM ( 'F', 'TE0047', 'POUR LES ELEMENTS DISCRETS, '//
     +       'IL FAUT DEFINIR UN REPERE (COMMANDE AFFE_CARA_ELEM). '//
     +       'CETTE INFORMATION DOIT ETRE FOURNIE DERRIERE LE MOT '//
     +       'CLE "CARA_ELEM" DE LA COMMANDE EN COURS.')
         CALL UTIMPK ( 'L', '   MAILLE ', 1, NOMAIL )
         CALL UTFINM
      ENDIF
      CALL R8COPY ( 3, ZR(LORIEN), 1, ANG, 1 )

C --- DEPLACEMENTS DANS LE REPERE GLOBAL
C        UGM = DEPLACEMENT PRECEDENT
C        DUG = INCREMENT DE DEPLACEMENT
C        UGP = DEPLACEMENT COURANT
C        XD  = VECTEUR JOIGNANT LES DEUX NOEUDS AU REPOS (NORME XL0)

      DO 10 I = 1,NEQ
        UGM(I) = ZR(IDEPLM+I-1)
        DUG(I) = ZR(IDEPLP+I-1)
        UGP(I) = UGM(I) + DUG(I)
   10 CONTINUE

      IF (NNO.EQ.2)  THEN
        CALL VDIFF(NDIM,ZR(IGEOM+NDIM),ZR(IGEOM),XD)
        CALL PSCAL(NDIM,XD,XD,XL2)
        XL0 = SQRT(XL2)
      END IF

C --- CHANGEMENT DE REPERE POUR L'ELEMENT TRANSLATION/ROTATION LIAISON

      IF (ZK16(ICOMPO+2) (6:10).EQ.'_REAC') THEN

C* REACTUALISATION DES POSITIONS DES 2 NOEUDS ET CALCUL DE LEUR DISTANCE
        DO 20 I = 1,NDIM
          COORG(I)      = UGP(I)    + ZR(IGEOM+I-1)
          COORG(I+NDIM) = UGP(I+NC) + ZR(IGEOM+I+NDIM-1)
   20   CONTINUE
        CALL VDIFF(NDIM,COORG(NDIM+1),COORG(1),XD)
        CALL PSCAL(NDIM,XD,XD,XL2)
        XL = SQRT(XL2)

      ENDIF

      IF (NOMTE.EQ.'MECA_DIS_TR_L'.AND.
     +   ZK16(ICOMPO+2) (6:10).EQ.'_REAC') THEN

C* CALCUL DE L'ANGLE DE VRILLE MOYEN SI ELEMENT DE LONGUEUR NON NULLE
          CALL PSCAL(3,UGP(4),XD,TET1)
          CALL PSCAL(3,UGP(10),XD,TET2)
          IF (XL.NE.0.D0) THEN
            TET1 = TET1/XL
            TET2 = TET2/XL
          ELSE
            TET1 = 0.D0
            TET2 = 0.D0
          END IF
          IF (XL0.NE.0.D0) CALL ANGVX(XD,ANG(1),ANG(2))
          ANG(3) = ANG(3) + (TET1+TET2)/2.D0

      END IF

C --- MATRICE MGL DE PASSAGE REPERE GLOBAL -> REPERE LOCAL

      CALL MATROT(ANG,PGL)

C --- DEPLACEMENTS DANS LE REPERE LOCAL
C        ULM = DEPLACEMENT PRECEDENT    = PLG * UGM
C        DUL = INCREMENT DE DEPLACEMENT = PLG * DUG
C        ULP = DEPLACEMENT COURANT      = PLG * UGP

      IF (NDIM.EQ.3) THEN
        CALL UTPVGL(NNO,NC,PGL,UGM,ULM)
        CALL UTPVGL(NNO,NC,PGL,DUG,DUL)
        CALL UTPVGL(NNO,NC,PGL,UGP,ULP)
      ELSE IF (NDIM.EQ.2) THEN
        CALL UT2VGL(NNO,NC,PGL,UGM,ULM)
        CALL UT2VGL(NNO,NC,PGL,DUG,DUL)
        CALL UT2VGL(NNO,NC,PGL,UGP,ULP)
      END IF

C ======================================================================
C                      COMPORTEMENT ELASTIQUE
C ======================================================================

      IF ((ZK16(ICOMPO).NE.'ELAS').AND.(OPTION(11:14).EQ.'ELAS')) THEN
        CALL UTMESS('F','ELEMENTS DISCRET (TE0047)',
     &              '"'//NOMTE//'" MATRICE DE DECHARGE NON DEVELOPPEE')
      ENDIF

      IF (ZK16(ICOMPO).EQ.'ELAS') THEN

C --- PARAMETRES EN ENTREE

        CALL JEVECH('PCADISK','L',JDC)
        IREP = NINT(ZR(JDC+NBT))

C     --- ABSOLU VERS LOCAL ? ---
C     --- IREP = 1 = MATRICE EN REPERE GLOBAL ==> PASSER EN LOCAL ---

        IF (IREP.EQ.1) THEN
          IF (NDIM.EQ.3) THEN
            CALL UTPSGL(NNO,NC,PGL,ZR(JDC),KLV)
          ELSE IF (NDIM.EQ.2) THEN
            CALL UT2MGL(NNO,NC,PGL,ZR(JDC),KLV)
          END IF
        ELSE
          CALL R8COPY(NBT,ZR(JDC),1,KLV,1)
        END IF

C --- CALCUL DE LA MATRICE TANGENTE

        IF (OPTION(1:9).EQ.'FULL_MECA' .OR. 
     &         OPTION(1:10).EQ.'RIGI_MECA_') THEN

          CALL JEVECH('PMATUUR','E',IMAT)
          IF (NDIM.EQ.3) THEN
            CALL UTPSLG(NNO,NC,PGL,KLV,ZR(IMAT))
          ELSE IF (NDIM.EQ.2) THEN
            CALL UT2MLG(NNO,NC,PGL,KLV,ZR(IMAT))
          END IF
CC
        END IF

C --- CALCUL DES EFFORTS GENERALISES ET DES FORCES NODALES

        IF (OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN

          CALL JEVECH('PVECTUR','E',IFONO)
          CALL JEVECH('PCONTPR','E',ICONTP)

          ILOGIC = 0
          PLOUF = 0.D0
          CALL DISIEF(NBT,NEQ,NNO,NC,PGL,KLV,DUL,ZR(ICONTM),ILOGIC,
     &                PLOUF,PLOUF,ZR(ICONTP),ZR(IFONO),PLOUF,PLOUF)

        END IF

      END IF

C ======================================================================
C                  FIN DU COMPORTEMENT ELASTIQUE
C ======================================================================


C ======================================================================
C                      COMPORTEMENT CORNIERE
C ======================================================================

      IF (ZK16(ICOMPO).EQ.'ASSE_CORN') THEN

C --- PARAMETRES EN ENTREE

        CALL JEVECH('PITERAT','L',IITER)
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PVARIMR','L',IVARIM)

C --- RELATION DE COMPORTEMENT DE LA CORNIERE

        IRMETG = 0
        IF (OPTION.EQ.'RIGI_MECA_TANG') IRMETG = 1
        ITERAT = NINT(ZR(IITER))
        CALL DICORN(IRMETG,NBT,NEQ,ITERAT,ZI(IMATE),ULM,DUL,ULP,
     &              ZR(ICONTM),ZR(IVARIM),ZR(IVARIM+1),ZR(IVARIM+2),KLV,
     &              KLV2,VARIP1,VARIP2,VARIP3)

C --- ACTUALISATION DE LA MATRICE TANGENTE

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RIGI_MECA_TANG') THEN

          CALL JEVECH('PMATUUR','E',IMAT)
          CALL UTPSLG(NNO,NC,PGL,KLV2,ZR(IMAT))

        END IF

C --- CALCUL DES EFFORTS GENERALISES, DES FORCES NODALES
C     ET DES VARIABLES INTERNES

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN

          CALL JEVECH('PVECTUR','E',IFONO)
          CALL JEVECH('PCONTPR','E',ICONTP)
          CALL JEVECH('PVARIPR','E',IVARIP)

          ILOGIC = 0
          PLOUF = 0.D0
          CALL DISIEF(NBT,NEQ,NNO,NC,PGL,KLV,DUL,ZR(ICONTM),ILOGIC,
     &                PLOUF,PLOUF,ZR(ICONTP),ZR(IFONO),PLOUF,PLOUF)

          ZR(IVARIP) = VARIP1
          ZR(IVARIP+1) = VARIP2
          ZR(IVARIP+2) = VARIP3
          ZR(IVARIP+4) = VARIP1
          ZR(IVARIP+5) = VARIP2
          ZR(IVARIP+6) = VARIP3
C  NB : POUR IMPR_RESU : ZR(IVARIP+3) ET ZR(IVARIP+7) SONT VIDES !!!

        END IF

      END IF

C ======================================================================
C                 FIN DU COMPORTEMENT CORNIERE
C ======================================================================


C ======================================================================
C                    COMPORTEMENT ARMEMENT
C ======================================================================

      IF (ZK16(ICOMPO).EQ.'ARME') THEN

C --- PARAMETRES EN ENTREE

        CALL JEVECH('PCADISK','L',JDC)
        IREP = NINT(ZR(JDC+NBT))
        CALL R8COPY(NBT,ZR(JDC),1,KLV,1)
        IF (IREP.EQ.1) THEN
          CALL UTPSGL(NNO,NC,PGL,ZR(JDC),KLV)
        END IF
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PVARIMR','L',IVARIM)

C --- RELATION DE COMPORTEMENT DE L'ARMEMENT

        CALL DIARME(NBT,NEQ,ZI(IMATE),ULM,DUL,ULP,ZR(ICONTM),ZR(IVARIM),
     &              KLV,VARIP,KTY2,DULY)

C --- ACTUALISATION DE LA MATRICE TANGENTE

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RIGI_MECA_TANG') THEN

          CALL JEVECH('PMATUUR','E',IMAT)
          CALL UTPSLG(NNO,NC,PGL,KLV,ZR(IMAT))

        END IF

C --- CALCUL DES EFFORTS GENERALISES, DES FORCES NODALES
C     ET DES VARIABLES INTERNES

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN

          CALL JEVECH('PVECTUR','E',IFONO)
          CALL JEVECH('PCONTPR','E',ICONTP)
          CALL JEVECH('PVARIPR','E',IVARIP)

          ILOGIC = 1
          PLOUF = 0.D0
          CALL DISIEF(NBT,NEQ,NNO,NC,PGL,KLV,DUL,ZR(ICONTM),ILOGIC,KTY2,
     &                DULY,ZR(ICONTP),ZR(IFONO),PLOUF,PLOUF)

          ZR(IVARIP) = VARIP
          ZR(IVARIP+1) = VARIP

        END IF

      END IF

C ======================================================================
C                 FIN DU COMPORTEMENT ARMEMENT
C ======================================================================
C  COMPORTEMENT DIS_CONTACT : APPLICATION : LIAISON GRILLE-CRAYON COMBU
C ======================================================================

      IF (ZK16(ICOMPO).EQ.'DIS_CONTACT') THEN

        CALL JEVECH('PCADISK','L',JDC)

C        MATRICE DE RIGIDITE EN REPERE LOCAL

        IREP = NINT(ZR(JDC+NBT))
        IF (IREP.EQ.1) THEN
          CALL UTPSGL(NNO,NC,PGL,ZR(JDC),KLV)
        ELSE
          CALL R8COPY(NBT,ZR(JDC),1,KLV,1)
        END IF

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN

          CALL JEVECH('PMATERC','L',IMATE)
          CALL JEVECH('PVARIMR','L',IVARIM)
          CALL JEVECH('PINSTPR','L',JTP)
          CALL JEVECH('PVECTUR','E',IFONO)
          CALL JEVECH('PCONTPR','E',ICONTP)
          CALL JEVECH('PVARIPR','E',IVARIP)
C
          CALL TECACH('ONN','PTEMPMR',1,ITEMPM,IRETM)
          CALL TECACH('ONN','PTEMPPR',1,ITEMPP,IRETP)
          IF ((IRETM.EQ.0).AND.(IRETP.EQ.0)) THEN
             ITEMP = 1
             TEMPM = 0.5D0 * ( ZR(ITEMPM) + ZR(ITEMPM+1) )
             TEMPP = 0.5D0 * ( ZR(ITEMPP) + ZR(ITEMPP+1) )
          ELSE
             ITEMP = 0
             TEMPM = 0.D0
             TEMPP = 0.D0
          ENDIF
C
          CALL TECACH('ONN','PIRRAMR',1,IRRAMR,IRETM)
          CALL TECACH('ONN','PIRRAPR',1,IRRAPR,IRETP)
          IF ((IRETM.EQ.0).AND.(IRETP.EQ.0)) THEN
             IRRAP = 0.5D0 * ( ZR(IRRAPR) + ZR(IRRAPR+1) )
          ELSE
             IRRAP = 0.D0 
          ENDIF
C           RELATION DE COMPORTEMENT : ELASTIQUE PARTOUT
C           SAUF SUIVANT Y LOCAL : FROTTEMENT DE COULOMB

          CALL DICOUL(OPTION,NNO,NBT,NEQ,NC,ZI(IMATE),
     &                ULM,DUL,ZR(ICONTM),ZR(JTP),ZR(IVARIM),
     &                PGL,KLV,ZR(IVARIP),ZR(IFONO),ZR(ICONTP),
     &                ITEMP, TEMPM, TEMPP,IRRAP)

        END IF

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RIGI_MECA_TANG') THEN
          CALL JEVECH('PMATUUR','E',IMAT)
          CALL UTPSLG(NNO,NC,PGL,KLV,ZR(IMAT))
        END IF

      END IF

C ======================================================================
C                 FIN DU COMPORTEMENT DIS_CONTACT
C ======================================================================



C ======================================================================
C                    COMPORTEMENT CHOC
C ======================================================================

      IF (ZK16(ICOMPO).EQ.'DIS_CHOC') THEN

C --- PARAMETRES EN ENTREE

        CALL JEVECH('PCADISK','L',JDC)
        IREP = NINT(ZR(JDC+NBT))
        CALL R8COPY(NBT,ZR(JDC),1,KLV,1)
        IF (IREP.EQ.1) THEN
          CALL UTPSGL(NNO,NC,PGL,ZR(JDC),KLV)
        END IF
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PVARIMR','L',IVARIM)
        DO 30 I = 1,7
          VARMO(I) = ZR(IVARIM+I-1)
   30   CONTINUE
        CALL JEVECH('PINSTPR','L',JINST)
        CALL TECACH('ONN','PVITPLU',1,IVITP,IRET)
        IF (IRET.EQ.0) THEN
          CALL UTPVGL(NNO,NC,PGL,ZR(IVITP),DVL)
        ELSE
          DO 40 I = 1,12
            DVL(I) = 0.D0
   40     CONTINUE
        END IF
        CALL TECACH('ONN','PDEPENT',1,IDEPEN,IRET)
        IF (IRET.EQ.0) THEN
          CALL UTPVGL(NNO,NC,PGL,ZR(IDEPEN),DPE)
        ELSE
          DO 50 I = 1,12
            DPE(I) = 0.D0
   50     CONTINUE
        END IF
        CALL TECACH('ONN','PVITENT',1,IVITEN,IRET)
        IF (IRET.EQ.0) THEN
          CALL UTPVGL(NNO,NC,PGL,ZR(IVITEN),DVE)
        ELSE
          DO 60 I = 1,12
            DVE(I) = 0.D0
   60     CONTINUE
        END IF

C --- RELATION DE COMPORTEMENT DE CHOC

        CALL DICHOC(NBT,NEQ,NNO,NC,ZI(IMATE),DUL,ULP,ZR(IGEOM),PGL,
     &              KLV,KTY2,DULY,DVL,DPE,DVE,FOR2,
     &              FOR3,VARMO,VARPL)

C --- ACTUALISATION DE LA MATRICE TANGENTE

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RIGI_MECA_TANG') THEN

          CALL JEVECH('PMATUUR','E',IMAT)
          CALL UTPSLG(NNO,NC,PGL,KLV,ZR(IMAT))

        END IF

C --- CALCUL DES EFFORTS GENERALISES, DES FORCES NODALES
C     ET DES VARIABLES INTERNES

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN

          CALL JEVECH('PVECTUR','E',IFONO)
          CALL JEVECH('PCONTPR','E',ICONTP)
          CALL JEVECH('PVARIPR','E',IVARIP)

          ILOGIC = 2
          CALL DISIEF(NBT,NEQ,NNO,NC,PGL,KLV,DUL,ZR(ICONTM),ILOGIC,KTY2,
     &                DULY,ZR(ICONTP),ZR(IFONO),FOR2,FOR3)

          DO 70 I = 1,7
            ZR(IVARIP+I-1) = VARPL(I)
            IF (NNO.EQ.2) ZR(IVARIP+I+6) = VARPL(I)
   70     CONTINUE

        END IF

      END IF

C ======================================================================
C                 FIN DU COMPORTEMENT CHOC
C ======================================================================



C  COMPORTEMENT DIS_GOUJON : APPLICATION : GOUJ2ECH
C ======================================================================

      IF (ZK16(ICOMPO) (1:10).EQ.'DIS_GOUJ2E') THEN

        CALL JEVECH('PCADISK','L',JDC)

C        MATRICE DE RIGIDITE EN REPERE LOCAL

        IREP = NINT(ZR(JDC+NBT))
        IF (IREP.EQ.1) THEN
          IF (NDIM.EQ.3) THEN
            CALL UTPSGL(NNO,NC,PGL,ZR(JDC),KLV)
          ELSE IF (NDIM.EQ.2) THEN
            CALL UT2MGL(NNO,NC,PGL,ZR(JDC),KLV)
          END IF
        ELSE
          CALL R8COPY(NBT,ZR(JDC),1,KLV,1)
        END IF

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN

          CALL JEVECH('PMATERC','L',IMATE)
          CALL JEVECH('PVARIMR','L',IVARIM)
          CALL JEVECH('PVECTUR','E',IFONO)
          CALL JEVECH('PCONTPR','E',ICONTP)
          CALL JEVECH('PVARIPR','E',IVARIP)
        ELSE IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
          CALL JEVECH('PMATERC','L',IMATE)
          CALL JEVECH('PVARIMR','L',IVARIM)
        END IF

C           RELATION DE COMPORTEMENT : ELASTIQUE PARTOUT
C           SAUF SUIVANT Y LOCAL : ELASTOPLASTIQUE VMIS_ISOT_TRAC

        CALL DIGOUJ(OPTION,ZK16(ICOMPO),NNO,NBT,NEQ,NC,ZI(IMATE),
     &              DUL,ZR(ICONTM),ZR(IVARIM),PGL,KLV,KLV2,ZR(IVARIP),
     &              ZR(IFONO),ZR(ICONTP),NOMTE)

        IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RIGI_MECA_TANG') THEN
          CALL JEVECH('PMATUUR','E',IMAT)
          IF (NDIM.EQ.3) THEN
            CALL UTPSLG(NNO,NC,PGL,KLV,ZR(IMAT))
          ELSE IF (NDIM.EQ.2) THEN
            CALL UT2MLG(NNO,NC,PGL,KLV,ZR(IMAT))
          END IF
        END IF

      END IF

C ======================================================================
C                 FIN DU COMPORTEMENT DIS_GOUJON
C ======================================================================

   80 CONTINUE
C
      IF ( OPTION(1:9).EQ.'FULL_MECA'  .OR.  
     +     OPTION(1:9).EQ.'RAPH_MECA'  ) THEN
         CALL JEVECH ( 'PCODRET', 'E', JCRET )
         ZI(JCRET) = 0
      ENDIF
C
      END
