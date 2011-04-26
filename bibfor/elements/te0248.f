      SUBROUTINE TE0248(OPTIOZ,NOMTEZ)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
      CHARACTER*(*) OPTIOZ,NOMTEZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================

C     CALCUL DES OPTIONS FULL_MECA, RAPH_MECA, RIGI_MECA_TANG
C     ET RIGI_MECA_IMPLEX POUR COMPORTEMENTS LINEAIRES ET NON LINEAIRES
C     DES ELEMENTS DE BARRE 'MECA_BARRE'

C ----------------------------------------------------------------------
C IN  : OPTION : NOM DE L'OPTION A CALCULER (K16)
C IN  : NOMTE  : NOM DU TYPE_ELEMENT (K16)
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

      INTEGER NEQ,NBT,NVAMAX,IMATE,IGEOM,IORIE,ISECT,IINSTM,IVARMP
      INTEGER IINSTP,IDEPLM,IDEPLP,ICONTM,IVARIM,ICOMPO
      INTEGER ICARCR,IMATUU,IVECTU,ICONTP,NNO,NC,IVARIP,JCRET,NBVARI
      INTEGER JTAB(7),IRET
      PARAMETER (NEQ=6,NBT=21,NVAMAX=8)
      CHARACTER*4  FAMI
      CHARACTER*16 VALKM(2)

C   CONSTANTES POUR INTO MENEGOTTO

      INTEGER NCSTPM,CODRET
      PARAMETER (NCSTPM=13)
      REAL*8 CSTPM(NCSTPM)

      REAL*8 E,EPSM,DDOT
      REAL*8 A,XLONG0,XLONGM,SIGY,DSDE
      REAL*8 PGL(3,3)
      REAL*8 DUL(NEQ),UML(NEQ),DLONG
      REAL*8 KLV(NBT),VIP(NVAMAX),VIM(NVAMAX)
      REAL*8 EFFNOM,EFFNOP,FONO(NEQ)
      REAL*8 W(6),ANG1(3),XD(3),MATUU(21),VECTU(6)
      REAL*8 DEPLM(6),DEPLP(6),SIGX,EPSX,DEPX,SIGXP
      REAL*8 ETAN
      REAL*8 ANGMAS(3),R8NNEM
      INTEGER I



      LOGICAL VECTEU

C *********** FIN DES DECLARATIONS DES VARIABLES LOCALES ***************


      OPTION = OPTIOZ
      NOMTE = NOMTEZ
      CODRET=0
      FAMI = 'RIGI'

C --- PARAMETRES EN ENTREE

      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCAORIE','L',IORIE)
      CALL JEVECH('PCAGNBA','L',ISECT)
      CALL JEVECH('PINSTMR','L',IINSTM)
      CALL JEVECH('PINSTPR','L',IINSTP)


C ---- LA PRESENCE DU CHAMP DE DEPLACEMENT A L INSTANT T+
C ---- DEVRAIT ETRE CONDITIONNE  PAR L OPTION (AVEC RIGI_MECA_TANG
C ---- CA N A PAS DE SENS).
C ---- CEPENDANT CE CHAMP EST INITIALISE A 0 PAR LA ROUTINE NMMATR.
      CALL JEVECH('PDEPLMR','L',IDEPLM)
      CALL JEVECH('PDEPLPR','L',IDEPLP)
      CALL JEVECH('PCONTMR','L',ICONTM)
      CALL JEVECH('PVARIMR','L',IVARIM)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      CALL JEVECH('PCARCRI','L',ICARCR)

C --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C --- INITIALISE A R8NNEM (ON NE S'EN SERT PAS)
      CALL R8INIR(3,  R8NNEM(), ANGMAS ,1)

C --- PARAMETRES EN SORTIE


      IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN
        CALL JEVECH('PMATUUR','E',IMATUU)
        IVARIP = IVARIM
        ICONTP = ICONTM
      ELSE IF (OPTION(1:9).EQ.'FULL_MECA') THEN
        CALL JEVECH('PMATUUR','E',IMATUU)
        CALL JEVECH('PVECTUR','E',IVECTU)
        CALL JEVECH('PCONTPR','E',ICONTP)
        CALL JEVECH('PVARIPR','E',IVARIP)
      ELSE IF (OPTION.EQ.'RAPH_MECA') THEN
        CALL JEVECH('PVECTUR','E',IVECTU)
        CALL JEVECH('PCONTPR','E',ICONTP)
        CALL JEVECH('PVARIPR','E',IVARIP)
      END IF

      IF (OPTION(1:16).EQ.'RIGI_MECA_IMPLEX') THEN
        CALL JEVECH('PCONTXR','E',ICONTP)
      END IF

      IF (ZK16(ICOMPO+3).EQ.'COMP_ELAS') THEN
        CALL U2MESS('F','ELEMENTS2_90')
      END IF

C --- RECUPERATION DE LA SECTION DE LA BARRE

      A = ZR(ISECT)
      NNO = 2
      NC = 3

C --- RECUPERATION DES ORIENTATIONS BETA,GAMMA
C --- ET CALCUL DES MATRICES DE CHANGEMENT DE REPERE

      IF (ZK16(ICOMPO+2) (6:10).EQ.'_REAC') THEN
        IF (NOMTE.EQ.'MECA_BARRE') THEN
          DO 10 I = 1,3
            W(I) = ZR(IGEOM-1+I) + ZR(IDEPLM-1+I) + ZR(IDEPLP-1+I)
            W(I+3) = ZR(IGEOM+2+I) + ZR(IDEPLM+2+I) + ZR(IDEPLP+2+I)
            XD(I) = W(I+3) - W(I)
   10     CONTINUE
        ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          W(1) = ZR(IGEOM-1+1) + ZR(IDEPLM-1+1) + ZR(IDEPLP-1+1)
          W(2) = ZR(IGEOM-1+2) + ZR(IDEPLM-1+2) + ZR(IDEPLP-1+2)
          W(3) = 0.D0
          W(4) = ZR(IGEOM-1+3) + ZR(IDEPLM-1+3) + ZR(IDEPLP-1+3)
          W(5) = ZR(IGEOM-1+4) + ZR(IDEPLM-1+4) + ZR(IDEPLP-1+4)
          W(6) = 0.D0
          XD(1) = W(4) - W(1)
          XD(2) = W(5) - W(2)
          XD(3) = 0.D0
        END IF
        CALL ANGVX(XD,ANG1(1),ANG1(2))
        ANG1(3) = ZR(IORIE+2)
        CALL MATROT(ANG1,PGL)
      ELSE
        IF (NOMTE.EQ.'MECA_BARRE') THEN
          DO 20 I = 1,3
            W(I) = ZR(IGEOM-1+I)
            W(I+3) = ZR(IGEOM+2+I)
            XD(I) = W(I+3) - W(I)
   20     CONTINUE
        ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
          W(1) = ZR(IGEOM-1+1)
          W(2) = ZR(IGEOM-1+2)
          W(3) = 0.D0
          W(4) = ZR(IGEOM-1+3)
          W(5) = ZR(IGEOM-1+4)
          W(6) = 0.D0
          XD(1) = W(4) - W(1)
          XD(2) = W(5) - W(2)
          XD(3) = 0.D0
        END IF
        CALL MATROT(ZR(IORIE),PGL)
      END IF

      XLONG0=DDOT(3,XD,1,XD,1)
      XLONG0 = SQRT(XLONG0)

      IF (XLONG0.EQ.0.D0) THEN
        CALL U2MESS('F','ELEMENTS3_62')
      END IF


C --- INCREMENT DE DEPLACEMENT EN REPERE LOCAL
C CORRECTION CHAVANT : DUL = INCREMENT ENTRE INSTANT
C PLUS ET INSTANT MOINS
C ---    DUL  ENTRE LE REPOS ET LE DERNIER ETAT CONVERGE
C --- INCREMENT D'ALLONGEMENT DLONG

      IF (NOMTE.EQ.'MECA_BARRE') THEN
        DO 30 I = 1,6
          DEPLM(I) = ZR(IDEPLM+I-1)
          DEPLP(I) = ZR(IDEPLP+I-1)
   30   CONTINUE
      ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
        DEPLM(1) = ZR(IDEPLM)
        DEPLM(2) = ZR(IDEPLM+1)
        DEPLM(3) = 0.D0
        DEPLM(4) = ZR(IDEPLM+2)
        DEPLM(5) = ZR(IDEPLM+3)
        DEPLM(6) = 0.D0

        DEPLP(1) = ZR(IDEPLP)
        DEPLP(2) = ZR(IDEPLP+1)
        DEPLP(3) = 0.D0
        DEPLP(4) = ZR(IDEPLP+2)
        DEPLP(5) = ZR(IDEPLP+3)
        DEPLP(6) = 0.D0

      END IF

      CALL UTPVGL(NNO,NC,PGL,DEPLM,UML)
      CALL UTPVGL(NNO,NC,PGL,DEPLP,DUL)

      DLONG = DUL(4) - DUL(1)

      XLONGM = XLONG0 + UML(4) - UML(1)



C --- RECUPERATION
C ---     DE L'EFFORT NORMAL PRECEDENT MOYEN EFFNOM POUR L'ELEMENT
      EFFNOM = ZR(ICONTM)


C --- RELATION DE COMPORTEMENT

C     ---------------------------------------------------
      IF (ZK16(ICOMPO).EQ.'ELAS' .OR.
     &    ZK16(ICOMPO).EQ.'VMIS_ISOT_LINE' .OR.
     &    ZK16(ICOMPO).EQ.'VMIS_ISOT_TRAC' .OR.
     &    ZK16(ICOMPO).EQ.'CORR_ACIER' .OR.
     &    ZK16(ICOMPO).EQ.'VMIS_CINE_LINE') THEN
C     ---------------------------------------------------

C --- RECUPERATION DES CARACTERISTIQUES DU MATERIAU

        EPSM = (UML(4)-UML(1))/XLONG0
        CALL NMICLB(FAMI,1,1,OPTION,ZK16(ICOMPO),ZI(IMATE),XLONG0,A,
     &              ZR(IINSTM),ZR(IINSTP),
     &              DLONG,EFFNOM,ZR(IVARIM),EFFNOP,
     &              ZR(IVARIP),KLV,FONO,EPSM,ZR(ICARCR),CODRET)

        IF (OPTION(1:16).EQ.'RIGI_MECA_IMPLEX') THEN
          ZR(ICONTP) = EFFNOP
        END IF

        IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN
          CALL UTPSLG(NNO,NC,PGL,KLV,MATUU)
        ELSE
          ZR(ICONTP) = EFFNOP
          IF (OPTION(1:9).EQ.'FULL_MECA') THEN
            CALL UTPSLG(NNO,NC,PGL,KLV,MATUU)
          END IF
          CALL UTPVLG(NNO,NC,PGL,FONO,VECTU)
        END IF

C     ---------------------------------------------------
      ELSE IF (ZK16(ICOMPO).EQ.'VMIS_ASYM_LINE') THEN
C     ---------------------------------------------------

C        RECUPERATION DES CARACTERISTIQUES DU MATERIAU

        CALL NMMABA(ZI(IMATE),ZK16(ICOMPO),E,DSDE,SIGY,NCSTPM,
     &              CSTPM)

        CALL NMASYM(FAMI,1,1,ZI(IMATE),OPTION,XLONG0,A,
     &              ZR(IINSTM),ZR(IINSTP),
     &              DLONG,EFFNOM,ZR(IVARIM),ZR(ICONTP),
     &              ZR(IVARIP),KLV,FONO)

        IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN
          CALL UTPSLG(NNO,NC,PGL,KLV,MATUU)
        ELSE
          IF (OPTION(1:9).EQ.'FULL_MECA') THEN
            CALL UTPSLG(NNO,NC,PGL,KLV,MATUU)
          END IF
          CALL UTPVLG(NNO,NC,PGL,FONO,VECTU)
        END IF

C     ---------------------------------------------------
      ELSE IF (ZK16(ICOMPO).EQ.'PINTO_MENEGOTTO') THEN
C     ---------------------------------------------------

C        RECUPERATION DES CARACTERISTIQUES DU MATERIAU

        CALL NMMABA(ZI(IMATE),ZK16(ICOMPO),E,DSDE,SIGY,NCSTPM,
     &              CSTPM)

        VIM(1) = ZR(IVARIM)
        VIM(2) = ZR(IVARIM+1)
        VIM(3) = ZR(IVARIM+2)
        VIM(4) = ZR(IVARIM+3)
        VIM(5) = ZR(IVARIM+4)
        VIM(6) = ZR(IVARIM+5)
        VIM(7) = ZR(IVARIM+6)
        VIM(8) = ZR(IVARIM+7)
        CALL NMPIME(FAMI,1,1,ZI(IMATE),OPTION,XLONG0,A,XLONGM,
     &              DLONG,NCSTPM,CSTPM,VIM,EFFNOM,VIP,
     &              EFFNOP,KLV,FONO)

        IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN
          CALL UTPSLG(NNO,NC,PGL,KLV,MATUU)
        ELSE
          ZR(ICONTP) = EFFNOP
          IF (OPTION(1:9).EQ.'FULL_MECA') THEN
            CALL UTPSLG(NNO,NC,PGL,KLV,MATUU)
          END IF
          ZR(IVARIP) = VIP(1)
          ZR(IVARIP+1) = VIP(2)
          ZR(IVARIP+2) = VIP(3)
          ZR(IVARIP+3) = VIP(4)
          ZR(IVARIP+4) = VIP(5)
          ZR(IVARIP+5) = VIP(6)
          ZR(IVARIP+6) = VIP(7)
          ZR(IVARIP+7) = VIP(8)
          CALL UTPVLG(NNO,NC,PGL,FONO,VECTU)
        END IF

C     ------------
      ELSE
C     ------------

        CALL R8INIR(NEQ,0.D0,FONO,1)
        CALL R8INIR(NBT,0.D0,KLV,1)
        VECTEU = ((OPTION(1:9).EQ.'FULL_MECA') .OR.
     &              (OPTION(1:9).EQ.'RAPH_MECA'))


        CALL JEVECH ('PCOMPOR','L',ICOMPO)
        IF ((ZK16(ICOMPO-1+5)(1:7).NE.'DEBORST').AND.
     &      (ZK16(ICOMPO)(1:4).NE.'SANS')) THEN
          VALKM(1) = ZK16(ICOMPO)
          VALKM(2) = 'COMP_INCR'
          CALL U2MESK('F','ALGORITH6_81',2,VALKM)
        ELSE

            SIGX=EFFNOM/A
            EPSX=(UML(4)-UML(1))/XLONG0
            DEPX=DLONG/XLONG0

            IF (VECTEU) THEN
               CALL TECACH('OON','PVARIMP',7,JTAB,IRET)
               NBVARI = MAX(JTAB(6),1)*JTAB(7)
               IVARMP=JTAB(1)
               CALL DCOPY(NBVARI,ZR(IVARMP),1,ZR(IVARIP),1)
            ENDIF

          CALL COMP1D(FAMI,1,1,OPTION,
     &                SIGX,EPSX,DEPX,
     &                ANGMAS,
     &                ZR(IVARIM),ZR(IVARIP),SIGXP,ETAN,CODRET)

            IF (VECTEU) THEN

C ---          STOCKAGE DE L'EFFORT NORMAL
               ZR(ICONTP) = SIGXP*A


C ---          CALCUL DES FORCES NODALES

               FONO(1) = -SIGXP*A
               FONO(4) =  SIGXP*A
C
            END IF

C ---       CALCUL DE LA MATRICE TANGENTE

            KLV(1) = ETAN
            KLV(7) = -ETAN
            KLV(10) = ETAN

        ENDIF

C ---  PASSAGE DE KLV ET FONO DU REPERE LOCAL AU REPERE GLOBAL

        IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN
          CALL UTPSLG(NNO,NC,PGL,KLV,MATUU)
        ELSE
          IF (OPTION(1:9).EQ.'FULL_MECA') THEN
            CALL UTPSLG(NNO,NC,PGL,KLV,MATUU)
          END IF
          CALL UTPVLG(NNO,NC,PGL,FONO,VECTU)
        END IF

C     ----------
      END IF
C     ----------

      IF (NOMTE.EQ.'MECA_BARRE') THEN
        IF ((OPTION(1:10).EQ.'RIGI_MECA_') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DO 70 I = 1,21
            ZR(IMATUU+I-1) = MATUU(I)
   70     CONTINUE
        END IF
        IF (OPTION(1:10).NE.'RIGI_MECA_') THEN
          DO 80 I = 1,6
            ZR(IVECTU+I-1) = VECTU(I)
   80     CONTINUE
        END IF

      ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
        IF ((OPTION(1:10).EQ.'RIGI_MECA_') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
          ZR(IMATUU) = MATUU(1)
          ZR(IMATUU+1) = MATUU(2)
          ZR(IMATUU+2) = MATUU(3)
          ZR(IMATUU+3) = MATUU(7)
          ZR(IMATUU+4) = MATUU(8)
          ZR(IMATUU+5) = MATUU(10)
          ZR(IMATUU+6) = MATUU(11)
          ZR(IMATUU+7) = MATUU(12)
          ZR(IMATUU+8) = MATUU(14)
          ZR(IMATUU+9) = MATUU(15)
        END IF
        IF (OPTION(1:10).NE.'RIGI_MECA_') THEN

          ZR(IVECTU) = VECTU(1)
          ZR(IVECTU+1) = VECTU(2)
          ZR(IVECTU+2) = VECTU(4)
          ZR(IVECTU+3) = VECTU(5)
        END IF

      END IF

      IF (OPTION(1:9).EQ.'FULL_MECA' .OR.
     &    OPTION(1:9).EQ.'RAPH_MECA') THEN
        CALL JEVECH('PCODRET','E',JCRET)
        ZI(JCRET) = CODRET
      END IF

      END
