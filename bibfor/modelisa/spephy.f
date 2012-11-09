      SUBROUTINE SPEPHY(IOPTCH,INTPHY,INTMOD,NOMU,TABLE,FREQ,CHAM,
     &                  SPECMR,SPECMI,DISC,NNOE,NOMCMP,NUOR,NBMR,
     &                  NBN,IMOD1,NBPF,NBM,IVITEF)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C  RESTITUTION SUR BASE PHYSIQUE D'UNE TABL_INTSP DE REPONSE MODALE
C  LA BASE MODALE EST DEFINIE PAR UN CONCEPT MELASFLU
C  LE CONCEPT PRODUIT EST UNE TABL_INTSP
C  LE CONCEPT TABL_INTSP SE COMPOSE :
C        D'UNE STRUCTURE TABLE QUI POINTE SUR UNE TABLE DE FONCTIONS
C        COMPLEXES
C-----------------------------------------------------------------------
C IN  : IOPTCH : INDICE DONNANT LA NATURE DES INTERSPECTRES A CALCULER
C       IOPTCH = 1 : INTERSPECTRES DE DEPLACEMENTS
C       IOPTCH = 2 : INTERSPECTRES DE VITESSES
C       IOPTCH = 3 : INTERSPECTRES D' ACCELERATIONS
C       IOPTCH = 4 : INTERSPECTRES DE CONTRAINTES
C IN  : INTPHY : BOOLEEN
C                CARACTERISE LE CONTENU DE LA TABLE D'INTERSPECTRES DE
C                REPONSE PHYSIQUE A CALCULER
C       INTPHY = .TRUE.  TOUS LES INTERSPECTRES SERONT CALCULES
C       INTPHY = .FALSE. SEULS LES AUTOSPECTRES SERONT CALCULES
C IN  : INTMOD : BOOLEEN
C                CARACTERISE LE CONTENU DE LA TABLE D'INTERSPECTRES DE
C                REPONSE MODALE (DONNEE DU CALCUL)
C       INTMOD = .TRUE.  TOUS LES INTERSPECTRES ONT ETE CALCULES
C       INTMOD = .FALSE. SEULS LES AUTOSPECTRES ONT ETE CALCULES
C IN  : NOMU   : NOM UTILISATEUR DU CONCEPT TABL_INTSP DE REPONSE
C                PHYSIQUE : A PRODUIRE
C IN  : TABLE  : NOM UTILISATEUR DU CONCEPT TABL_INTSP DE REPONSE
C                MODALE : DONNEE DU CALCUL
C IN  : FREQ   : CARACT. MODALES DE LA BASE DE CONCEPT MELASFLU
C IN  : CHAM   : CHAMP DE GRANDEURS MODALES AUX NOEUDS DE REPONSE
C IN  : SPECMR : VECTEUR DE TRAVAIL
C IN  : SPECMI : VECTEUR DE TRAVAIL
C I/O : DISC   : DISCRETISATION FREQUENTIELLE POUR CHAQUE VITESSE
C IN  : NNOE   : LISTE DES NOEUDS OU LA REPONSE EST CALCULEE
C IN  : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES PRIS EN COMPTE
C IN  : NBMR   : NBR. DE MODES PRIS EN COMPTE
C IN  : NBN    : NBR. DE NOEUDS DE REPONSE
C IN  : NBFO1  : NBR. DE SPECTRES D EXCITATION DEFINIS
C IN  : IMOD1  : INDICE DU PREMIER MODE PRIS EN COMPTE DANS LA BASE DE
C                CONCEPT MELASFLU
C IN  : NBPF   : NBR. DE POINTS DE LA DISCRETISATION FREQUENTIELLE
C IN  : NBM    : NBR. DE MODES DE LA BASE DE CONCEPT MELASFLU
C IN  : IVITEF : INDICE VITESSE DE FLUIDE
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      LOGICAL      INTPHY,INTMOD,EXIIND
      INTEGER      IOPTCH,NBMR,NBN,IMOD1,NBPF,NBM,IVITEF
      INTEGER      NUOR(NBMR),LNUMI,LNUMJ,IJ
      REAL*8       CHAM(NBN,NBMR),SPECMR(NBPF,*),SPECMI(NBPF,*)
      REAL*8       DISC(*), FREQ(2,NBM,*)
      CHARACTER*8  NOMU, TABLE, NNOE(NBN), NOMCMP
C
      INTEGER       IVAL(3)
      INTEGER       NBABS,ISPEC,MXVAL,MXVALS,LNOEI,LNOEJ,LCMPI,LCMPJ
      REAL*8        PI, R8PI
C-----------------------------------------------------------------------
      INTEGER I1 ,I2 ,IDEB ,IDEBM ,IDEBN ,IF1 ,IFON
      INTEGER IL ,IM1 ,IM2 ,IMI ,IMJ ,INI ,INJ ,LFREQ,LFREQS
      INTEGER ISJ ,ISM ,IV

      REAL*8 SPECI ,SPECR
C
      CHARACTER*8   K8B
      CHARACTER*24  KVAL(5), VALK(2),CHVALS,CHFREQ
      CHARACTER*24  CHNUMI,CHNUMJ,CHVALE,CHNOEI,CHNOEJ,CHCMPI,CHCMPJ
C
C-----------------------------------------------------------------------
      CALL JEMARQ()
      PI = R8PI()

      CHNUMI = TABLE//'.NUMI'
      CHNUMJ = TABLE//'.NUMJ'
      CHFREQ = TABLE//'.FREQ'
      CHVALE = TABLE//'.VALE'
      CALL JEVEUO(CHNUMI,'L',LNUMI)
      CALL JEVEUO(CHNUMJ,'L',LNUMJ)
      CALL JEVEUO(CHFREQ,'L',LFREQ)
      CALL JELIRA(CHNUMI,'LONMAX',MXVAL,K8B)

C
C --- POUR CHAQUE PAS EN VITESSE ON CALCULE L INTERSPECTRE DE REPONSE
C
      IV = IVITEF

        IVAL(1) = IV
C
C ---   TEST POUR DETECTER UN EVENTUEL PROBLEME DE CONVERGENCE EN AMONT
C ---   DANS L'OPERATEUR CALC_FLUI_STRU POUR CALCULER LES PARAMETRES
C ---   MODAUX A LA VITESSE D'ECOULEMENT CONSIDEREE
C ---   DANS CE CAS LES INTERSPECTRES DE REPONSE MODALE N'ONT PAS ETE
C ---   CALCULES PAR L'OPERATEUR DYNA_SPEC_MODAL
C ---   => ON PASSE A LA VITESSE SUIVANTE
C
        IVAL(2) = NUOR(1)
        IVAL(3) = NUOR(1)
        EXIIND = .FALSE.
        DO 200 I1 = 1,MXVAL
          IF ((ZI(LNUMI-1+I1) .EQ. IVAL(2)) .AND.
     &        (ZI(LNUMJ-1+I1) .EQ. IVAL(3))) EXIIND = .TRUE.
200     CONTINUE

        IF ( .NOT. EXIIND ) GO TO 20
C
C     --- RECUPERATION DES FONCTIONS (SPECTRES) ET STOCKAGE DANS
C     ---          SPECMR,SPECMI
C
        DO 30 IMJ = 1,NBMR
C
          IVAL(3) = NUOR(IMJ)
C
          IDEB = IMJ
          IF ( INTMOD ) IDEB = 1
C
          DO 40 IMI = IDEB,IMJ
C
            IVAL(2) = NUOR(IMI)
C
            EXIIND = .FALSE.
            DO 210 I1 = 1,MXVAL
              IF ((ZI(LNUMI-1+I1) .EQ. IVAL(3)) .AND.
     &            (ZI(LNUMJ-1+I1) .EQ. IVAL(2))) THEN
                EXIIND = .TRUE.
                CALL JEVEUO(JEXNUM(CHVALE,I1),'L',IFON)
              ENDIF
210         CONTINUE

            IF (.NOT.EXIIND) THEN
               VALK(1)(1:10) = 'INTE_SPEC'
               VALK(2)(1:8) = TABLE
               CALL U2MESK('F','MODELISA2_91', 2, VALK)
            ENDIF
C
            ISJ = (IMJ* (IMJ-1))/2 + IMI
            IF ( ISJ .EQ. 1 ) THEN
               DO 51 IF1 = 1,NBPF
                  DISC(IF1) = ZR(LFREQ+ (IF1-1))
   51          CONTINUE
            ENDIF
C
            DO 50 IF1 = 1,NBPF
              IF (IVAL(2) .EQ. IVAL(3)) THEN
               SPECMR(IF1,ISJ) = ZR(IFON+(IF1-1))
               SPECMI(IF1,ISJ) = 0.D0
              ELSE
               SPECMR(IF1,ISJ) = ZR(IFON+ (IF1-1)*2)
               SPECMI(IF1,ISJ) = ZR(IFON+ (IF1-1)*2+1)
              ENDIF
   50       CONTINUE
   40     CONTINUE
C
   30   CONTINUE
C
C    --- CREATION ET REMPLISSAGE DES FONCTIONS - SPECTRES REPONSES
C
        CHNOEI = NOMU//'.NOEI'
        CHNOEJ = NOMU//'.NOEJ'
        CHCMPI = NOMU//'.CMPI'
        CHCMPJ = NOMU//'.CMPJ'
        CHVALS = NOMU//'.VALE'
        CALL WKVECT(NOMU//'.FREQ','G V R',NBPF,LFREQS)
        DO 80 IL = 1,NBPF
          ZR(LFREQS+IL-1) = DISC(IL)
80      CONTINUE

        IF ( INTPHY ) THEN
          MXVALS = NBN*(NBN+1)/2
        ELSE
          MXVALS = NBN
        ENDIF

        CALL WKVECT(CHNOEI,'G V K8',MXVALS,LNOEI)
        CALL WKVECT(CHNOEJ,'G V K8',MXVALS,LNOEJ)
        CALL WKVECT(CHCMPI,'G V K8',MXVALS,LCMPI)
        CALL WKVECT(CHCMPJ,'G V K8',MXVALS,LCMPJ)
        CALL JECREC(CHVALS,'G V R','NU','DISPERSE','VARIABLE',MXVALS)

        IJ = 0
        DO 60 INJ = 1,NBN
C
          KVAL(3) = NNOE(INJ)
          KVAL(4) = NOMCMP
C
          IDEBN = INJ
          IF ( INTPHY ) IDEBN = 1
C
          DO 70 INI = IDEBN,INJ
C
            IJ = IJ+1
            KVAL(1) = NNOE(INI)
            KVAL(2) = NOMCMP
C
            ZK8(LNOEI-1+IJ) = KVAL(1)
            ZK8(LNOEJ-1+IJ) = KVAL(3)
            ZK8(LCMPI-1+IJ) = KVAL(2)
            ZK8(LCMPJ-1+IJ) = KVAL(4)

            IF ((KVAL(1) .EQ. KVAL(3)) .AND.
     &           (KVAL(2) .EQ. KVAL(4))) THEN
              NBABS = NBPF
            ELSE
              NBABS = 2*NBPF
            ENDIF

            CALL JECROC(JEXNUM(CHVALS,IJ))
            CALL JEECRA(JEXNUM(CHVALS,IJ),'LONMAX',NBABS,' ')
            CALL JEECRA(JEXNUM(CHVALS,IJ),'LONUTI',NBABS,' ')
            CALL JEVEUO(JEXNUM(CHVALS,IJ),'E',ISPEC)
C
            DO 90 IL = 1,NBPF
C
              SPECR = 0.D0
              SPECI = 0.D0
C
              DO 100 IM2 = 1 , NBMR
C
                IDEBM = IM2
                IF ( INTMOD ) IDEBM = 1
C
                DO 110 IM1 = IDEBM,IM2
                  I1 = IMOD1 + IM1 - 1
                  I2 = IMOD1 + IM2 - 1
                  ISM = (IM2* (IM2-1))/2 + IM1
C
                  IF (IM1.EQ.IM2) THEN
C                 --------------------
C
                    IF (IOPTCH.EQ.1 .OR. IOPTCH.EQ.4) THEN
                      SPECR = SPECR + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                        SPECMR(IL,ISM)
C
                    ELSE IF (IOPTCH.EQ.2) THEN
                      SPECR = SPECR + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                    FREQ(1,I1,IV)*FREQ(1,I2,IV)*SPECMR(IL,ISM)
C
                    ELSE IF (IOPTCH.EQ.3) THEN
                      SPECR = SPECR + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                    FREQ(1,I1,IV)*FREQ(1,I2,IV)*FREQ(1,I1,IV)*
     &                    FREQ(1,I2,IV)*SPECMR(IL,ISM)
C
                    END IF
C
                  ELSE
C                 ----
C
                    IF (IOPTCH.EQ.1 .OR. IOPTCH.EQ.4) THEN
                      SPECR = SPECR + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                      SPECMR(IL,ISM) + CHAM(INI,IM2)*
     &                      CHAM(INJ,IM1)*SPECMR(IL,ISM)
                      SPECI = SPECI + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                      SPECMI(IL,ISM) - CHAM(INI,IM2)*
     &                      CHAM(INJ,IM1)*SPECMI(IL,ISM)
C
                    ELSE IF (IOPTCH.EQ.2) THEN
                      SPECR = SPECR + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                  SPECMR(IL,ISM)*FREQ(1,I1,IV)*FREQ(1,I2,IV) +
     &                  CHAM(INI,IM2)*CHAM(INJ,IM1)*FREQ(1,I1,IV)*
     &                  FREQ(1,I2,IV)*SPECMR(IL,ISM)
                      SPECI = SPECI + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                  SPECMI(IL,ISM)*FREQ(1,I1,IV)*FREQ(1,I2,IV) -
     &                  CHAM(INI,IM2)*CHAM(INJ,IM1)*FREQ(1,I1,IV)*
     &                  FREQ(1,I2,IV)*SPECMI(IL,ISM)
C
                    ELSE IF (IOPTCH.EQ.3) THEN
                      SPECR = SPECR + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                  SPECMR(IL,ISM)*FREQ(1,I1,IV)*FREQ(1,I2,IV)*
     &                  FREQ(1,I1,IV)*FREQ(1,I2,IV) +
     &                  CHAM(INI,IM2)*CHAM(INJ,IM1)*FREQ(1,I1,IV)*
     &                  FREQ(1,I2,IV)*SPECMR(IL,ISM)*FREQ(1,I1,IV)*
     &                  FREQ(1,I2,IV)
                      SPECI = SPECI + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                  SPECMI(IL,ISM)*FREQ(1,I1,IV)*FREQ(1,I2,IV)*
     &                  FREQ(1,I1,IV)*FREQ(1,I2,IV) -
     &                  CHAM(INI,IM2)*CHAM(INJ,IM1)*FREQ(1,I1,IV)*
     &                  FREQ(1,I2,IV)*SPECMI(IL,ISM)*FREQ(1,I1,IV)*
     &                  FREQ(1,I2,IV)
C
                    ENDIF
C
                  ENDIF
C                 -----
C
  110           CONTINUE
  100         CONTINUE
C
              IF (IOPTCH.EQ.2) THEN
                SPECR = SPECR * 4.D0 * PI * PI
                SPECI = SPECI * 4.D0 * PI * PI
              ELSE IF (IOPTCH.EQ.3) THEN
                SPECR = SPECR * 16.D0 * PI * PI * PI * PI
                SPECI = SPECI * 16.D0 * PI * PI * PI * PI
              ENDIF
              IF ((KVAL(1) .EQ. KVAL(3)) .AND.
     &            (KVAL(2) .EQ. KVAL(4))) THEN
                ZR(ISPEC-1+IL) = SPECR
              ELSE
                ZR(ISPEC+2*(IL-1)  ) = SPECR
                ZR(ISPEC+2*(IL-1)+1) = SPECI
              ENDIF
   90       CONTINUE
C
   70     CONTINUE
C
   60   CONTINUE
   20 CONTINUE
C
      CALL JEDEMA()
      END
