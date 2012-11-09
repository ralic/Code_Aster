      SUBROUTINE SPEPH1 ( INTPHY, INTMOD, NOMU, CHAM, SPECMR,
     &                   SPECMI, NNOE, NOMCMP, NBMODE, NBN, NBPF )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      LOGICAL             INTPHY, INTMOD
      INTEGER             NBMODE, NBN, NBPF
      REAL*8              CHAM(NBN,*), SPECMR(NBPF,*), SPECMI(NBPF,*)
      CHARACTER*8         NOMU, NNOE(*), NOMCMP(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C  LA BASE MODALE EST DEFINIE PAR UN CONCEPT MODE_MECA
C-----------------------------------------------------------------------
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
C IN  : CHAM   : CHAMP DE GRANDEURS MODALES AUX NOEUDS DE REPONSE
C IN  : SPECMR : VECTEUR DE TRAVAIL
C IN  : SPECMI : VECTEUR DE TRAVAIL
C IN  : NNOE   : LISTE DES NOEUDS OU LA REPONSE EST CALCULEE
C IN  : NBMODE : NBR. DE MODES PRIS EN COMPTE
C IN  : NBN    : NBR. DE NOEUDS DE REPONSE
C IN  : NBPF   : NBR. DE POINTS DE LA DISCRETISATION FREQUENTIELLE
C-----------------------------------------------------------------------
C
      INTEGER       NBPAR, INJ, IDEBN, INI, IL,
     +              IM2, IDEBM, IM1, ISM
      INTEGER       NBABS,ISPEC,MXVAL,LNOEI,LNOEJ,LCMPI,LCMPJ,IJ
      PARAMETER   ( NBPAR = 5 )
      REAL*8        SPECR, SPECI
      CHARACTER*24  KVAL(NBPAR)
      CHARACTER*24 CHNOEI,CHNOEJ,CHCMPI,CHCMPJ,CHVALS
C
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C    --- CREATION ET REMPLISSAGE DES FONCTIONS - SPECTRES REPONSES
C
      CHNOEI = NOMU//'.NOEI'
      CHNOEJ = NOMU//'.NOEJ'
      CHCMPI = NOMU//'.CMPI'
      CHCMPJ = NOMU//'.CMPJ'
      CHVALS = NOMU//'.VALE'

      IF ( INTPHY ) THEN
        MXVAL = NBN*(NBN+1)/2
      ELSE
        MXVAL = NBN
      ENDIF

      CALL WKVECT(CHNOEI,'G V K8',MXVAL,LNOEI)
      CALL WKVECT(CHNOEJ,'G V K8',MXVAL,LNOEJ)
      CALL WKVECT(CHCMPI,'G V K8',MXVAL,LCMPI)
      CALL WKVECT(CHCMPJ,'G V K8',MXVAL,LCMPJ)
      CALL JECREC(CHVALS,'G V R','NU','DISPERSE','VARIABLE',MXVAL)

      IJ = 0
      DO 60 INJ = 1,NBN
C
         KVAL(3) = NNOE(INJ)
         KVAL(4) = NOMCMP(INJ)
C
         IDEBN = INJ
         IF ( INTPHY ) IDEBN = 1
C
         DO 70 INI = IDEBN,INJ
C
            IJ = IJ+1
            KVAL(1) = NNOE(INI)
            KVAL(2) = NOMCMP(INI)
C
            ZK8(LNOEI-1+IJ) = KVAL(1)(1:8)
            ZK8(LNOEJ-1+IJ) = KVAL(3)(1:8)
            ZK8(LCMPI-1+IJ) = KVAL(2)(1:8)
            ZK8(LCMPJ-1+IJ) = KVAL(4)(1:8)

            IF ((KVAL(1) .EQ. KVAL(3)) .AND.
     &            (KVAL(2) .EQ. KVAL(4)))  THEN
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
              DO 100 IM2 = 1 , NBMODE
C
                IDEBM = IM2
                IF ( INTMOD ) IDEBM = 1
C
                DO 110 IM1 = IDEBM,IM2
                  ISM = (IM2* (IM2-1))/2 + IM1
C
                  IF (IM1.EQ.IM2) THEN
C                 --------------------
C
                    SPECR = SPECR + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                        SPECMR(IL,ISM)
C
C
                  ELSE
C                 ----
C
                      SPECR = SPECR + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                      SPECMR(IL,ISM) + CHAM(INI,IM2)*
     &                      CHAM(INJ,IM1)*SPECMR(IL,ISM)
                      SPECI = SPECI + CHAM(INI,IM1)*CHAM(INJ,IM2)*
     &                      SPECMI(IL,ISM) - CHAM(INI,IM2)*
     &                      CHAM(INJ,IM1)*SPECMI(IL,ISM)
C
                  ENDIF
C                 -----
C
  110           CONTINUE
  100         CONTINUE
C
              IF ((KVAL(1) .EQ. KVAL(3)) .AND.
     &            (KVAL(2) .EQ. KVAL(4)))  THEN
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
C
      CALL JEDEMA()
      END
