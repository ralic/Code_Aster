      SUBROUTINE SPEPH1 ( INTPHY, INTMOD, NOMU, CHAM, SPECMR,
     &                   SPECMI, DISC, NNOE, NOMCMP, NBMODE, NBN, NBPF )
      IMPLICIT   NONE
      LOGICAL             INTPHY, INTMOD
      INTEGER             NBMODE, NBN, NBPF
      REAL*8              CHAM(NBN,*), SPECMR(NBPF,*), SPECMI(NBPF,*),
     +                    DISC(*)
      CHARACTER*8         NOMU, NNOE(*), NOMCMP(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       NBPAR, IVAL(2), INJ, IDEBN, INI, LVALE, LPROL, IL,
     +              IM2, IDEBM, IM1, ISM
      PARAMETER   ( NBPAR = 5 )
      REAL*8        R8B, SPECR, SPECI
      CHARACTER*16  NOPAOU(NBPAR)
      CHARACTER*19  NOMCOD
      CHARACTER*24  VALE, PROL, KVAL(NBPAR)
      COMPLEX*16    C16B
C
      DATA NOPAOU / 'NOEUD_I'  , 'NOM_CMP_I'  ,
     +              'NOEUD_J'  , 'NOM_CMP_J' , 'FONCTION_C'     /
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C    --- CREATION ET REMPLISSAGE DES FONCTIONS - SPECTRES REPONSES
C
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
            KVAL(1) = NNOE(INI)
            KVAL(2) = NOMCMP(INI)
C
            WRITE(NOMCOD,'(A8,A3,2I4.4)') NOMU,'.FO',INI,INJ
C
            KVAL(5) = NOMCOD
            CALL TBAJLI ( NOMU, NBPAR, NOPAOU,
     +                          IVAL, R8B, C16B, KVAL, 0 )
C
            VALE = NOMCOD(1:19)//'.VALE'
            PROL = NOMCOD(1:19)//'.PROL'
            CALL WKVECT ( VALE, 'G V R ', 3*NBPF, LVALE )
            CALL WKVECT ( PROL, 'G V K24', 6, LPROL )
C
            ZK24(LPROL  ) = 'FONCT_C '
            ZK24(LPROL+1) = 'LIN LIN '
            ZK24(LPROL+2) = 'FREQ    '
            ZK24(LPROL+3) = 'DSP     '
            ZK24(LPROL+4) = 'LL      '
            ZK24(LPROL+5) = NOMCOD
C
            DO 80 IL = 1,NBPF
              ZR(LVALE+IL-1) = DISC(IL)
   80       CONTINUE
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
              ZR(LVALE+NBPF+2*(IL-1)  ) = SPECR
              ZR(LVALE+NBPF+2*(IL-1)+1) = SPECI
   90       CONTINUE
C
   70     CONTINUE
C
   60   CONTINUE
C
      CALL JEDEMA()
      END
