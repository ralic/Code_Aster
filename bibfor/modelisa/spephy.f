      SUBROUTINE SPEPHY(IOPTCH,INTPHY,INTMOD,NOMU,TABLE,FREQ,CHAM,
     &                  SPECMR,SPECMI,DISC,NNOE,NOMCMP,NUOR,NBMR,NPV,
     &                  NBN,IMOD1,NBPF,NBM,VITE)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 17/12/2002   AUTEUR CIBHHGB G.BERTRAND 
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
C IN  : NPV    : NBR. DE VITESSES ETUDIEES
C IN  : NBN    : NBR. DE NOEUDS DE REPONSE
C IN  : NBFO1  : NBR. DE SPECTRES D EXCITATION DEFINIS
C IN  : IMOD1  : INDICE DU PREMIER MODE PRIS EN COMPTE DANS LA BASE DE
C                CONCEPT MELASFLU
C IN  : NBPF   : NBR. DE POINTS DE LA DISCRETISATION FREQUENTIELLE
C IN  : NBM    : NBR. DE MODES DE LA BASE DE CONCEPT MELASFLU
C IN  : VITE   : TABLEAU DES VITESSES DE FLUIDE
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
      LOGICAL      INTPHY,INTMOD
      INTEGER      IOPTCH,NBMR,NPV,NBN,IMOD1,NBPF,NBM
      INTEGER      NUOR(NBMR)
      REAL*8       CHAM(NBN,NBMR),SPECMR(NBPF,*),SPECMI(NBPF,*)
      REAL*8       DISC(*), FREQ(2,NBM,*), VITE(*)
      CHARACTER*8  NOMU, TABLE, NNOE(NBN), NOMCMP
C
      PARAMETER   ( NBPAR = 7 )
      INTEGER       IBID, IVAL(3)
      REAL*8        R8B, PI, R8PI
      CHARACTER*8   K8B
      CHARACTER*16  NOPAIN(3), NOPAOU(NBPAR)
      CHARACTER*19  NOMCOD
      CHARACTER*24  NOMFON, VALE, PROL, KVAL(5)
      COMPLEX*16    C16B
C
      DATA NOPAIN / 'NUME_VITE_FLUI' , 'NUME_ORDRE_I' , 'NUME_ORDRE_J' /
      DATA NOPAOU / 'NUME_VITE_FLUI' , 'VITE_FLUIDE' ,
     +              'NOEUD_I'  , 'NOM_CMP_I'  , 
     +              'NOEUD_J'  , 'NOM_CMP_J' , 'FONCTION'     /
C-----------------------------------------------------------------------
      CALL JEMARQ()
      PI = R8PI()
C
C --- POUR CHAQUE PAS EN VITESSE ON CALCULE L INTERSPECTRE DE REPONSE
C
      DO 20 IV = 1,NPV
C
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
        CALL TBLIVA ( TABLE, 3, NOPAIN, IVAL, R8B, C16B, K8B, K8B,
     +         R8B, 'FONCTION', K8B, IBID, R8B, C16B, NOMFON, IRET )
        IF ( IRET .NE. 0 ) GO TO 20
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
            CALL TBLIVA ( TABLE, 3, NOPAIN, IVAL, R8B, C16B, K8B, K8B,
     +             R8B, 'FONCTION', K8B, IBID, R8B, C16B, NOMFON, IRET )
            IF ( IRET .NE. 0 ) CALL UTMESS('F','SPEPHY','Y A UN BUG' )
C
            CALL JEVEUO ( NOMFON(1:19)//'.VALE', 'L', IFON ) 
            ISJ = (IMJ* (IMJ-1))/2 + IMI
            IF ( ISJ .EQ. 1 ) THEN
               DO 51 IF1 = 1,NBPF
                  DISC(IF1) = ZR(IFON+ (IF1-1))
   51          CONTINUE
            ENDIF
C
            DO 50 IF1 = 1,NBPF
               SPECMR(IF1,ISJ) = ZR(IFON+NBPF+ (IF1-1)*2)
               SPECMI(IF1,ISJ) = ZR(IFON+NBPF+ (IF1-1)*2+1)
   50       CONTINUE
   40     CONTINUE
C
   30   CONTINUE
C
C    --- CREATION ET REMPLISSAGE DES FONCTIONS - SPECTRES REPONSES
C
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
            KVAL(1) = NNOE(INI)
            KVAL(2) = NOMCMP
C
            WRITE (NOMCOD,'(A8,A2,3I3.3)') NOMU,'.S',IV,INI,INJ
C
            KVAL(5) = NOMCOD
            CALL TBAJLI ( NOMU, NBPAR, NOPAOU, 
     +                          IVAL, VITE(IV), C16B, KVAL, 0 )
C
            VALE = NOMCOD(1:19)//'.VALE'
            PROL = NOMCOD(1:19)//'.PROL'
            CALL WKVECT(VALE,'G V R ',3*NBPF,LVALE)
            CALL WKVECT(PROL,'G V K16',5,LPROL)
C
            ZK16(LPROL  ) = 'FONCT_C '
            ZK16(LPROL+1) = 'LIN LIN '
            ZK16(LPROL+2) = 'FREQ    '
            ZK16(LPROL+3) = 'DSP     '
            ZK16(LPROL+4) = 'LL      '
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
              ZR(LVALE+NBPF+2* (IL-1)  ) = SPECR
              ZR(LVALE+NBPF+2* (IL-1)+1) = SPECI
   90       CONTINUE
C
   70     CONTINUE
C
   60   CONTINUE
   20 CONTINUE
C
      CALL JEDEMA()
      END
