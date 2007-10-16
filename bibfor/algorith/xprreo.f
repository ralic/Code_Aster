      SUBROUTINE XPRREO(MODEL,NOMA,FISS,NOESOM,NORESI,CNSLN,CNSLT,
     &                  CNSGLN,CNSGLT,DELTAT,LCMIN,ISOZRO,CNXINV)
      IMPLICIT NONE
      REAL*8         DELTAT,LCMIN
      CHARACTER*8    MODEL,NOMA,FISS
      CHARACTER*19   CNSLN,CNSLT,CNSGLN,CNSGLT,NORESI,NOESOM,ISOZRO,
     &               CNXINV

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MASSIN P.MASSIN
C TOLE CRP_20
C     ------------------------------------------------------------------
C
C       XPRREO   : X-FEM PROPAGATION : REORTHOGONALISATION DE LEVEL SET
C       ------     -     --            ---
C    REORTHOGONALISATION D'UNE LEVEL SET APRES PROPAGATION
C
C    ENTREE
C        MODEL   : NOM DU CONCEPT MODELE
C        NOMA    : NOM DU CONCEPT MAILLAGE
C        FISS    : NOM DU CONCEPT FISSURE XFEM
C        NOESOM  : INDICATEUR LOGIQUE DE NOEUD SOMMET
C        NORESI  : VECTEUR LOGIQUE INDIQUANT SI LE RESIDU EST A ESTIMER
C                   SUR LE NOEUD
C        CNSLN   : CHAM_NO_S  LEVEL SET NORMALE
C        CNSLT   : CHAM_NO_S  LEVEL SET TANGENTE
C        CNSGLN  : CHAM_NO_S  GRADIENT DE LEVEL SET NORMALE
C        CNSGLT  : CHAM_NO_S  GRADIENT DE LEVEL SET TANGENTE
C        DELTAT  : PAS DE TEMPS
C        LCMIN   : LONGUEUR CARACTERISTIQUE MINIMALE DES ELEMENTS
C        ISOZRO  : VECTEUR LOGIQUE INDIQUANT SI LA "VRAIE" LEVEL SET
C                   (DISTANCE SIGNEE) A ETE CALCULEE AU NOEUD
C        CNXINV  : MATRICE DE CONNECTIVITE INVERSEE
C
C    SORTIE
C        CNSLT   : CHAM_NO_S  LEVEL SET TANGENTE REORTHOGONALISEE
C        CNSGLT  : CHAM_NO_S  GRADIENT DE LEVEL SET TANGENTE
C
C     ------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER        I,IFM,NIV,NBNO,NBMA,IRET,JCONX1,JCONX2,JMAI,ADDIM,
     &               NDIM,JLNNO,JGLNNO,JLTNO,JGLTNO,JVI,JVIL,JWI,JWIL,
     &               JMEAST,JMESTL,JMESTD,JVF,JVFL,JGDF,JGDFL,ITEMP,IMA,
     &               JDELFI,JDEFIL,JDEFID,JALPHA,JALPHL,JALPHD,NBNOMA,
     &               ITYPMA,IADMET,IADDFI,INO,IADALP,NUNO,IBID,J,JZERO,
     &               JNOSOM,JRESDU,NMANOI,JMANOI,CPTNOV,IMAI,NUMAI,JCOOR
      REAL*8         SIGNLN,NORMGN,NORMGT,SIGMLT,SDIFF,LTPREC,LTNOUV,
     &               R8PREM,SIGLST,SDIFFT,GRAD(3),JI(3),PROSCA,SIGLSI
      CHARACTER*3    ITERK3,ITRPK3
      CHARACTER*8    K8B,LPAIN(4),LPAOUT(2),TYPMA,LICMP(3),NOMNO,METHOD
      CHARACTER*10   RESK10,REMK10,RETK10
      CHARACTER*19   CNOLT,CNOGLT,CELGLT,CHAMS,CELDFI,CESDFI,CELALF,
     &               CESALF,CNSVI,CNSWI,CNSVF,CNSGDF,CNOGDF,CELGDF,MAI
      CHARACTER*24   LIGREL,LCHIN(4),LCHOUT(2)

      REAL*8         RESIMX
      PARAMETER      (RESIMX=1.D-3)
      INTEGER        ITERMX
      PARAMETER      (ITERMX=100)
      REAL*8         RESIDU(ITERMX),RESIT(ITERMX)

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

C  RECUPERATION DES CARACTERISTIQUES DU MAILLAGE
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IRET)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8B,IRET)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
      MAI = NOMA//'.TYPMAIL'
      CALL JEVEUO(MAI,'L',JMAI)
      CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
      NDIM=ZI(ADDIM-1+6)
      LIGREL = MODEL//'.MODELE'

C   RECUPERATION DE LA METHODE DE REINITIALISATION A EMPLOYER
      CALL GETVTX(' ','METHODE',1,1,1,METHOD,IBID)
      WRITE(IFM,*) '   UTILISATION DE LA METHODE '//METHOD
      IF (METHOD.EQ.'UPWIND')  CALL U2MESS('F','XFEM2_24')

C   RECUPERATION DE L'ADRESSE DES VALEURS DE LS ET DU GRADIENT DE LS
      CALL JEVEUO(CNSLN//'.CNSV','L',JLNNO)
      CALL JEVEUO(CNSGLN//'.CNSV','L',JGLNNO)
      CALL JEVEUO(CNSLT//'.CNSV','E',JLTNO)
      CALL JEVEUO (CNSGLT//'.CNSV','E',JGLTNO)

C  RECUPERATION DE L'ADRESSE DE L'INFORMATION 'NOEUD SOMMET'
      CALL JEVEUO(NOESOM,'L',JNOSOM)

C  RECUPERATION DE L'ADRESSE DE L'INFORMATION 'RESIDU A CALCULER'
      CALL JEVEUO(NORESI,'L',JRESDU)

C  RECUPERATION DES NOEUDS DONT LA "VRAIE" LST A ETE CALCULEE
      CALL JEVEUO(ISOZRO,'L',JZERO)

C---------------------------------
C   CREATION DES OBJETS VOLATILES
C---------------------------------
C   LEVEL SET TANGENTE
      CNOLT = '&&XPRREO.CNOLT'
C   GRADIENT DE LA LEVEL SET
      CNOGLT =  '&&XPRREO.CNOGLT'
      CELGLT =  '&&XPRREO.CELGLT'
      CHAMS =  '&&XPRREO.CHAMS'
C   V AU NOEUDS
      CNSVI = '&&XPRREO.CNSVI'
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSVI)
      CALL JEVEUO(CNSVI//'.CNSV','E',JVI)
      CALL JEVEUO(CNSVI//'.CNSL','E',JVIL)
C   W AU NOEUDS
      CNSWI = '&&XPRREO.CNSWI'
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSWI)
      CALL JEVEUO(CNSWI//'.CNSV','E',JWI)
      CALL JEVEUO(CNSWI//'.CNSL','E',JWIL)

      IF (METHOD.EQ.'SIMPLEXE') THEN
C   DELTA_PHI
         CELDFI = '&&XPRREO.CELDFI'
         CESDFI = '&&XPRREO.CESDFI'
C   ALPHA
         CELALF = '&&XPRREO.CELALF'
         CESALF = '&&XPRREO.CESALF'
      ENDIF

C-------------------------------------------------------------
C   CALCUL DU CHAM_NO_S VECT_F = SIGN(LN)*GRAD(LN)/|GRAD(LN)|
C-------------------------------------------------------------
      LICMP(1)='X1'
      LICMP(2)='X2'
      LICMP(3)='X3'
      CNSVF =  '&&XPRREO.CNSVF'
      CALL CNSCRE(NOMA,'NEUT_R',3,LICMP,'V',CNSVF)
      CALL JEVEUO(CNSVF//'.CNSV','E',JVF)
      CALL JEVEUO(CNSVF//'.CNSL','E',JVFL)

      DO 10 I=1,NBNO
         ZL(JVFL-1+3*(I-1)+1)=.TRUE.
         ZL(JVFL-1+3*(I-1)+2)=.TRUE.
         ZL(JVFL-1+3*(I-1)+3)=.TRUE.
         NORMGN = ( ZR(JGLNNO-1+3*(I-1)+1)**2.D0 +
     &              ZR(JGLNNO-1+3*(I-1)+2)**2.D0 +
     &              ZR(JGLNNO-1+3*(I-1)+3)**2.D0 )**0.5D0

         IF (NORMGN.LT.R8PREM().OR.ABS(ZR(JLNNO-1+I)).LT.R8PREM()) THEN
            ZR(JVF-1+3*(I-1)+1) = 0.D0
            ZR(JVF-1+3*(I-1)+2) = 0.D0
            ZR(JVF-1+3*(I-1)+3) = 0.D0
         ELSE
            SIGNLN = ZR(JLNNO-1+I) / ABS(ZR(JLNNO-1+I))
            ZR(JVF-1+3*(I-1)+1) = SIGNLN*ZR(JGLNNO-1+3*(I-1)+1) / NORMGN
            ZR(JVF-1+3*(I-1)+2) = SIGNLN*ZR(JGLNNO-1+3*(I-1)+2) / NORMGN
            ZR(JVF-1+3*(I-1)+3) = SIGNLN*ZR(JGLNNO-1+3*(I-1)+3) / NORMGN
         ENDIF

 10   CONTINUE

C-----------------------------------------------------------------------
      IF (METHOD.EQ.'SIMPLEXE') THEN
C----------------------------------------
C   RECUPERATION DE |T| SUR LES ELEMENTS
C----------------------------------------
         CALL JEVEUO (FISS//'.PRO.MES_EL'//'.CESV','L',JMEAST)
         CALL JEVEUO (FISS//'.PRO.MES_EL'//'.CESL','L',JMESTL)
         CALL JEVEUO (FISS//'.PRO.MES_EL'//'.CESD','L',JMESTD)
      ENDIF
C-----------------------------------------------------------------------
      CNSGDF =  '&&XPRREO.CNSGDF'
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSGDF)
      CALL JEVEUO(CNSGDF//'.CNSV','E',JGDF)
      CALL JEVEUO(CNSGDF//'.CNSL','E',JGDFL)
      CNOGDF = '&&XPRREO.CNOGDF'
      CELGDF =  '&&XPRREO.CELGDF'

      CALL CNSCNO(CNSLT,' ','NON','V',CNOLT)
C-----BOUCLE PRINCIPALE-------------------------------------------------
      DO 995 ITEMP=1,ITERMX
         DO 110 I=1,NBNO
            ZL(JVIL-1+I) = .TRUE.
            ZL(JWIL-1+I) = .TRUE.
            ZR(JVI-1+I) = 0.D0
            ZR(JWI-1+I) = 0.D0
 110     CONTINUE

C--------------------------------------
C   CALCUL DE GRAND F SUR LES ELEMENTS
C--------------------------------------
         DO 90 I=1,NBNO
            NORMGT = ( ZR(JGLTNO-1+3*(I-1)+1)**2.D0 +
     &                 ZR(JGLTNO-1+3*(I-1)+2)**2.D0 +
     &                 ZR(JGLTNO-1+3*(I-1)+3)**2.D0 )**0.5D0
            ZL(JGDFL-1+I) = .TRUE.
            IF (NORMGT.EQ.0.D0) THEN
               ZR(JGDF-1+I)=0.D0
            ELSE
               ZR(JGDF-1+I)=
     &           ( ZR(JGLTNO-1+3*(I-1)+1)*ZR(JVF-1+3*(I-1)+1) +
     &             ZR(JGLTNO-1+3*(I-1)+2)*ZR(JVF-1+3*(I-1)+2) +
     &             ZR(JGLTNO-1+3*(I-1)+3)*ZR(JVF-1+3*(I-1)+3) ) / NORMGT
            ENDIF
 90      CONTINUE
         CALL CNSCNO(CNSGDF,' ','NON','V',CNOGDF)

         LPAIN(1)='PNEUTR'
         LCHIN(1)=CNOGDF
         LPAOUT(1)='PMOYEL'
         LCHOUT(1)=CELGDF

         CALL CALCUL('S','MOY_NOEU_S',LIGREL,1,LCHIN,LPAIN,1,
     &               LCHOUT,LPAOUT,'V')

C-----------------------------------------------------------------------
         IF (METHOD.EQ.'SIMPLEXE') THEN

C---------------------------------------------------------
C     CALCUL DU CHAM_ELEM DELTA_PHI ET DU CHAM_ELNO ALPHA
C---------------------------------------------------------
            CALL CNSCNO(CNSGLT,' ','NON','V',CNOGLT)
            LPAIN(1)='PLSNO'
            LCHIN(1)=CNOLT
            LPAIN(2)='PGRLS'
            LCHIN(2)=CNOGLT
            LPAIN(3)='PGRANDF'
            LCHIN(3)=CELGDF
            LPAIN(4)='PNIELNO'
            LCHIN(4)=FISS//'.PRO.NORMAL'
            LPAOUT(1)='PDPHI'
            LCHOUT(1)=CELDFI
            LPAOUT(2)='PALPHA'
            LCHOUT(2)=CELALF

            CALL CALCUL('S','XFEM_SMPLX_CALC',LIGREL,4,LCHIN,LPAIN,2,
     &                  LCHOUT,LPAOUT,'V')
            CALL CELCES (CELDFI,'V',CESDFI)
            CALL JEVEUO (CESDFI//'.CESV','L',JDELFI)
            CALL JEVEUO (CESDFI//'.CESL','L',JDEFIL)
            CALL JEVEUO (CESDFI//'.CESD','L',JDEFID)
            CALL CELCES (CELALF,'V',CESALF)
            CALL JEVEUO (CESALF//'.CESV','L',JALPHA)
            CALL JEVEUO (CESALF//'.CESL','L',JALPHL)
            CALL JEVEUO (CESALF//'.CESD','L',JALPHD)

C---------------------------------------
C     CALCUL DES CHAMPS NODAUX VI ET WI
C---------------------------------------
C   BOUCLE SUR LES MAILLES DU MAILLAGE
            DO 120 IMA = 1,NBMA
               NBNOMA = ZI(JCONX2+IMA) - ZI(JCONX2+IMA-1)
C   VERIFICATION DU TYPE DE MAILLE
               ITYPMA=ZI(JMAI-1+IMA)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
               IF ((TYPMA(1:5).NE.'TETRA').AND.
     &             (TYPMA(1:4).NE.'HEXA'))    GOTO 120
C   BOUCLE SUR LES NOEUDS DE LA MAILLE
               CALL CESEXI('S',JMESTD,JMESTL,IMA,1,1,1,IADMET)
               CALL CESEXI('S',JDEFID,JDEFIL,IMA,1,1,1,IADDFI)
               DO 130 INO = 1,NBNOMA
                  CALL CESEXI('S',JALPHD,JALPHL,IMA,INO,1,1,IADALP)
                  NUNO = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INO-1)
                  ZR(JVI-1+NUNO) = ZR(JVI-1+NUNO) + ZR(JALPHA-1+IADALP)
     &                                        * ZR(JDELFI-1+IADDFI)
                  ZR(JWI-1+NUNO) = ZR(JWI-1+NUNO) + ZR(JALPHA-1+IADALP)
     &                                         * ZR(JMEAST-1+IADMET)
 130           CONTINUE
 120        CONTINUE

C-----------------------------------------------------------------------
         ELSEIF (METHOD.EQ.'UPWIND') THEN

         ENDIF
C-----------------------------------------------------------------------

C---------------------------------------
C     CALCUL DE LA LEVEL SET RESULTANTE
C---------------------------------------
         SIGMLT = 0.D0
         SDIFF = 0.D0
         SDIFFT = 0.D0
         SIGLST = 0.D0
         DO 200 I=1,NBNO
C  ON ECARTE LES NOEUDS MILIEUX
            IF (.NOT.ZL(JNOSOM-1+I)) GOTO 200
C  ON ECARTE LES NOEUDS CALCULES PRECEDEMMENT
            IF (ZL(JZERO-1+I)) GOTO 200
            LTPREC = ZR(JLTNO-1+I)
            IF (ABS(ZR(JWI-1+I)).GT.R8PREM()) THEN
               ZR(JLTNO-1+I) = ZR(JLTNO-1+I) - DELTAT *
     &                       ( ZR(JVI-1+I) / ZR(JWI-1+I) )
               LTNOUV = ZR(JLTNO-1+I)
               IF (ZL(JRESDU-1+I)) THEN
                  SDIFF = SDIFF + (LTNOUV-LTPREC)**2.0D0
                  SIGMLT = SIGMLT + LTPREC**2.0D0
               ENDIF
               SDIFFT = SDIFFT + (LTNOUV-LTPREC)**2.0D0
               SIGLST = SIGLST + LTPREC**2.0D0
            ENDIF
 200     CONTINUE

C  CAS OU TOUS LES RESIDUS A ESTIMER SONT CALCULES
         IF (SDIFF.EQ.0.D0 .AND. SIGMLT.EQ.0.D0) THEN
            RESIDU(ITEMP) = 0.D0
         ELSE
            RESIDU(ITEMP) = (SDIFF/SIGMLT)**0.5D0
         ENDIF
         IF (SDIFFT.EQ.0.D0 .AND. SIGLST.EQ.0.D0) THEN
            RESIT(ITEMP) = 0.D0
         ELSE
            RESIT(ITEMP) = (SDIFFT/SIGLST)**0.5D0
         ENDIF

C---------------------------------
C     CALCUL DES NOEUDS DONT WI=0
C---------------------------------
         DO 800 I=1,NBNO
C  ON ECARTE LES NOEUDS MILIEUX
            IF (.NOT.ZL(JNOSOM-1+I)) GOTO 800
C  ON ECARTE LES NOEUDS CALCULES PLUS HAUT
            IF (ZL(JZERO-1+I)) GOTO 800
            IF (ABS(ZR(JWI-1+I)).LT.R8PREM()) THEN
               CPTNOV = 0
               SIGLSI = 0.D0
C    RECUPERATION DES MAILLES CONTENANT LE NOEUD I
               CALL JELIRA(JEXNUM(CNXINV,I),'LONMAX',NMANOI,K8B)
               CALL JEVEUO(JEXNUM(CNXINV,I),'L',JMANOI)
C     BOUCLE SUR LES MAILLES CONTENANT LE NOEUD I
               DO 160 IMAI=1,NMANOI
                  NUMAI = ZI(JMANOI-1+IMAI)
                  ITYPMA = ZI(JMAI-1+NUMAI)
                  CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
C     SI MAILLE NON VOLUMIQUE ON SAUTE LA MAIILE
                  IF (TYPMA(1:4).NE.'HEXA'.AND.TYPMA(1:5).NE.'TETRA')
     &               GOTO 160
C     BOUCLE SUR LES NOEUDS DE LA MAILLE
                  NBNOMA = ZI(JCONX2+NUMAI) - ZI(JCONX2+NUMAI-1)
                  DO 170 INO=1,NBNOMA
                     NUNO=ZI(JCONX1-1+ZI(JCONX2+NUMAI-1)+INO-1)
                     IF (.NOT.ZL(JNOSOM-1+NUNO)) GOTO 170
                     IF (ABS(ZR(JWI-1+NUNO)).GT.R8PREM()) THEN
                        DO 175 J=1,3
                           JI(J) = ZR(JCOOR-1+3*(I-1)+J)
     &                            -ZR(JCOOR-1+3*(NUNO-1)+J)
                           GRAD(J) = ZR(JGLTNO-1+NDIM*(NUNO-1)+J)
 175                    CONTINUE
                        CALL LCPRSN(3,JI,GRAD,PROSCA)
                        SIGLSI = SIGLSI + ZR(JLTNO-1+NUNO) + PROSCA
                        CPTNOV = CPTNOV+1
                  ENDIF
 170              CONTINUE
 160           CONTINUE
               IF (CPTNOV.NE.0) THEN
                  ZR(JLTNO-1+I) = SIGLSI/CPTNOV
               ELSE
                  ZR(JLTNO-1+I) = ZR(JLTNO-1+I) - DELTAT *
     &            ( ZR(JGLTNO-1+3*(I-1)+1) * ZR(JVF-1+3*(I-1)+1) +
     &              ZR(JGLTNO-1+3*(I-1)+2) * ZR(JVF-1+3*(I-1)+2) +
     &              ZR(JGLTNO-1+3*(I-1)+3) * ZR(JVF-1+3*(I-1)+3) )
               ENDIF
            ENDIF
 800     CONTINUE

C---------------------------------------------------
C     CALCUL DU GRADIENT DE LA LEVEL SET RESULTANTE
C---------------------------------------------------
         CALL CNSCNO (CNSLT,' ','NON','V',CNOLT)
         LPAIN(1)='PGEOMER'
         LCHIN(1)=NOMA//'.COORDO'
         LPAIN(2)='PNEUTER'
         LCHIN(2)=CNOLT
         LPAOUT(1)='PGNEUTR'
         LCHOUT(1)=CELGLT

         CALL CALCUL('S','GRAD_NEUT_R',LIGREL,2,LCHIN,LPAIN,1,LCHOUT,
     &               LPAOUT,'V')

C  PASSAGE D'UN CHAM_ELNO EN UN CHAM_NO
         CALL CELCES (CELGLT, 'V', CHAMS)
         CALL CESCNS (CHAMS, ' ', 'V', CNSGLT)
         CALL JEVEUO (CNSGLT//'.CNSV','E',JGLTNO)

C-------------------------------------------------
C     TESTS DES CONDITIONS DE SORTIE DE LA BOUCLE
C-------------------------------------------------
C  CONVERGENCE ATTEINTE
         IF (RESIDU(ITEMP).LT.RESIMX) GOTO 999
C  NOMBRE D'ITERATION MAXI
         IF (ITEMP.EQ.ITERMX) GOTO 999
C  MINIMUM LOCAL DU RESIDU GLOBAL ATTEINT
         IF (ITEMP.GT.5) THEN
            IF ((RESIT(ITEMP)-RESIT(ITEMP-1)).GE.0.D0)  GOTO 999
         ENDIF

 995  CONTINUE
C-----FIN DE LA BOUCLE PRINCIPALE---------------------------------------
 999  CONTINUE

C-------------------------------------
C     AFFICHAGE DES INFOS UTILISATEUR
C-------------------------------------
C      IF(NIV.GT.1) THEN
         WRITE(IFM,*) '   REORTHOGONALISATION DE LT :'
         WRITE(IFM,900)
         WRITE(IFM,901)
         WRITE(IFM,902)
         WRITE(IFM,903)
         DO 300 I=1,ITEMP
            WRITE(IFM,904)I,RESIDU(I),RESIT(I)
 300     CONTINUE
         WRITE(IFM,903)
C      ENDIF

      CALL CODENT(ITEMP,'D',ITERK3)
      CALL CODREE(RESIDU(ITEMP),'E',RESK10)
      CALL CODREE(RESIT(ITEMP),'E',RETK10)

C  CONVERGENCE ATTEINTE
      IF (RESIDU(ITEMP).LT.RESIMX)
     &   WRITE(IFM,*)'   CONVERGENCE ATTEINTE A L''ITERATION '//ITERK3

C  MINIMUM LOCAL ATTEINT
      IF (ITEMP.GT.5.AND.(RESIT(ITEMP)-RESIT(ITEMP-1)).GE.0.D0) THEN
         WRITE(IFM,*)'   MINIMUM LOCAL DU RESIDU GLOBAL ATTEINT.'
         WRITE(IFM,*)'   ARRET A L''ITERATION '//ITERK3
      ENDIF

C  NOMBRE MAXI D'ITERATIONS ATTEINT
      IF (ITEMP.EQ.ITERMX)
     &   WRITE(IFM,*)'   NOMBRE MAX D''ITERATION ('//ITERK3//') ATTEINT'

      WRITE(IFM,*)'   RESIDU LOCAL  = '//RESK10
      WRITE(IFM,*)'   RESIDU GLOBAL = '//RETK10

      CALL ASSERT(ITEMP.LE.ITERMX)

C   DESTRUCTION DES OBJETS VOLATILES
      CALL JEDETR(CNOLT)
      CALL JEDETR(CNOGLT)
      CALL JEDETR(CELGLT)
      CALL JEDETR(CHAMS)
      CALL JEDETR(CNSVI)
      CALL JEDETR(CNSWI)
      CALL JEDETR(CNSVF)

      IF (METHOD.EQ.'SIMPLEXE') THEN
         CALL JEDETR(CELDFI)
         CALL JEDETR(CESDFI)
         CALL JEDETR(CELALF)
         CALL JEDETR(CESALF)
         CALL JEDETR(CELGDF)
         CALL JEDETR(CNSGDF)
         CALL JEDETR(CNOGDF)
      ENDIF
 900  FORMAT(3X,'+',11('-'),'+',12('-'),'+',12('-'),'+')
 901  FORMAT('   | ITERATION |   RESIDU   |   RESIDU   |')
 902  FORMAT('   |           |   LOCAL    |   GLOBAL   |')
 903  FORMAT(3X,'+',11('-'),'+',12('-'),'+',12('-'),'+')
 904  FORMAT(3X,'|',5X,I3,2X,2(' |',E11.4),' | ')

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
