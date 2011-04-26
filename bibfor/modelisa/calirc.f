      SUBROUTINE CALIRC(CHARGZ)
      IMPLICIT NONE
      CHARACTER*(*) CHARGZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C RESPONSABLE PELLET
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
C TOLE CRP_20

C     CREER LES CARTES CHAR.CHXX.CMULT ET CHAR.CHXX.CIMPO
C          ET REMPLIR LIGRCH, POUR LE MOT-CLE LIAISON_MAIL
C     (COMMANDES AFFE_CHAR_MECA ET AFFE_CHAR_THER)

C IN  : CHARGE : NOM UTILISATEUR DU RESULTAT DE CHARGE
C-----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNUM
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------

      INTEGER K,KK,NUNO1,NUNO2,INO1,INO2,NDIM,IER,NOCC,IOCC
      INTEGER IBID,JNOMA,NNOMX,IDMAX,IDNOMN,IDCOEF,IDNOMD,IGEOM
      INTEGER IDIREC,IDIMEN,IAGMA1,IAGNO2,NBMA1,NBNO2,NBNO2T
      INTEGER NNO1,I,INDIRE,LNO
      INTEGER JCONB,JCONU,JCOCF,JCOM1,IDECAL
      INTEGER JCONB1,JCONU1,JCOCF1,JCOM11,IDECA1
      INTEGER JCONB2,JCONU2,JCOCF2,JCOM12,IDECA2
      INTEGER NBTYP,NDDL2,NBMA2,IDMAI2,JLISTK,JDIM,NDIM1
      INTEGER JNUNOE,JNORM,IDIM,IJ,IMA1,JLISV1
      INTEGER KNO2,KKNO2,JNU2BS,JELIM,JCOOR
      LOGICAL LROTA,DNOR
      REAL*8 BETA,COEF1,MROTA(3,3),ZERO,NORMAL(3)
      COMPLEX*16 BETAC,CBID
      CHARACTER*2 TYPLAG
      CHARACTER*4 FONREE
      CHARACTER*4 TYPCOE,TYPLIA
      CHARACTER*8 NOMA,MO,M8BLAN,KELIM,KBID
      CHARACTER*8 KBETA,NONO1,NONO2,CHARGE,CMP,DDL2,LISTYP(8)
      CHARACTER*16 MOTFAC,TYMOCL(4),MOTCLE(4),NOMCMD
      CHARACTER*16 CORRES,CORRE1,CORRE2,TYPRAC
      CHARACTER*19 LIGRMO,LISREL
      CHARACTER*24 GEOM2
      CHARACTER*24 VALK(2)
      CHARACTER*1 KB
      REAL*8 RBID
C ----------------------------------------------------------------------

      CALL JEMARQ()
      MOTFAC='LIAISON_MAIL'
      CALL GETFAC(MOTFAC,NOCC)
      IF (NOCC.EQ.0)GOTO 320

      CALL GETRES(KB,KB,NOMCMD)
      IF (NOMCMD.EQ.'AFFE_CHAR_MECA') THEN
        TYPLIA='DEPL'
      ELSEIF (NOMCMD.EQ.'AFFE_CHAR_THER') THEN
        TYPLIA='TEMP'
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF


      FONREE='REEL'
      TYPCOE='REEL'
      CHARGE=CHARGZ

      LISREL='&&CALIRC.RLLISTE'
      ZERO=0.0D0
      BETA=0.0D0
      BETAC=(0.0D0,0.0D0)
      KBETA=' '
      TYPLAG='12'
      M8BLAN='        '
      NDIM1=3

      CALL DISMOI('F','NOM_MODELE',CHARGE(1:8),'CHARGE',IBID,MO,IER)
      LIGRMO=MO//'.MODELE'
      CALL JEVEUO(LIGRMO//'.LGRF','L',JNOMA)
      NOMA=ZK8(JNOMA)

      CALL DISMOI('F','DIM_GEOM',MO,'MODELE',IGEOM,KBID,IER)
      IF (IGEOM.EQ.2) THEN
        NDIM=2
      ELSE
        NDIM=3
      ENDIF

      IF (NDIM.EQ.2) THEN
        NBTYP=3
        LISTYP(1)='SEG2'
        LISTYP(2)='SEG3'
        LISTYP(3)='SEG4'
      ELSEIF (NDIM.EQ.3) THEN
        NBTYP=8
        LISTYP(1)='TRIA3'
        LISTYP(2)='TRIA6'
        LISTYP(3)='QUAD4'
        LISTYP(4)='QUAD8'
        LISTYP(5)='QUAD9'
        LISTYP(6)='SEG2'
        LISTYP(7)='SEG3'
        LISTYP(8)='SEG4'
      ENDIF

      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NNOMX,KB,IER)
C     -- IDMAX : NOMBRE MAX DE TERMES D'UNE RELATION LINEAIRE
C              = 2*27 + 3 = 57
      IDMAX=57
      CALL WKVECT('&&CALIRC.NOMNOE','V V K8',IDMAX,IDNOMN)
      CALL WKVECT('&&CALIRC.NOMDDL','V V K8',IDMAX,IDNOMD)
      CALL WKVECT('&&CALIRC.COEF','V V R',IDMAX,IDCOEF)
      CALL WKVECT('&&CALIRC.DIRECT','V V R',IDMAX*3,IDIREC)
      CALL WKVECT('&&CALIRC.DIMENSION','V V I',IDMAX,IDIMEN)


C     &&CALIRC.ELIM(INO) : 0 -> INO PAS ELIMINE
C                          1 -> INO ELIMINE
      CALL WKVECT('&&CALIRC.ELIM','V V I',NNOMX,JELIM)

      CORRES='&&CALIRC.CORRES'
      CORRE1='&&CALIRC.CORRE1'
      CORRE2='&&CALIRC.CORRE2'

      DO 310 IOCC=1,NOCC

C       IL FAUT REMETTRE à ZERO CES 2 OBJETS ENTRE 2 OCCURENCES :
        DO 10,KK=1,IDMAX
          ZI(IDIMEN-1+KK)=0
   10   CONTINUE
        DO 20,KK=1,3*IDMAX
          ZR(IDIREC-1+KK)=0.D0
   20   CONTINUE

        DNOR=.FALSE.
        TYPRAC=' '
        IF (TYPLIA.EQ.'DEPL') THEN
          CALL GETVTX(MOTFAC,'DDL_ESCL',IOCC,1,1,DDL2,NDDL2)
          IF (NDDL2.GT.0)DNOR=.TRUE.
          CALL GETVTX(MOTFAC,'TYPE_RACCORD',IOCC,1,1,TYPRAC,IBID)
          IF (TYPRAC.EQ.'COQUE') CALL ASSERT(NDIM.EQ.3)
          IF (TYPRAC.EQ.'COQUE_MASSIF') CALL ASSERT(NDIM.EQ.3)
          IF (TYPRAC.EQ.'MASSIF_COQUE') CALL ASSERT(NDIM.EQ.3)
        ENDIF

C        1.1 RECUPERATION DE LA LISTE DES MAILLE_MAIT :
C        ----------------------------------------------
        MOTCLE(1)='MAILLE_MAIT'
        TYMOCL(1)='MAILLE'
        MOTCLE(2)='GROUP_MA_MAIT'
        TYMOCL(2)='GROUP_MA'
        CALL RELIEM(MO,NOMA,'NU_MAILLE',MOTFAC,IOCC,2,MOTCLE,TYMOCL,
     &              '&&CALIRC.LIMANU1',NBMA1)
        CALL JEVEUO('&&CALIRC.LIMANU1','L',IAGMA1)


C        1.2 RECUPERATION DES NOEUD_ESCL
C        -------------------------------
        IF (.NOT.DNOR) THEN

C        -- RECUPERATION DE LA LISTE DES NOEUD_ESCL :
C        --------------------------------------------
          MOTCLE(1)='NOEUD_ESCL'
          TYMOCL(1)='NOEUD'
          MOTCLE(2)='GROUP_NO_ESCL'
          TYMOCL(2)='GROUP_NO'
          MOTCLE(3)='MAILLE_ESCL'
          TYMOCL(3)='MAILLE'
          MOTCLE(4)='GROUP_MA_ESCL'
          TYMOCL(4)='GROUP_MA'
          CALL RELIEM(' ',NOMA,'NU_NOEUD',MOTFAC,IOCC,4,MOTCLE,TYMOCL,
     &                '&&CALIRC.LINONU2',NBNO2)
          CALL JEVEUO('&&CALIRC.LINONU2','L',IAGNO2)

        ELSE

C        -- RECUPERATION DE LA LISTE DES MAILLE_ESCL :
C        ---------------------------------------------
          MOTCLE(1)='MAILLE_ESCL'
          TYMOCL(1)='MAILLE'
          MOTCLE(2)='GROUP_MA_ESCL'
          TYMOCL(2)='GROUP_MA'
          CALL RELIEM(MO,NOMA,'NU_MAILLE',MOTFAC,IOCC,2,MOTCLE,TYMOCL,
     &                '&&CALIRC.LIMANU2',NBMA2)
          IF (NBMA2.EQ.0) THEN
            VALK(1)=MOTCLE(1)
            VALK(2)=MOTCLE(2)
            CALL U2MESG('F','MODELISA8_49',2,VALK,0,0,0,0.D0)
          ENDIF
          CALL JEVEUO('&&CALIRC.LIMANU2','L',IDMAI2)

C ---        CREATION DU TABLEAU DES NUMEROS DES NOEUDS '&&NBNLMA.LN'
C ---        ET DES NOMBRES D'OCCURENCES DE CES NOEUDS '&&NBNLMA.NBN'
C ---        DES MAILLES DE PEAU MAILLE_ESCL :
C            -------------------------------
          CALL NBNLMA(NOMA,NBMA2,ZI(IDMAI2),NBTYP,LISTYP,NBNO2)

C ---        CALCUL DES NORMALES EN CHAQUE NOEUD :
C            -----------------------------------
          CALL WKVECT('&&CALIRC.LISTK','V V K8',1,JLISTK)
          CALL JEVEUO('&&NBNLMA.LN','L',JNUNOE)

C ---        CREATION DU TABLEAU D'INDIRECTION ENTRE LES INDICES
C ---        DU TABLEAU DES NORMALES ET LES NUMEROS DES NOEUDS :
C            -------------------------------------------------
          CALL WKVECT('&&CALIRC.INDIRE','V V I',NNOMX,INDIRE)
          CALL JELIRA('&&NBNLMA.LN','LONUTI',LNO,KB)

          DO 30 I=1,LNO
            ZI(INDIRE+ZI(JNUNOE+I-1)-1)=I
   30     CONTINUE

          CALL CANORT(NOMA,NBMA2,ZI(IDMAI2),ZK8(JLISTK),NDIM,NBNO2,
     &                ZI(JNUNOE),1)
          CALL JEVEUO('&&CANORT.NORMALE','L',JNORM)
          CALL JEDUPO('&&NBNLMA.LN','V','&&CALIRC.LINONU2',.FALSE.)
          CALL JEVEUO('&&CALIRC.LINONU2','L',IAGNO2)
        ENDIF


C       1.3 ON ELIMINE DE LINONU2 LES NOEUDS DEJA ELIMINES LORS DES
C           OCCURENCES PRECEDENTES DE LIAISON_MAILLE
C       ---------------------------------------------------------------
        CALL GETVTX(MOTFAC,'ELIM_MULT',IOCC,1,1,KELIM,IBID)
        IF (KELIM.EQ.'NON') THEN
          KKNO2=0
          CALL WKVECT('&&CALIRC.LINONU2BIS','V V I',NBNO2,JNU2BS)
          DO 40,KNO2=1,NBNO2
            NUNO2=ZI(IAGNO2-1+KNO2)
C            -- SI NUNO2 N'EST PAS ENCORE ELIMINE :
            IF (ZI(JELIM-1+NUNO2).EQ.0) THEN
              ZI(JELIM-1+NUNO2)=1
              KKNO2=KKNO2+1
              ZI(JNU2BS+KKNO2)=NUNO2
            ENDIF
   40     CONTINUE
          NBNO2=KKNO2
          CALL JEDETR('&&CALIRC.LINONU2')
          CALL WKVECT('&&CALIRC.LINONU2','V V I',NBNO2,IAGNO2)
          DO 50,KNO2=1,NBNO2
            ZI(IAGNO2-1+KNO2)=ZI(JNU2BS+KNO2)
   50     CONTINUE
          CALL JEDETR('&&CALIRC.LINONU2BIS')
        ENDIF


C       1.4 TRANSFORMATION DE LA GEOMETRIE DE GRNO2 :
C       ------------------------------------------
        GEOM2='&&CALIRC.GEOM_TRANSF'
        CALL CALIRG('LIAISON_MAIL',IOCC,NDIM,NOMA,'&&CALIRC.LINONU2',
     &              GEOM2,MROTA,LROTA)
C       -- LROTA = .TRUE. : ON A UTILISE LE MOT CLE ANGL_NAUT
        IF (TYPRAC.EQ.'COQUE_MASSIF') CALL ASSERT(.NOT.LROTA)
        IF (TYPRAC.EQ.'MASSIF_COQUE') CALL ASSERT(.NOT.LROTA)
        IF (TYPRAC.EQ.'COQUE') CALL ASSERT(.NOT.LROTA)



C       2. CALCUL DE CORRES (+ EVENTUELLEMENT CORRE1, CORRE2) :
C       --------------------------------------------------------
        IF (NDIM.EQ.2) THEN
          CALL ASSERT((TYPRAC.EQ.' ') .OR. (TYPRAC.EQ.'MASSIF'))
          CALL PJ2DCO('PARTIE',MO,MO,NBMA1,ZI(IAGMA1),NBNO2,ZI(IAGNO2),
     &                ' ',GEOM2,CORRES,.FALSE.,RBID)
        ELSEIF (NDIM.EQ.3) THEN
          IF ((TYPRAC.EQ.' ') .OR. (TYPRAC.EQ.'MASSIF')) THEN
            CALL PJ3DCO('PARTIE',MO,MO,NBMA1,ZI(IAGMA1),NBNO2,
     &                  ZI(IAGNO2),' ',GEOM2,CORRES,.FALSE.,RBID)
          ELSEIF (TYPRAC.EQ.'COQUE' .OR. TYPRAC.EQ.'MASSIF_COQUE') THEN
            CALL PJ4DCO('PARTIE',MO,MO,NBMA1,ZI(IAGMA1),NBNO2,
     &                  ZI(IAGNO2),' ',GEOM2,CORRES,.FALSE.,RBID,'NON')
          ELSEIF (TYPRAC.EQ.'COQUE_MASSIF') THEN
            CALL PJ3DCO('PARTIE',MO,MO,NBMA1,ZI(IAGMA1),NBNO2,
     &                  ZI(IAGNO2),' ',GEOM2,CORRES,.FALSE.,RBID)
            CALL WKVECT('&&CALIRC.LISV1','V V R',3*NNOMX,JLISV1)
            CALL CALIR3(MO,NBMA1,ZI(IAGMA1),NBNO2,ZI(IAGNO2),GEOM2,
     &                  CORRE1,CORRE2,JLISV1,IOCC)
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF

        ENDIF

        CALL JEVEUO(CORRES//'.PJEF_NB','L',JCONB)
        CALL JEVEUO(CORRES//'.PJEF_M1','L',JCOM1)
        CALL JEVEUO(CORRES//'.PJEF_NU','L',JCONU)
        CALL JEVEUO(CORRES//'.PJEF_CF','L',JCOCF)

        IF (TYPRAC.EQ.'COQUE_MASSIF') THEN
          CALL JEVEUO(CORRE1//'.PJEF_NB','L',JCONB1)
          CALL JEVEUO(CORRE1//'.PJEF_M1','L',JCOM11)
          CALL JEVEUO(CORRE1//'.PJEF_NU','L',JCONU1)
          CALL JEVEUO(CORRE1//'.PJEF_CF','L',JCOCF1)
          CALL JEVEUO(CORRE2//'.PJEF_NB','L',JCONB2)
          CALL JEVEUO(CORRE2//'.PJEF_M1','L',JCOM12)
          CALL JEVEUO(CORRE2//'.PJEF_NU','L',JCONU2)
          CALL JEVEUO(CORRE2//'.PJEF_CF','L',JCOCF2)
        ENDIF
        CALL JELIRA(CORRES//'.PJEF_NB','LONMAX',NBNO2T,KB)
        CALL ASSERT(NBNO2T.EQ.NNOMX)



C       3. ECRITURE DES RELATIONS LINEAIRES :
C       =====================================


C       3.1 CAS "DEPL" :
C       =================
        IF (TYPLIA.EQ.'DEPL') THEN

C       -- 3.1.1 S'IL N'Y A PAS DE ROTATION :
C       -------------------------------------
          IF (.NOT.LROTA) THEN
            IDECAL=0
            IDECA1=0
            IDECA2=0

C           -- ON BOUCLE SUR TOUS LES NOEUDS DU MAILLAGE :
            DO 140 INO2=1,NBNO2T
C             IMA1: MAILLE A CONNECTER A INO2
              IMA1=ZI(JCOM1-1+INO2)
C             -- SI IMA1=0, C'EST QUE INO2 NE FAIT PAS PARTIE
C                DES NOEUDS ESCLAVES
              IF (IMA1.EQ.0)GOTO 140

C             NNO1: NB NOEUDS DE IMA1
              NNO1=ZI(JCONB-1+INO2)

              NUNO2=INO2
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO2),NONO2)

              ZK8(IDNOMN-1+1)=NONO2
              ZR(IDCOEF-1+1)=-1.D0

              DO 60,INO1=1,NNO1
                NUNO1=ZI(JCONU+IDECAL-1+INO1)
                COEF1=ZR(JCOCF+IDECAL-1+INO1)
                CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO1),NONO1)
                ZK8(IDNOMN+INO1)=NONO1
                ZR(IDCOEF+INO1)=COEF1
C               SI LA RELATION EST UNE TAUTOLOGIE, ON NE L'ECRIT PAS :
                IF (NUNO1.EQ.NUNO2) THEN
                  IF (ABS(ZR(IDCOEF+INO1)-1.D0).LT.1.D-2) THEN
                    CALL U2MESK('A','CALCULEL5_49',1,NONO1)
                    GOTO 130

                  ENDIF
                ENDIF
   60         CONTINUE

C           -- AFFECTATION DES RELATIONS CONCERNANT LE NOEUD INO2 :
C           -----------------------------------------------------
              IF (DNOR) THEN
                DO 80 INO1=1,NNO1+1
                  ZI(IDIMEN+INO1-1)=NDIM
                  ZK8(IDNOMD-1+INO1)='DEPL'
                  DO 70 IDIM=1,NDIM
                    ZR(IDIREC+(INO1-1)*NDIM1+IDIM-1)=ZR(JNORM+
     &                (ZI(INDIRE+INO2-1)-1)*NDIM+IDIM-1)
   70             CONTINUE
   80           CONTINUE
                CALL AFRELA(ZR(IDCOEF),CBID,ZK8(IDNOMD),ZK8(IDNOMN),
     &                      ZI(IDIMEN),ZR(IDIREC),NNO1+1,BETA,BETAC,
     &                      KBETA,TYPCOE,FONREE,TYPLAG,1.D-6,LISREL)
              ELSE

C               -- RELATIONS CONCERNANT LES TRANSLATIONS (DX/DY/DZ) :
C               -----------------------------------------------------
                IF (TYPRAC.NE.'MASSIF_COQUE') THEN
                  DO 100,K=1,NDIM
                    IF (K.EQ.1)CMP='DX'
                    IF (K.EQ.2)CMP='DY'
                    IF (K.EQ.3)CMP='DZ'
                    DO 90,INO1=1,NNO1+1
                      ZK8(IDNOMD-1+INO1)=CMP
   90               CONTINUE
                    CALL AFRELA(ZR(IDCOEF),CBID,ZK8(IDNOMD),ZK8(IDNOMN),
     &                          ZI(IDIMEN),ZR(IDIREC),NNO1+1,BETA,BETAC,
     &                          KBETA,TYPCOE,FONREE,TYPLAG,1.D-6,LISREL)
                    CALL IMPREL(MOTFAC,NNO1+1,ZR(IDCOEF),ZK8(IDNOMD),
     &                          ZK8(IDNOMN),BETA)
  100             CONTINUE

                ELSE IF (TYPRAC.EQ.'MASSIF_COQUE') THEN
                  CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
                  CALL CALIR5(NOMA,LISREL,NONO2,NUNO2,JCOOR,IDECAL,
     &                        JCONB,JCOCF,JCONU)
                ENDIF


C               -- RELATIONS CONCERNANT LES ROTATIONS (DRX/DRY/DRZ) :
C               -----------------------------------------------------
                IF (TYPRAC.EQ.'COQUE') THEN
                  DO 120,K=1,NDIM
                    IF (K.EQ.1)CMP='DRX'
                    IF (K.EQ.2)CMP='DRY'
                    IF (K.EQ.3)CMP='DRZ'
                    DO 110,INO1=1,NNO1+1
                      ZK8(IDNOMD-1+INO1)=CMP
  110               CONTINUE
                    CALL AFRELA(ZR(IDCOEF),CBID,ZK8(IDNOMD),ZK8(IDNOMN),
     &                          ZI(IDIMEN),ZR(IDIREC),NNO1+1,BETA,BETAC,
     &                          KBETA,TYPCOE,FONREE,TYPLAG,1.D-6,LISREL)
                    CALL IMPREL(MOTFAC,NNO1+1,ZR(IDCOEF),ZK8(IDNOMD),
     &                          ZK8(IDNOMN),BETA)
  120             CONTINUE

                ELSEIF (TYPRAC.EQ.'COQUE_MASSIF') THEN
                  CALL CALIR4(NOMA,LISREL,NONO2,INO2,
     &                        ZR(JLISV1+3*(INO2-1)),JCONB1,JCOCF1,
     &                        JCONU1,IDECA1,JCONB2,JCOCF2,JCONU2,IDECA2)
                ENDIF
              ENDIF
  130         CONTINUE
              IDECAL=IDECAL+NNO1
              IF (TYPRAC.EQ.'COQUE_MASSIF') THEN
                IDECA1=IDECA1+ZI(JCONB1-1+INO2)
                IDECA2=IDECA2+ZI(JCONB2-1+INO2)
              ENDIF
  140       CONTINUE


C       -- 3.1.2  S'IL Y A UNE ROTATION :
C       ---------------------------------
          ELSE
            IDECAL=0

            DO 260 INO2=1,NBNO2T

C ---       NNO1: NB DE NOEUD_MAIT LIES A INO2 :
C           ------------------------------------
              NNO1=ZI(JCONB-1+INO2)
              IF (NNO1.EQ.0)GOTO 260
              DO 160 K=1,IDMAX
                ZK8(IDNOMN+K-1)=M8BLAN
                ZK8(IDNOMD+K-1)=M8BLAN
                ZR(IDCOEF+K-1)=ZERO
                ZI(IDIMEN+K-1)=0
                DO 150 KK=1,3
                  ZR(IDIREC+3*(K-1)+KK-1)=ZERO
  150           CONTINUE
  160         CONTINUE

              NORMAL(1)=ZERO
              NORMAL(2)=ZERO
              NORMAL(3)=ZERO

              NUNO2=INO2
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO2),NONO2)

              IF (DNOR) THEN
                IJ=1
              ELSE
                IJ=NDIM
              ENDIF

              DO 170,INO1=1,NNO1
                NUNO1=ZI(JCONU+IDECAL-1+INO1)
                COEF1=ZR(JCOCF+IDECAL-1+INO1)
                CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO1),NONO1)
                ZK8(IDNOMN+IJ+INO1-1)=NONO1
                ZR(IDCOEF+IJ+INO1-1)=COEF1
  170         CONTINUE


C           -- AFFECTATION DES RELATIONS CONCERNANT LE NOEUD INO2 :
C           -----------------------------------------------------
              IF (DNOR) THEN
                DO 190 IDIM=1,NDIM
                  DO 180 JDIM=1,NDIM
                    NORMAL(IDIM)=NORMAL(IDIM)+
     &                           MROTA(JDIM,IDIM)*ZR(JNORM+(ZI(INDIRE+
     &                           INO2-1)-1)*NDIM+JDIM-1)
  180             CONTINUE
  190           CONTINUE
                ZR(IDCOEF+1-1)=1.0D0
                ZK8(IDNOMN+1-1)=NONO2
                ZK8(IDNOMD+1-1)='DEPL'
                ZI(IDIMEN+1-1)=NDIM
                DO 200 IDIM=1,NDIM
                  ZR(IDIREC+IDIM-1)=ZR(JNORM+(ZI(INDIRE+INO2-1)-1)*NDIM+
     &                              IDIM-1)
  200           CONTINUE
                DO 220 INO1=2,NNO1+1
                  ZI(IDIMEN+INO1-1)=NDIM
                  ZK8(IDNOMD-1+INO1)='DEPL'
                  DO 210 IDIM=1,NDIM
                    ZR(IDIREC+(INO1-1)*NDIM1+IDIM-1)=-NORMAL(IDIM)
  210             CONTINUE
  220           CONTINUE
                CALL AFRELA(ZR(IDCOEF),CBID,ZK8(IDNOMD),ZK8(IDNOMN),
     &                      ZI(IDIMEN),ZR(IDIREC),NNO1+1,BETA,BETAC,
     &                      KBETA,TYPCOE,FONREE,TYPLAG,1.D-6,LISREL)
              ELSE
                DO 250,K=1,NDIM
                  IF (K.EQ.1)CMP='DX'
                  IF (K.EQ.2)CMP='DY'
                  IF (K.EQ.3)CMP='DZ'
                  DO 230,INO1=1,NNO1
                    ZK8(IDNOMD+NDIM+INO1-1)=CMP
  230             CONTINUE
                  DO 240 KK=1,NDIM
                    IF (KK.EQ.1)CMP='DX'
                    IF (KK.EQ.2)CMP='DY'
                    IF (KK.EQ.3)CMP='DZ'
                    ZK8(IDNOMN+KK-1)=NONO2
                    ZK8(IDNOMD+KK-1)=CMP
                    ZR(IDCOEF+KK-1)=-MROTA(KK,K)
  240             CONTINUE
                  CALL AFRELA(ZR(IDCOEF),CBID,ZK8(IDNOMD),ZK8(IDNOMN),
     &                        ZI(IDIMEN),ZR(IDIREC),NNO1+NDIM,BETA,
     &                        BETAC,KBETA,TYPCOE,FONREE,TYPLAG,1.D-6,
     &                        LISREL)
                  CALL IMPREL(MOTFAC,NNO1+NDIM,ZR(IDCOEF),ZK8(IDNOMD),
     &                        ZK8(IDNOMN),BETA)
  250           CONTINUE
              ENDIF
              IDECAL=IDECAL+NNO1
  260       CONTINUE
          ENDIF


C       3.2 CAS "TEMP" :
C       =================
        ELSEIF (TYPLIA.EQ.'TEMP') THEN
          IDECAL=0
          DO 300 INO2=1,NBNO2T
C           NNO1: NB DE NOEUD_MAIT LIES A INO2
            NNO1=ZI(JCONB-1+INO2)
            IF (NNO1.EQ.0)GOTO 300

            NUNO2=INO2
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO2),NONO2)

            ZK8(IDNOMN-1+1)=NONO2
            ZR(IDCOEF-1+1)=-1.D0

            DO 270,INO1=1,NNO1
              NUNO1=ZI(JCONU+IDECAL-1+INO1)
              COEF1=ZR(JCOCF+IDECAL-1+INO1)
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUNO1),NONO1)
              ZK8(IDNOMN+INO1)=NONO1
              ZR(IDCOEF+INO1)=COEF1
C             SI LA RELATION EST UNE TAUTOLOGIE, ON NE L'ECRIT PAS :
              IF (NUNO1.EQ.NUNO2) THEN
                IF (ABS(ZR(IDCOEF+INO1)-1.D0).LT.1.D-2) THEN
                  CALL U2MESK('A','CALCULEL5_49',1,NONO1)
                  GOTO 290

                ENDIF
              ENDIF
  270       CONTINUE

C           -- AFFECTATION DE LA RELATION CONCERNANT LE NOEUD INO2 :
C           -----------------------------------------------------
            CMP='TEMP'
            DO 280,INO1=1,NNO1+1
              ZK8(IDNOMD-1+INO1)=CMP
  280       CONTINUE
            CALL AFRELA(ZR(IDCOEF),CBID,ZK8(IDNOMD),ZK8(IDNOMN),
     &                  ZI(IDIMEN),ZR(IDIREC),NNO1+1,BETA,BETAC,KBETA,
     &                  TYPCOE,FONREE,TYPLAG,1.D-6,LISREL)
            CALL IMPREL(MOTFAC,NNO1+1,ZR(IDCOEF),ZK8(IDNOMD),
     &                  ZK8(IDNOMN),BETA)

  290       CONTINUE
            IDECAL=IDECAL+NNO1
  300     CONTINUE
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

        CALL DETRSD('CORRESP_2_MAILLA',CORRES)
        CALL DETRSD('CORRESP_2_MAILLA',CORRE1)
        CALL DETRSD('CORRESP_2_MAILLA',CORRE2)
        CALL JEDETR(GEOM2)
        CALL JEDETR('&&CALIRC.LIMANU1')
        CALL JEDETR('&&CALIRC.LIMANU2')
        CALL JEDETR('&&CALIRC.LINONU2')
        CALL JEDETR('&&CALIRC.LISTK')
        CALL JEDETR('&&CALIRC.LISV1')
        CALL JEDETR('&&CALIRC.INDIRE')
        CALL JEDETR('&&NBNLMA.LN')
        CALL JEDETR('&&NBNLMA.NBN')
        CALL JEDETR('&&CANORT.NORMALE')

  310 CONTINUE

      CALL JEDETR('&&CALIRC.NOMNOE')
      CALL JEDETR('&&CALIRC.NOMDDL')
      CALL JEDETR('&&CALIRC.COEF')
      CALL JEDETR('&&CALIRC.DIRECT')
      CALL JEDETR('&&CALIRC.DIMENSION')
      CALL JEDETR('&&CALIRC.ELIM')

C --- AFFECTATION DE LA LISTE DE RELATIONS A LA CHARGE :
C     ------------------------------------------------
      CALL AFLRCH(LISREL,CHARGE)

  320 CONTINUE
      CALL JEDEMA()
      END
