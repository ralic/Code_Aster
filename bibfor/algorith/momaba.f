      SUBROUTINE MOMABA(MAILLA)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      CHARACTER*8 MAILLA
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
C TOLE CRP_20
C
      INTEGER JTYMA,NBMC,NBMA,JNUMA,I,J,ITYP,JCOOR,N1,N2,I1,I2,
     &        NBNO,NBMAT,JPOIN,IER,IFM,NIV,JCON,JCONM,NDIM,NN,JNBMA,
     &        NCOUNT,JDIM
      LOGICAL LNMF,LMODI
      PARAMETER(NBMC=2)
      CHARACTER*8 K8B,TYPE
      CHARACTER*16 TYMOCL(NBMC),MOTCLE(NBMC)
      CHARACTER*24 CONNEX,NOMMAI,NOMNOE,NOMJV
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL INFNIV(IFM,NIV)
C
      CONNEX=MAILLA//'.CONNEX'
      NOMMAI=MAILLA//'.NOMMAI'
      NOMNOE=MAILLA//'.NOMNOE'
      CALL JEVEUO(MAILLA//'.TYPMAIL        ','L',JTYMA)
      CALL JEVEUO(MAILLA//'.COORDO    .VALE','E',JCOOR)
      CALL JEVEUO(MAILLA//'.DIME           ','L',JDIM)
      CALL DISMOI('F','NB_MA_MAILLA',MAILLA,'MAILLAGE',NBMAT,K8B,IER)
      LMODI=.FALSE.
C     ------------------------------------------------------------------
C
C --- LECTURE DE LA LISTE DE MAILLES
C
      MOTCLE(1)='GROUP_MA_FOND'
      TYMOCL(1)='GROUP_MA'
      MOTCLE(2)='MAILLE_FOND'
      TYMOCL(2)='MAILLE'
      NOMJV='&&MOMABA.LISTE_MA'
      CALL RELIEM(' ',MAILLA,'NU_MAILLE','MODI_MAILLE',1,NBMC,MOTCLE,
     &            TYMOCL,NOMJV,NBMA)

      IF (NBMA.EQ.0)GOTO 150

      CALL JEVEUO(NOMJV,'L',JNUMA)

C --- TRAITEMENT DES MAILLES
C
C     ON INTERDIT 'GROUP_MA_FOND' ET 'MAILLE_FOND'
C     SI LE MAILLAGE EST DE DIMENSION 2.
      IF (ZI(JDIM+5).EQ.2) CALL U2MESS('F','ALGORITH6_16')

      DO 140 I=1,NBMA
        ITYP=JTYMA-1+ZI(JNUMA+I-1)
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)

        IF (NIV.EQ.2) THEN
          CALL JENUNO(JEXNUM(NOMMAI,ZI(JNUMA+I-1)),K8B)
          WRITE (IFM,*)'TRAITEMENT DE LA MAILLE ',K8B
        ENDIF
        IF (TYPE(1:4).EQ.'SEG3') THEN
          CALL JEVEUO(JEXNUM(CONNEX,ZI(JNUMA+I-1)),'L',JPOIN)
          N1=ZI(JPOIN)
          N2=ZI(JPOIN+1)
        ELSEIF (TYPE(1:4).EQ.'POI1') THEN
          CALL JEVEUO(JEXNUM(CONNEX,ZI(JNUMA+I-1)),'L',JPOIN)
          N1=ZI(JPOIN)
          N2=0
        ELSE
          CALL U2MESS('F','ALGORITH6_17')
        ENDIF

        DO 130 J=1,NBMAT
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JTYMA-1+J)),TYPE)
          CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
C
C --------- TRIA6 , TRIA7
          IF (TYPE.EQ.'TRIA6' .OR. TYPE.EQ.'TRIA7') THEN
            NBNO=3
            DO 20 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                IF (N2.EQ.0) THEN
                  LMODI=.TRUE.
                  CALL BARTRI(I1,N2,ZR(JCOOR),ZI(JPOIN))
                  IF (NIV.EQ.2) THEN
                    CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                    WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                  ENDIF
                  GOTO 130

                ELSE
                  DO 10 I2=1,NBNO
                    IF (ZI(JPOIN+I2-1).EQ.N2) THEN
                      LMODI=.TRUE.
                      CALL BARTRI(I1,I2,ZR(JCOOR),ZI(JPOIN))
                      IF (NIV.EQ.2) THEN
                        CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                        WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                      ENDIF
                      GOTO 130

                    ENDIF
   10             CONTINUE
                ENDIF
              ENDIF
   20       CONTINUE
C
C --------- QUAD8 , QUAD9
          ELSEIF (TYPE.EQ.'QUAD8' .OR. TYPE.EQ.'QUAD9') THEN
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            NBNO=4
            DO 40 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                IF (N2.EQ.0) THEN
                  LMODI=.TRUE.
                  CALL BARQUA(I1,N2,ZR(JCOOR),ZI(JPOIN))
                  IF (NIV.EQ.2) THEN
                    CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                    WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                  ENDIF
                  GOTO 130

                ELSE
                  DO 30 I2=1,NBNO
                    IF (ZI(JPOIN+I2-1).EQ.N2) THEN
                      LMODI=.TRUE.
                      CALL BARQUA(I1,I2,ZR(JCOOR),ZI(JPOIN))
                      IF (NIV.EQ.2) THEN
                        CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                        WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                      ENDIF
                      GOTO 130

                    ENDIF
   30             CONTINUE
                ENDIF
              ENDIF
   40       CONTINUE
C
C --------- TETRA10
          ELSEIF (TYPE.EQ.'TETRA10') THEN
            NBNO=4
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            DO 60 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                IF (N2.EQ.0) THEN
                  LMODI=.TRUE.
                  CALL BARTET(I1,N2,ZR(JCOOR),ZI(JPOIN))
                  IF (NIV.EQ.2) THEN
                    CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                    WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                  ENDIF
                  GOTO 130

                ELSE
                  DO 50 I2=1,NBNO
                    IF (ZI(JPOIN+I2-1).EQ.N2) THEN
                      LMODI=.TRUE.
                      CALL BARTET(I1,I2,ZR(JCOOR),ZI(JPOIN))
                      IF (NIV.EQ.2) THEN
                        CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                        WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                      ENDIF
                      GOTO 130

                    ENDIF
   50             CONTINUE
                ENDIF
              ENDIF
   60       CONTINUE
C
C --------- PENTA15 , PENTA18
          ELSEIF (TYPE.EQ.'PENTA15' .OR. TYPE.EQ.'PENTA18') THEN
            NBNO=6
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            DO 80 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                IF (N2.EQ.0) THEN
                  LMODI=.TRUE.
                  CALL BARPEN(I1,N2,ZR(JCOOR),ZI(JPOIN))
                  IF (NIV.EQ.2) THEN
                    CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                    WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                  ENDIF
                  GOTO 130

                ELSE
                  DO 70 I2=1,NBNO
                    IF (ZI(JPOIN+I2-1).EQ.N2) THEN
                      LMODI=.TRUE.
                      CALL BARPEN(I1,I2,ZR(JCOOR),ZI(JPOIN))
                      IF (NIV.EQ.2) THEN
                        CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                        WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                      ENDIF
                      GOTO 130

                    ENDIF
   70             CONTINUE
                ENDIF
              ENDIF
   80       CONTINUE
C
C --------- PYRAM13
          ELSEIF (TYPE.EQ.'PYRAM13') THEN
            NBNO=5
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            DO 100 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                IF (N2.EQ.0) THEN
                  LMODI=.TRUE.
                  CALL BARPYR(I1,N2,ZR(JCOOR),ZI(JPOIN))
                  IF (NIV.EQ.2) THEN
                    CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                    WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                  ENDIF
                  GOTO 130

                ELSE
                  DO 90 I2=1,NBNO
                    IF (ZI(JPOIN+I2-1).EQ.N2) THEN
                      LMODI=.TRUE.
                      CALL BARPYR(I1,I2,ZR(JCOOR),ZI(JPOIN))
                      IF (NIV.EQ.2) THEN
                        CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                        WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                      ENDIF
                      GOTO 130

                    ENDIF
   90             CONTINUE
                ENDIF
              ENDIF
  100       CONTINUE
C
C --------- HEXA20 , HEXA27
          ELSEIF (TYPE.EQ.'HEXA20' .OR. TYPE.EQ.'HEXA27') THEN
            NBNO=8
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            DO 120 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                IF (N2.EQ.0) THEN
                  LMODI=.TRUE.
                  CALL BARHEX(I1,N2,ZR(JCOOR),ZI(JPOIN))
                  IF (NIV.EQ.2) THEN
                    CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                    WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                  ENDIF
                  GOTO 130

                ELSE
                  DO 110 I2=1,NBNO
                    IF (ZI(JPOIN+I2-1).EQ.N2) THEN
                      LMODI=.TRUE.
                      CALL BARHEX(I1,I2,ZR(JCOOR),ZI(JPOIN))
                      IF (NIV.EQ.2) THEN
                        CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                        WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                      ENDIF
                      GOTO 130

                    ENDIF
  110             CONTINUE
                ENDIF
              ENDIF
  120       CONTINUE
C
          ELSE
C
          ENDIF
C
  130   CONTINUE
C
  140 CONTINUE
C
      CALL JEDETR(NOMJV)
      CALL JEDETR('&&MOMABA_MAILLE')
C
  150 CONTINUE
C     ------------------------------------------------------------------

C --- LECTURE DE LA LISTE DE NOEUDS

      CALL JEVEUO(MAILLA//'.COORDO    .VALE','L',JCONM)
      CALL JELIRA(MAILLA//'.COORDO    .VALE','LONMAX',NDIM,K8B)
C
C     ON STOCKE LES COORDONNEES DES NOEUDS DU FOND DE FISSURE AVANT
C     LEURS MODIFICATIONS
      CALL WKVECT('&&COORD_NOEUDS','V V R',NDIM,JCON)
      DO 160 I=1,NDIM
        ZR(JCON+I-1)=ZR(JCONM+I-1)
  160 CONTINUE

      MOTCLE(1)='GROUP_NO_FOND'
      TYMOCL(1)='GROUP_NO'
      MOTCLE(2)='NOEUD_FOND'
      TYMOCL(2)='NOEUD'
      NOMJV='&&MOMABA.LISTE_NO'
      CALL RELIEM(' ',MAILLA,'NU_NOEUD','MODI_MAILLE',1,NBMC,MOTCLE,
     &            TYMOCL,NOMJV,NBMA)
      IF (NBMA.EQ.0)GOTO 260

C     ON VERIFIE L'UNICITE DU NOEUD DU FOND DE FISSURE POUR UN
C     MAILLAGE DE DIMENSION 2
      IF (ZI(JDIM+5).EQ.2 .AND. NBMA.GT.1) CALL U2MESS('F',
     &    'ALGORITH6_18')
C
      CALL JEVEUO(NOMJV,'L',JNUMA)
      CALL WKVECT('&&NOEU_MIL_FISS','V V I',NBMA,JNBMA)
C
C --- TRAITEMENT DES NOEUDS
C
      NCOUNT=0
      DO 240 I=1,NBMA
        N1=ZI(JNUMA+I-1)
        N2=0
        IF (NIV.EQ.2) THEN
          CALL JENUNO(JEXNUM(NOMNOE,ZI(JNUMA+I-1)),K8B)
          WRITE (IFM,*)'TRAITEMENT DU NOEUD ',K8B
        ENDIF
        LNMF=.TRUE.
        DO 230 J=1,NBMAT
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JTYMA-1+J)),TYPE)
          CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
C
C --------- TRIA6 , TRIA7
          IF (TYPE.EQ.'TRIA6' .OR. TYPE.EQ.'TRIA7') THEN
            NBNO=3
            DO 170 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                LNMF=.FALSE.
                LMODI=.TRUE.
                CALL BARTRI(I1,N2,ZR(JCOOR),ZI(JPOIN))
                IF (NIV.EQ.2) THEN
                  CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                  WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                ENDIF
                GOTO 230

              ENDIF
  170       CONTINUE
C
C --------- QUAD8 , QUAD9
          ELSEIF (TYPE.EQ.'QUAD8' .OR. TYPE.EQ.'QUAD9') THEN
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            NBNO=4
            DO 180 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                LNMF=.FALSE.
                LMODI=.TRUE.
                CALL BARQUA(I1,N2,ZR(JCOOR),ZI(JPOIN))
                IF (NIV.EQ.2) THEN
                  CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                  WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                ENDIF
                GOTO 230

              ENDIF
  180       CONTINUE
C
C --------- TETRA10
          ELSEIF (TYPE.EQ.'TETRA10') THEN
            NBNO=4
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            DO 190 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                LNMF=.FALSE.
                LMODI=.TRUE.
                CALL BARTET(I1,N2,ZR(JCOOR),ZI(JPOIN))
                IF (NIV.EQ.2) THEN
                  CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                  WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                ENDIF
                GOTO 230

              ENDIF
  190       CONTINUE
C
C --------- PENTA15
          ELSEIF (TYPE.EQ.'PENTA15') THEN
            NBNO=6
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            DO 200 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                LNMF=.FALSE.
                LMODI=.TRUE.
                CALL BARPEN(I1,N2,ZR(JCOOR),ZI(JPOIN))
                IF (NIV.EQ.2) THEN
                  CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                  WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                ENDIF
                GOTO 230

              ENDIF
  200       CONTINUE
C
C --------- PYRAM13
          ELSEIF (TYPE.EQ.'PYRAM13') THEN
            NBNO=5
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            DO 210 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                LNMF=.FALSE.
                LMODI=.TRUE.
                CALL BARPYR(I1,N2,ZR(JCOOR),ZI(JPOIN))
                IF (NIV.EQ.2) THEN
                  CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                  WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                ENDIF
                GOTO 230

              ENDIF
  210       CONTINUE
C
C --------- HEXA20 , HEXA27
          ELSEIF (TYPE.EQ.'HEXA20' .OR. TYPE.EQ.'HEXA27') THEN
            NBNO=8
            CALL JEVEUO(JEXNUM(CONNEX,J),'L',JPOIN)
            DO 220 I1=1,NBNO
              IF (ZI(JPOIN+I1-1).EQ.N1) THEN
                LNMF=.FALSE.
                LMODI=.TRUE.
                CALL BARHEX(I1,N2,ZR(JCOOR),ZI(JPOIN))
                IF (NIV.EQ.2) THEN
                  CALL JENUNO(JEXNUM(NOMMAI,J),K8B)
                  WRITE (IFM,*)'   MAILLE MODIFIEE ',K8B
                ENDIF
                GOTO 230

              ENDIF
  220       CONTINUE
C
          ELSE
C
          ENDIF
C
  230   CONTINUE
C
        IF (LNMF) THEN
C         ON STOCKE LES NOEUDS MILIEU DU FOND DE FISSURE
          ZI(JNBMA+NCOUNT)=N1
          NCOUNT=NCOUNT+1
        ENDIF

  240 CONTINUE

      DO 250 I=1,NCOUNT
C       ON REAJUSTE LES COORDONNEES DES NOEUDS MILIEU
C       DU FOND DE FISSURE
        NN=3*(ZI(JNBMA+I-1)-1)
        ZR(JCOOR+NN)=ZR(JCON+NN)
        ZR(JCOOR+NN+1)=ZR(JCON+NN+1)
        ZR(JCOOR+NN+2)=ZR(JCON+NN+2)
  250 CONTINUE

      IF (.NOT.LMODI) CALL U2MESS('F','ALGORITH16_72')

      CALL JEDETR('&&NOEU_MIL_FISS')
      CALL JEDETR('&&COORD_NOEUDS')
      CALL JEDETR(NOMJV)
      CALL JEDETR('&&MOMABA_MAILLE')
C
  260 CONTINUE
C
      CALL JEDEMA()
      END
