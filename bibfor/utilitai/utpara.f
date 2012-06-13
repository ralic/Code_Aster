      SUBROUTINE UTPARA (BAS1,NOMSD,TYPSD,NBORDR )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)       NOMSD,   TYPSD
      CHARACTER*1 BAS1
      INTEGER                                            NBORDR
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C ----------------------------------------------------------------------
      CHARACTER*8  CH8,TYPE,ACCES,KBID
      CHARACTER*5  SUFFIX
      CHARACTER*19 NOMS2
      INTEGER NBPAMX,NBPARA
      PARAMETER (NBPAMX=100)
      CHARACTER*32 PARA,LIPARA(NBPAMX)
      CHARACTER*16 NOPARA
      CHARACTER*8 LIACCE(NBPAMX), LITYPE(NBPAMX)
      INTEGER I, JTAVA,ICO,JPARA,IUNDEF,ISNNEM,I1
      INTEGER NBR, NBI, NBC, NBK8, NBK16, NBK24, NBK32, NBK80, N1,N2
      REAL*8 R8VIDE, RUNDEF
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMS2  = NOMSD
      RUNDEF = R8VIDE()
      IUNDEF = ISNNEM()


C     -- RECUPERATION DE LA LISTE DES PARAMETRES :
C     --------------------------------------------
      CALL UTPAR1(TYPSD,NBPAMX,LIPARA,NBPARA)


C     -- CREATION DE .NOVA ET .TAVA :
C     --------------------------------------------
      CALL JECREO(NOMS2//'.NOVA',BAS1//' N K16')
      CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBPARA,' ')
      DO 2 I=1,NBPARA
        PARA=LIPARA(I)
        I1= INDEX(PARA,'#')
        CALL ASSERT(I1.GE.2)
        NOPARA=PARA(1:I1-1)
        CALL JECROC(JEXNOM(NOMS2//'.NOVA',NOPARA))
  2   CONTINUE

      CALL JECREC(NOMS2//'.TAVA',BAS1//' V K8','NU','CONTIG',
     &            'CONSTANT',NBPARA)
      CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')


C     -- CALCUL DE NBR, NBC, NBI, NBK8, ... :
C     -- CALCUL DE LIACCE et LITYPE :
C     ----------------------------------------
      NBR=0
      NBC=0
      NBI=0
      NBK8=0
      NBK16=0
      NBK24=0
      NBK32=0
      NBK80=0
      DO 1 I = 1 , NBPARA
        PARA=LIPARA(I)
        I1= INDEX(PARA,'#')
        CALL ASSERT(PARA(I1:I1+2).EQ.'#A#'.OR.PARA(I1:I1+2).EQ.'#P#')
        TYPE=PARA(I1+3:32)
        LITYPE(I)=TYPE
        ACCES=PARA(I1+1:I1+1)
        IF (ACCES.EQ.'A') THEN
          LIACCE(I)='ACCES'
        ELSE
          LIACCE(I)='PARA'
        ENDIF
        IF (TYPE.EQ.'R') THEN
          NBR=NBR+1
        ELSEIF (TYPE.EQ.'C') THEN
          NBC=NBC+1
        ELSEIF (TYPE.EQ.'I') THEN
          NBI=NBI+1
        ELSEIF (TYPE.EQ.'K8') THEN
          NBK8=NBK8+1
        ELSEIF (TYPE.EQ.'K16') THEN
          NBK16=NBK16+1
        ELSEIF (TYPE.EQ.'K24') THEN
          NBK24=NBK24+1
        ELSEIF (TYPE.EQ.'K32') THEN
          NBK32=NBK32+1
        ELSEIF (TYPE.EQ.'K80') THEN
          NBK80=NBK80+1
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
 1    CONTINUE


C     -- PARAMETRES REELS :
C     ---------------------
      IF (NBR.GT.0) THEN
        SUFFIX='.RSPR'
        N1=NBR
        N2=N1*NBORDR
        CALL WKVECT(NOMS2//SUFFIX,BAS1//' V R',N2,JPARA)
        CALL JEECRA(NOMS2//SUFFIX,'LONUTI',0,KBID)
        DO 10 I = 1,N2
           ZR(JPARA+I-1) = RUNDEF
 10   CONTINUE

        CALL CODENT( N1 , 'G' , CH8 )
        ICO=0
        DO 11 I = 1 , NBPARA
           PARA=LIPARA(I)
           I1= INDEX(PARA,'#')
           TYPE=LITYPE(I)
           ACCES=LIACCE(I)
           IF (TYPE.EQ.'R') THEN
             ICO=ICO+1
             NOPARA= PARA(1:I1-1)
             CALL JENONU(JEXNOM(NOMS2//'.NOVA',NOPARA),I1)
             CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',I1),'E',JTAVA)
             ZK8(JTAVA-1+1)   = SUFFIX
             CALL CODENT(ICO,'G',ZK8(JTAVA-1+2) )
             ZK8(JTAVA-1+3) = CH8
             ZK8(JTAVA-1+4) = ACCES
           ENDIF
 11     CONTINUE
      ENDIF


C     -- PARAMETRES COMPLEXES :
C     -------------------------
      IF (NBC.GT.0) THEN
        SUFFIX='.RSPC'
        N1=NBC
        N2=N1*NBORDR
        CALL WKVECT(NOMS2//SUFFIX,BAS1//' V C',N2,JPARA)
        CALL JEECRA(NOMS2//SUFFIX,'LONUTI',0,KBID)
        DO 20 I = 1,N2
           ZC(JPARA+I-1) = DCMPLX(RUNDEF,RUNDEF)
 20     CONTINUE

        CALL CODENT( N1 , 'G' , CH8 )
        ICO=0
        DO 21 I = 1 , NBPARA
           PARA=LIPARA(I)
           I1= INDEX(PARA,'#')
           TYPE=LITYPE(I)
           ACCES=LIACCE(I)
           ACCES=PARA(I1+1:I1+1)
           IF (TYPE.EQ.'C') THEN
             ICO=ICO+1
             NOPARA= PARA(1:I1-1)
             CALL JENONU(JEXNOM(NOMS2//'.NOVA',NOPARA),I1)
             CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',I1),'E',JTAVA)
             ZK8(JTAVA-1+1)   = SUFFIX
             CALL CODENT(ICO,'G',ZK8(JTAVA-1+2) )
             ZK8(JTAVA-1+3) = CH8
             ZK8(JTAVA-1+4) = ACCES
           ENDIF
 21     CONTINUE
      ENDIF


C     -- PARAMETRES ENTIERS :
C     ---------------------
      IF (NBI.GT.0) THEN
        SUFFIX='.RSPI'
        N1=NBI
        N2=N1*NBORDR
        CALL WKVECT(NOMS2//SUFFIX,BAS1//' V I',N2,JPARA)
        CALL JEECRA(NOMS2//SUFFIX,'LONUTI',0,KBID)
        DO 30 I = 1,N2
           ZI(JPARA+I-1) = IUNDEF
 30     CONTINUE

        CALL CODENT( N1 , 'G' , CH8 )
        ICO=0
        DO 31 I = 1 , NBPARA
           PARA=LIPARA(I)
           I1= INDEX(PARA,'#')
           ACCES=PARA(I1+1:I1+1)
           TYPE=LITYPE(I)
           ACCES=LIACCE(I)
           IF (TYPE.EQ.'I') THEN
             ICO=ICO+1
             NOPARA= PARA(1:I1-1)
             CALL JENONU(JEXNOM(NOMS2//'.NOVA',NOPARA),I1)
             CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',I1),'E',JTAVA)
             ZK8(JTAVA-1+1)   = SUFFIX
             CALL CODENT(ICO,'G',ZK8(JTAVA-1+2) )
             ZK8(JTAVA-1+3) = CH8
             ZK8(JTAVA-1+4) = ACCES
           ENDIF
 31     CONTINUE
      ENDIF


C     -- PARAMETRES K8 :
C     ---------------------
      IF (NBK8.GT.0) THEN
        SUFFIX='.RSP8'
        N1=NBK8
        N2=N1*NBORDR
        CALL WKVECT(NOMS2//SUFFIX,BAS1//' V K8',N2,JPARA)
        CALL JEECRA(NOMS2//SUFFIX,'LONUTI',0,KBID)

        CALL CODENT( N1 , 'G' , CH8 )
        ICO=0
        DO 41 I = 1 , NBPARA
           PARA=LIPARA(I)
           I1= INDEX(PARA,'#')
           ACCES=PARA(I1+1:I1+1)
           TYPE=LITYPE(I)
           ACCES=LIACCE(I)
           IF (TYPE.EQ.'K8') THEN
             ICO=ICO+1
             NOPARA= PARA(1:I1-1)
             CALL JENONU(JEXNOM(NOMS2//'.NOVA',NOPARA),I1)
             CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',I1),'E',JTAVA)
             ZK8(JTAVA-1+1)   = SUFFIX
             CALL CODENT(ICO,'G',ZK8(JTAVA-1+2) )
             ZK8(JTAVA-1+3) = CH8
             ZK8(JTAVA-1+4) = ACCES
           ENDIF
 41     CONTINUE
      ENDIF


C     -- PARAMETRES K16 :
C     ---------------------
      IF (NBK16.GT.0) THEN
        SUFFIX='.RS16'
        N1=NBK16
        N2=N1*NBORDR
        CALL WKVECT(NOMS2//SUFFIX,BAS1//' V K16',N2,JPARA)
        CALL JEECRA(NOMS2//SUFFIX,'LONUTI',0,KBID)

        CALL CODENT( N1 , 'G' , CH8 )
        ICO=0
        DO 51 I = 1 , NBPARA
           PARA=LIPARA(I)
           I1= INDEX(PARA,'#')
           ACCES=PARA(I1+1:I1+1)
           TYPE=LITYPE(I)
           ACCES=LIACCE(I)
           IF (TYPE.EQ.'K16') THEN
             ICO=ICO+1
             NOPARA= PARA(1:I1-1)
             CALL JENONU(JEXNOM(NOMS2//'.NOVA',NOPARA),I1)
             CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',I1),'E',JTAVA)
             ZK8(JTAVA-1+1)   = SUFFIX
             CALL CODENT(ICO,'G',ZK8(JTAVA-1+2) )
             ZK8(JTAVA-1+3) = CH8
             ZK8(JTAVA-1+4) = ACCES
           ENDIF
 51     CONTINUE
      ENDIF


C     -- PARAMETRES K24 :
C     ---------------------
      IF (NBK24.GT.0) THEN
        SUFFIX='.RS24'
        N1=NBK24
        N2=N1*NBORDR
        CALL WKVECT(NOMS2//SUFFIX,BAS1//' V K24',N2,JPARA)
        CALL JEECRA(NOMS2//SUFFIX,'LONUTI',0,KBID)

        CALL CODENT( N1 , 'G' , CH8 )
        ICO=0
        DO 61 I = 1 , NBPARA
           PARA=LIPARA(I)
           I1= INDEX(PARA,'#')
           ACCES=PARA(I1+1:I1+1)
           TYPE=LITYPE(I)
           ACCES=LIACCE(I)
           IF (TYPE.EQ.'K24') THEN
             ICO=ICO+1
             NOPARA= PARA(1:I1-1)
             CALL JENONU(JEXNOM(NOMS2//'.NOVA',NOPARA),I1)
             CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',I1),'E',JTAVA)
             ZK8(JTAVA-1+1)   = SUFFIX
             CALL CODENT(ICO,'G',ZK8(JTAVA-1+2) )
             ZK8(JTAVA-1+3) = CH8
             ZK8(JTAVA-1+4) = ACCES
           ENDIF
 61     CONTINUE
      ENDIF


C     -- PARAMETRES K32 :
C     ---------------------
      IF (NBK32.GT.0) THEN
        SUFFIX='.RS32'
        N1=NBK32
        N2=N1*NBORDR
        CALL WKVECT(NOMS2//SUFFIX,BAS1//' V K32',N2,JPARA)
        CALL JEECRA(NOMS2//SUFFIX,'LONUTI',0,KBID)

        CALL CODENT( N1 , 'G' , CH8 )
        ICO=0
        DO 71 I = 1 , NBPARA
           PARA=LIPARA(I)
           I1= INDEX(PARA,'#')
           ACCES=PARA(I1+1:I1+1)
           TYPE=LITYPE(I)
           ACCES=LIACCE(I)
           IF (TYPE.EQ.'K32') THEN
             ICO=ICO+1
             NOPARA= PARA(1:I1-1)
             CALL JENONU(JEXNOM(NOMS2//'.NOVA',NOPARA),I1)
             CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',I1),'E',JTAVA)
             ZK8(JTAVA-1+1)   = SUFFIX
             CALL CODENT(ICO,'G',ZK8(JTAVA-1+2) )
             ZK8(JTAVA-1+3) = CH8
             ZK8(JTAVA-1+4) = ACCES
           ENDIF
 71     CONTINUE
      ENDIF


C     -- PARAMETRES K80 :
C     ---------------------
      IF (NBK80.GT.0) THEN
        SUFFIX='.RS80'
        N1=NBK80
        N2=N1*NBORDR
        CALL WKVECT(NOMS2//SUFFIX,BAS1//' V K80',N2,JPARA)
        CALL JEECRA(NOMS2//SUFFIX,'LONUTI',0,KBID)

        CALL CODENT( N1 , 'G' , CH8 )
        ICO=0
        DO 81 I = 1 , NBPARA
           PARA=LIPARA(I)
           I1= INDEX(PARA,'#')
           ACCES=PARA(I1+1:I1+1)
           TYPE=LITYPE(I)
           ACCES=LIACCE(I)
           IF (TYPE.EQ.'K80') THEN
             ICO=ICO+1
             NOPARA= PARA(1:I1-1)
             CALL JENONU(JEXNOM(NOMS2//'.NOVA',NOPARA),I1)
             CALL JEVEUO(JEXNUM(NOMS2//'.TAVA',I1),'E',JTAVA)
             ZK8(JTAVA-1+1)   = SUFFIX
             CALL CODENT(ICO,'G',ZK8(JTAVA-1+2) )
             ZK8(JTAVA-1+3) = CH8
             ZK8(JTAVA-1+4) = ACCES
           ENDIF
 81     CONTINUE
      ENDIF


      CALL JEDEMA()
      END
