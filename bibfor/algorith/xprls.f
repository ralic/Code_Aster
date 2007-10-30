      SUBROUTINE XPRLS(MODEL,NOMA,CNSLN,CNSLT,GRLN,GRLT,CNSVT,CNSVN,
     &                 DELTAT)
      IMPLICIT NONE
      REAL*8         DELTAT
      CHARACTER*8    MODEL,NOMA
      CHARACTER*19   CNSLN,CNSLT,GRLN,GRLT,CNSVT,CNSVN

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2007   AUTEUR PELLET J.PELLET 
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
C     ------------------------------------------------------------------
C
C       XPRLS   : X-FEM PROPAGATION DES LEVEL SETS
C       -----     -     --              -     -
C    PROPAGATION DES LEVEL SETS AU PAS DE TEMP SUIVANT
C
C    ENTREE
C        MODEL   : NOM DU CONCEPT MODELE
C        NOMA    : NOM DU CONCEPT MAILLAGE
C        CNSLT   : CHAM_NO_S LEVEL SET TANGENTIELLE
C        CNSLN   : CHAM_NO_S LEVEL SET NORMALE
C        GRLT    : CHAM_NO_S GRADIENT DE LEVEL SET TANGENTIELLE
C        GRLN    : CHAM_NO_S GRADIENT DE LEVEL SET NORMALE
C        CNSVT   : CHAM_NO_S VITESSE TANGENTIELLE DE PROPAGATION
C        CNSVN   : CHAM_NO_S VITESSE NORMALE DE PROPAGATION
C        DELTAT  : PAS DE TEMPS
C
C    SORTIE
C        CNSLT   : CHAM_NO_S LEVEL SET TANGENTIELLE
C        CNSLN   : CHAM_NO_S LEVEL SET NORMALE
C        GRLT    : CHAM_NO_S GRADIENT DE LEVEL SET TANGENTIELLE
C        GRLN    : CHAM_NO_S GRADIENT DE LEVEL SET NORMALE
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

      INTEGER    I,IFM,NIV,NBNO,IRET,JLTNO,JLNNO,JGRTNO,JGRNNO
      INTEGER    JVTNO,JVNNO,JLTI,JLTIL,JLNI,JLNIL,JGRTI,JGRNI,IBID
      CHARACTER*8      K8B,LPAIN(2),LPAOUT(1)
      CHARACTER*19     CNSLTI,CNSLNI,CNSGTI,CNSGNI,CHGRTI,CHGRNI,CHAMSI,
     &                 CHGRLT,CHGRLN,CHAMS,CNOLTI,CNOLNI,CNOLT,CNOLN
      CHARACTER*24     OBJMA,LCHIN(2),LCHOUT(1),LIGRMO
      REAL*8           NORMGN,NORMGT,NORGNI,NORGTI

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

C  RECUPERATION DE CARACTERISTIQUES DU MAILLAGE
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IRET)

      WRITE(IFM,*)'   DELTA_T = ',DELTAT

C   RECUPERATION DE L'ADRESSE DES VALEURS DE LT, LN ET LEURS GRADIENTS
      CALL JEVEUO(CNSLT//'.CNSV','E',JLTNO)
      CALL JEVEUO(CNSLN//'.CNSV','E',JLNNO)
      CALL JEVEUO(GRLT//'.CNSV','E',JGRTNO)
      CALL JEVEUO(GRLN//'.CNSV','E',JGRNNO)

C   RECUPERATION DES ADRESSES DES CHAMPS DE VITESSE AUX NOEUDS
      CALL JEVEUO(CNSVT//'.CNSV','L',JVTNO)
      CALL JEVEUO(CNSVN//'.CNSV','L',JVNNO)

C     CREATION DES OBJETS VOLATILES
      CNSLTI = '&&XPRLS.CNSLTI'
      CNSLNI = '&&XPRLS.CNSLNI'
      CNOLTI = '&&XPRLS.CNOLTI'
      CNOLNI = '&&XPRLS.CNOLNI'
      CHGRTI = '&&XPRLS.CHGRTI'
      CHGRNI = '&&XPRLS.CHGRNI'
      CNSGTI = '&&XPRLS.CNSGTI'
      CNSGNI = '&&XPRLS.CNSGNI'
      CHGRLT = '&&XPRLS.CHGRLT'
      CHGRLN = '&&XPRLS.CHGRLN'
      CHAMS  = '&&XPRLS.CHAMS'
      CNOLT  = '&&XPRLS.CNOLT'
      CNOLN  = '&&XPRLS.CNOLN'


C-----------------------------------------------------------------------
C     CALCUL DES LEVEL SETS INTERMEDIAIRES
C-----------------------------------------------------------------------

      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSLTI)
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSLNI)

      CALL JEVEUO(CNSLTI//'.CNSV','E',JLTI)
      CALL JEVEUO(CNSLTI//'.CNSL','E',JLTIL)
      CALL JEVEUO(CNSLNI//'.CNSV','E',JLNI)
      CALL JEVEUO(CNSLNI//'.CNSL','E',JLNIL)

      DO 100 I=1,NBNO
      NORMGT = ( ZR(JGRTNO-1+3*(I-1)+1)**2.D0 +
     &           ZR(JGRTNO-1+3*(I-1)+2)**2.D0 +
     &           ZR(JGRTNO-1+3*(I-1)+3)**2.D0 )**.5D0
      NORMGN = ( ZR(JGRNNO-1+3*(I-1)+1)**2.D0 +
     &           ZR(JGRNNO-1+3*(I-1)+2)**2.D0 +
     &           ZR(JGRNNO-1+3*(I-1)+3)**2.D0 )**.5D0
         ZL(JLTIL-1+I)=.TRUE.
         ZL(JLNIL-1+I)=.TRUE.
         IF ( (NORMGN.EQ.(0.D0)) .AND. (NORMGT.EQ.(0.D0)) ) THEN
            ZR(JLTI-1+I)=0.D0
            ZR(JLNI-1+I)=0.D0
         ELSE
            ZR(JLTI-1+I)=ZR(JLTNO-1+I)-DELTAT*ZR(JVTNO-1+I)*NORMGT
            ZR(JLNI-1+I)=ZR(JLNNO-1+I)-DELTAT*ZR(JVNNO-1+I)*NORMGN
         ENDIF
 100  CONTINUE

C-----------------------------------------------------------------------
C     CALCUL DES GRADIENTS DES LEVEL SETS INTERMEDIAIRES
C-----------------------------------------------------------------------

C  GRADIENT DE LTI
      CALL CNSCNO(CNSLTI,' ','NON','V',CNOLTI,'F',IBID)

      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = NOMA//'.COORDO'
      LPAIN(2) = 'PNEUTER'
      LCHIN(2) = CNOLTI
      LPAOUT(1)= 'PGNEUTR'
      LCHOUT(1)= CHGRTI
      LIGRMO = MODEL//'.MODELE'

      CALL CALCUL('S','GRAD_NEUT_R',LIGRMO,2,LCHIN,LPAIN,1,
     &            LCHOUT,LPAOUT,'V')

      CALL CELCES ( CHGRTI, 'V', CHAMS )
      CALL CESCNS ( CHAMS, ' ', 'V', CNSGTI )
      CALL JEVEUO ( CNSGTI//'.CNSV','L',JGRTI)

C  GRADIENT DE LNI
      CALL CNSCNO(CNSLNI,' ','NON','V',CNOLNI,'F',IBID)

      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = NOMA//'.COORDO'
      LPAIN(2) = 'PNEUTER'
      LCHIN(2) = CNOLNI
      LPAOUT(1)= 'PGNEUTR'
      LCHOUT(1)= CHGRNI
      LIGRMO = MODEL//'.MODELE'

      CALL CALCUL('S','GRAD_NEUT_R',LIGRMO,2,LCHIN,LPAIN,1,
     &            LCHOUT,LPAOUT,'V')

      CALL CELCES ( CHGRNI, 'V', CHAMS )
      CALL CESCNS ( CHAMS, ' ', 'V', CNSGNI )
      CALL JEVEUO ( CNSGNI//'.CNSV','L',JGRNI)

C-----------------------------------------------------------------------
C     CALCUL DES LEVEL SETS RESULTANTES
C-----------------------------------------------------------------------

      DO 200 I=1,NBNO
         NORGTI = ( ZR(JGRTI-1+3*(I-1)+1)**2.D0 +
     &              ZR(JGRTI-1+3*(I-1)+2)**2.D0 +
     &              ZR(JGRTI-1+3*(I-1)+3)**2.D0 )**.5D0
         NORGNI = ( ZR(JGRNI-1+3*(I-1)+1)**2.D0 +
     &              ZR(JGRNI-1+3*(I-1)+2)**2.D0 +
     &              ZR(JGRNI-1+3*(I-1)+3)**2.D0 )**.5D0
         IF ( (NORGTI.EQ.(0.D0)) .AND. (NORGNI.EQ.(0.D0)) ) THEN
            ZR(JLTNO-1+I) = 0.D0
            ZR(JLNNO-1+I) = 0.D0
         ELSE
            ZR(JLTNO-1+I) = (ZR(JLTNO-1+I)+ZR(JLTI-1+I))/2.D0
     &                      - DELTAT/2.D0*ZR(JVTNO-1+I)*NORGTI
            ZR(JLNNO-1+I) = (ZR(JLNNO-1+I)+ZR(JLNI-1+I))/2.D0
     &                      - DELTAT/2.D0*ZR(JVNNO-1+I)*NORGNI
         ENDIF
 200  CONTINUE

C-----------------------------------------------------------------------
C     CALCUL DES GRADIENTS DES LEVEL SETS RESULTANTES
C-----------------------------------------------------------------------

C  GRADIENT DE LT
      CALL CNSCNO(CNSLT,' ','NON','V',CNOLT,'F',IBID)

      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = NOMA//'.COORDO'
      LPAIN(2) = 'PNEUTER'
      LCHIN(2) = CNOLT
      LPAOUT(1)= 'PGNEUTR'
      LCHOUT(1)= CHGRLT
      LIGRMO = MODEL//'.MODELE'

      CALL CALCUL('S','GRAD_NEUT_R',LIGRMO,2,LCHIN,LPAIN,1,
     &            LCHOUT,LPAOUT,'V')

      CALL CELCES ( CHGRLT, 'V', CHAMS )
      CALL CESCNS ( CHAMS, ' ', 'V', GRLT )

C  GRADIENT DE LN
      CALL CNSCNO(CNSLN,' ','NON','V',CNOLN,'F',IBID)

      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = NOMA//'.COORDO'
      LPAIN(2) = 'PNEUTER'
      LCHIN(2) = CNOLN
      LPAOUT(1)= 'PGNEUTR'
      LCHOUT(1)= CHGRLN
      LIGRMO = MODEL//'.MODELE'

      CALL CALCUL('S','GRAD_NEUT_R',LIGRMO,2,LCHIN,LPAIN,1,
     &            LCHOUT,LPAOUT,'V')

      CALL CELCES ( CHGRLN, 'V', CHAMS )
      CALL CESCNS ( CHAMS, ' ', 'V', GRLN )

C  DESTRUCTION DES OBJETS VOLATILES
      CALL JEDETR(CNSLTI)
      CALL JEDETR(CNSLNI)
      CALL JEDETR(CNOLTI)
      CALL JEDETR(CNOLNI)
      CALL JEDETR(CHGRTI)
      CALL JEDETR(CHGRNI)
      CALL JEDETR(CNSGTI)
      CALL JEDETR(CNSGNI)
      CALL JEDETR(CHGRLT)
      CALL JEDETR(CHGRLN)
      CALL JEDETR(CHAMS)
      CALL JEDETR(CNOLT)
      CALL JEDETR(CNOLN)

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
