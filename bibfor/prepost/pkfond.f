      SUBROUTINE PKFOND ( FOND )
      IMPLICIT   NONE
      CHARACTER*8         FOND
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 06/05/2003   AUTEUR CIBHHPD D.NUNEZ 
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
C TOLE CRP_20

C     OPERATEUR POST_K1_K2_K3 : TRAITEMENT DU MOT CLE "FOND_3D"

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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      INTEGER      NDIM, IBID, N1, NBPAR1, IDCOOR, NBNOE, IM, IN,
     +             NBVAL, JABSCS, JDXS, JDYS, JDZS, JABSCI, JDXI, JDYI,
     +             JDZI, IRET, NBMA, IADRMA, JLIMA, IDLINO, II,
     +             NBNOLI, NBNOFO, INF, NUMORI, NBVAS, NBVAI, NBNOFT,
     +             JNOLS, JNOLI, NBNOLS, NBTRLS, NBTRLI, JCOORI, JCOORS,
     +             KNOLS, KNOLI, JNOFO, JINTS, JINTI, NUTYP, IPAS,
     +             NUMEXT, KNULS, KNULI, IFM, NIV, IATYMA, NTTRI6,
     +             NTQUA8, NTQUA9, JTGOR, JTGEX, NO1, NO2, NO3,
     +             NBINST, JINST, IIN, JNFT,IERA
      PARAMETER  ( NBPAR1=12 )
      REAL*8       R8B, COEFD, COEFD3, COEFG, COEFG3, X0(3), D1, D2,
     +             D, RMAX, EPSI, VECNOR(3), X1, X2, Y1, Y2, Z1, Z2,
     +             KG2(10), KG1(10), VECTY(3), VO(3), VE(3), RMAXEM,
     +             DMAX, ABSC, VP(3), TGOR(3), TGEX(3), DINST,
     +             PRECI, PREC, PRECV, PRECN
      COMPLEX*16   CBID
      LOGICAL      EXTGOR, EXTGEX, EXIST
      CHARACTER*2  TYPPA1(NBPAR1)
      CHARACTER*8  K8B, NOMRES, NOMA, CRITI, CRITN
      CHARACTER*16 NOMCMD, CONCEP,NOMPA1(NBPAR1), MOTFAC
      CHARACTER*19 DEPSU2, DEPIN2
      CHARACTER*24 NOESUP, ABSSUP, DXSUP, DYSUP, DZSUP, NOEINF, ABSINF,
     +             DEPINF, DXINF, DYINF, DZINF, NUMSUP, NUMINF, MESNOE,
     +             FONNOE, FONLSU, FONLIN, FONTOR, FONTEX, NOMNOE,
     +             DEPSUP

      DATA  NOMPA1 / 'INST' , 'NOEUD_FOND' , 'ABSC_CURV',
     +               'METHODE' , 'K1_MAX' , 'K1_MIN' , 'K2_MAX' ,
     +              'K2_MIN' , 'K3_MAX' , 'K3_MIN' , 'G_MAX' , 'G_MIN' /
      DATA  TYPPA1 / 'R', 'K8', 'R', 'I', 'R', 'R', 'R', 'R', 'R',
     +               'R', 'R', 'R' /
C DEB ------------------------------------------------------------------
      CALL JEMARQ ( )
      CRITN = 'RELATIF'
      CALL INFNIV ( IFM , NIV )

      CALL GETRES ( NOMRES , CONCEP , NOMCMD )

      FONNOE = FOND//'.FOND      .NOEU'
      CALL JEEXIN ( FONNOE, IRET )
      IF (IRET.EQ.0) CALL UTMESS('F',NOMCMD,'BUG: MANQUE .NOEU')
      CALL JELIRA ( FONNOE, 'LONMAX', NBNOFO, K8B )
      CALL JEVEUO ( FONNOE, 'L', JNOFO )

      FONLSU = FOND//'.LEVRESUP  .MAIL'
      CALL JEEXIN ( FONLSU, IRET )
      IF (IRET.EQ.0) CALL UTMESS('F',NOMCMD,'BUG: MANQUE .LEV_SUP')

      FONLIN = FOND//'.LEVREINF  .MAIL'
      CALL JEEXIN ( FONLIN, IRET )
      IF (IRET.EQ.0) CALL UTMESS('F',NOMCMD,'BUG: MANQUE .LEV_INF')

      FONTOR = FOND//'.DTAN_ORIGINE'
      CALL JEEXIN ( FONTOR, IRET )
      IF (IRET.EQ.0) THEN
         EXTGOR = .FALSE.
      ELSE
         EXTGOR = .TRUE.
         CALL JEVEUO ( FONTOR, 'L', JTGOR )
         TGOR(1) = ZR(JTGOR)
         TGOR(2) = ZR(JTGOR+1)
         TGOR(3) = ZR(JTGOR+2)
      ENDIF

      FONTEX = FOND//'.DTAN_EXTREMITE'
      CALL JEEXIN ( FONTEX, IRET )
      IF (IRET.EQ.0) THEN
         EXTGEX = .FALSE.
      ELSE
         EXTGEX = .TRUE.
         CALL JEVEUO ( FONTEX, 'L', JTGEX )
         TGEX(1) = ZR(JTGEX)
         TGEX(2) = ZR(JTGEX+1)
         TGEX(3) = ZR(JTGEX+2)
      ENDIF

C     ------------------------------------------------------------------
C                     CHOIX DE ABSC_CURV_MAXI
C     ------------------------------------------------------------------

      RMAX = RMAXEM()
      CALL GETVR8 ( ' ', 'ABSC_CURV_MAXI', 1,1,1, RMAX, N1 )

C     ------------------------------------------------------------------
C            LA PRECISION POUR DETERMINER LES NOEUDS DU PLAN
C     ------------------------------------------------------------------

      CALL GETVR8 ( ' ', 'PREC_VIS_A_VIS', 1,1,1, PRECV, N1 )

C     ------------------------------------------------------------------
C                        CHOIX DU REPERE
C     ------------------------------------------------------------------

      CALL GETVR8 ( ' ', 'VECT_K1', 1,1,3, VECTY, N1 )

C     ------------------------------------------------------------------
C                     CARACTERISTIQUES MATERIAUX
C     ------------------------------------------------------------------

      CALL PKMATE ( NDIM, COEFD, COEFD3, COEFG, COEFG3 )
      IF ( NDIM .NE. 3 ) THEN
         CALL UTMESS('F',NOMCMD,'LA MODELISATION DOIT ETRE "3D"')
      ENDIF

C     ------------------------------------------------------------------
C                        LE MAILLAGE
C     ------------------------------------------------------------------

      CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, NOMA , N1 )

      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', IDCOOR )
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOE,K8B,IRET)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA6') , NTTRI6 )
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD8') , NTQUA8 )
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD9') , NTQUA9 )
      CALL JEVEUO ( NOMA//'.TYPMAIL', 'L', IATYMA )
      NOMNOE = NOMA//'.NOMNOE'

C     ------------------------------------------------------------------
C             DEPLACEMENT DE LA LEVRE SUPERIEURE ET INFERIEURE
C     ------------------------------------------------------------------

      CALL PKDEPL ( NOMA, FOND, DEPSUP, DEPINF )

C     ------------------------------------------------------------------
C                   LES NOEUDS DE POST-TRAITEMENT
C     ------------------------------------------------------------------
      MESNOE = '&&PKFOND.MES_NOEUDS'
      CALL PKNOEU ( NOMA, ZK8(JNOFO), NBNOFO, MESNOE )
      CALL JEVEUO ( MESNOE, 'L', JNFT )

C     ------------------------------------------------------------------
C                             LA TABLE
C     ------------------------------------------------------------------
      CALL TBCRSD ( NOMRES, 'G' )
      CALL TBAJPA ( NOMRES, NBPAR1, NOMPA1, TYPPA1 )

C     ------------------------------------------------------------------
C --- VECTEURS DE TRAVAIL

      CALL WKVECT ( '&&PKFOND_LIST_NOEUD'  , 'V V I', NBNOE, IDLINO )
      CALL WKVECT ( '&&PKFOND_NOEU_LEV_SUP', 'V V I', NBNOE, JNOLS  )
      CALL WKVECT ( '&&PKFOND_NOEU_LEV_INF', 'V V I', NBNOE, JNOLI  )

C --- GROUP_MA LEVRE_SUP --> GROUP_NO LEVRE_SUP

      CALL JELIRA ( FONLSU, 'LONMAX', NBMA, K8B )
      CALL JEVEUO ( FONLSU, 'L', IADRMA )
      CALL WKVECT ( '&&PKFOND_MAILLE_LEV_SUP', 'V V I', NBMA, JLIMA )
      DO 10 IM = 1 , NBMA
         CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(IADRMA+IM-1)),
     +                                                 ZI(JLIMA+IM-1) )
 10   CONTINUE
      CALL GMGNRE ( NOMA, NBNOE, ZI(IDLINO), ZI(JLIMA), NBMA,
     +                           ZI(JNOLS), NBNOLS, 'TOUS' )
      CALL WKVECT ('&&PKFOND_COOR_LEV_SUP', 'V V R', 3*NBNOLS, JCOORS )
      DO 12 IN = 1 , NBNOLS
         ZR(JCOORS-1+3*(IN-1)+1) = ZR(IDCOOR-1+3*(ZI(JNOLS+IN-1)-1)+1)
         ZR(JCOORS-1+3*(IN-1)+2) = ZR(IDCOOR-1+3*(ZI(JNOLS+IN-1)-1)+2)
         ZR(JCOORS-1+3*(IN-1)+3) = ZR(IDCOOR-1+3*(ZI(JNOLS+IN-1)-1)+3)
 12   CONTINUE

C --- GROUP_MA LEVRE_INF --> GROUP_NO LEVRE_INF

      CALL JELIRA ( FONLIN, 'LONMAX', NBMA, K8B )
      CALL JEVEUO ( FONLIN, 'L', IADRMA )
      CALL WKVECT ( '&&PKFOND_MAILLE_LEV_INF', 'V V I', NBMA, JLIMA )
      DO 20 IM = 1 , NBMA
         CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(IADRMA+IM-1)),
     +                                                 ZI(JLIMA+IM-1) )
 20   CONTINUE
      CALL GMGNRE ( NOMA, NBNOE, ZI(IDLINO), ZI(JLIMA), NBMA,
     +                           ZI(JNOLI), NBNOLI, 'TOUS' )
      CALL WKVECT ('&&PKFOND_COOR_LEV_INF', 'V V R', 3*NBNOLI, JCOORI )
      DO 22 IN = 1 , NBNOLI
         ZR(JCOORI-1+3*(IN-1)+1) = ZR(IDCOOR-1+3*(ZI(JNOLI+IN-1)-1)+1)
         ZR(JCOORI-1+3*(IN-1)+2) = ZR(IDCOOR-1+3*(ZI(JNOLI+IN-1)-1)+2)
         ZR(JCOORI-1+3*(IN-1)+3) = ZR(IDCOOR-1+3*(ZI(JNOLI+IN-1)-1)+3)
 22   CONTINUE

      IPAS = 1
      NUTYP = ZI(IATYMA-1+ZI(JLIMA))
      IF ( NUTYP .EQ. NTTRI6 ) THEN
         IPAS = 2
      ELSEIF ( NUTYP .EQ. NTQUA8 ) THEN
         IPAS = 2
      ELSEIF ( NUTYP .EQ. NTQUA9 ) THEN
         IPAS = 2
      END IF
C ---------------------------------------------------------------------
      DEPSU2 = '&&PKFOND.DEPL_SUP'
      DEPIN2 = '&&PKFOND.DEPL_INF'

      NOESUP = '&&PKFOND.ABSC_NOEUD_SUP'
      NUMSUP = '&&PKFOND.ABSC_NUME_SUP'
      ABSSUP = '&&PKFOND.ABSC_CURV_SUP'
      DXSUP  = '&&PKFOND.DX_SUP'
      DYSUP  = '&&PKFOND.DY_SUP'
      DZSUP  = '&&PKFOND.DZ_SUP'

      NOEINF = '&&PKFOND.ABSC_NOEUD_INF'
      NUMINF = '&&PKFOND.ABSC_NUME_INF'
      ABSINF = '&&PKFOND.ABSC_CURV_INF'
      DXINF  = '&&PKFOND.DX_INF'
      DYINF  = '&&PKFOND.DY_INF'
      DZINF  = '&&PKFOND.DZ_INF'

C     ------------------------------------------------------------------
C                   LES INSTANTS DE POST-TRAITEMENT
C     ------------------------------------------------------------------
      CALL TBEXIP ( DEPSUP, 'INST', EXIST, K8B )
      IF ( EXIST ) THEN
         MOTFAC = ' '
         CALL TBUTNU ( MOTFAC, 1, '&&PKFOND.INSTANT', NBINST, DEPSUP,
     +                                                 PRECI, CRITI )
         CALL JEVEUO ( '&&PKFOND.INSTANT', 'L', JINST )
      ELSE
         NBINST = 1
      ENDIF

C     ------------------------------------------------------------------
      IF ( EXTGOR ) THEN
         NBNOFT = NBNOFO
      ELSE
C ------ CAS DU FOND_FERME: LE PREMIER ET LE DERNIER NOEUD SONT
C                           IDENTIQUES
         NBNOFT = NBNOFO - 1
      ENDIF

C     --- BOUCLE SUR LES INSTANTS ---

      DO 100 IIN = 1 , NBINST
         IF ( EXIST ) THEN
            DINST = ZR(JINST+IIN-1)
            CALL TBEXTB ( DEPSUP, 'V', DEPSU2, 1, 'INST', 'EQ',
     +                    IBID, DINST, CBID, K8B, PRECI, CRITI )

            CALL TBEXTB ( DEPINF, 'V', DEPIN2, 1, 'INST', 'EQ',
     +                    IBID, DINST, CBID, K8B, PRECI, CRITI )
         ELSE
            DINST = 0.D0
            DEPSU2 = DEPSUP
            DEPIN2 = DEPINF
         ENDIF
         IF ( NIV .EQ. 2 )  WRITE(IFM,1100) DINST

C --- BOUCLE SUR LES NOEUDS DU FOND DE FISSURE :
C     ----------------------------------------

      DO 200 INF = 1 , NBNOFT, IPAS
         IERA = 0
         IF ( .NOT. ZL(JNFT+INF-1) ) GOTO 200
         IF ( NIV .EQ. 2 )  WRITE(IFM,1000) ZK8(JNOFO+INF-1)
         CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), NUMORI )

C ------ DETERMINATION DU PLAN PASSANT PAR UN NOEUD N_I DU FOND DE
C        FISSURE ET UN VECTEUR NORMAL

         X0(1) =  ZR(IDCOOR-1+3*(NUMORI-1)+1)
         X0(2) =  ZR(IDCOOR-1+3*(NUMORI-1)+2)
         X0(3) =  ZR(IDCOOR-1+3*(NUMORI-1)+3)

         IF ( INF .EQ. 1 ) THEN
            IF ( EXTGOR ) THEN
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), IN )
               X1 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF)), IN )
               X2 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y2 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z2 = ZR(IDCOOR-1+3*(IN-1)+3)
               D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               VECNOR(1) = (X2-X1) / D
               VECNOR(2) = (Y2-Y1) / D
               VECNOR(3) = (Z2-Z1) / D
               CALL PROVEC ( VECNOR, TGOR, VP )
               CALL PROVEC ( VP, TGOR, VECNOR )
            ELSE
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+NBNOFO-2)), NO1 )
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), NO2 )
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF  )), NO3 )
               CALL PKFON1 ( ZR(IDCOOR), VECNOR, NO1, NO2, NO3 )
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+NBNOFO-2)), IN )
               X1 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), IN )
               X2 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y2 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z2 = ZR(IDCOOR-1+3*(IN-1)+3)
               D1 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF)), IN )
               X1 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
               D2 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               D  = MIN(D1,D2)
            ENDIF
         ELSEIF ( INF .EQ. NBNOFO ) THEN
            IF ( EXTGEX ) THEN
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-2)), IN )
               X1 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), IN )
               X2 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y2 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z2 = ZR(IDCOOR-1+3*(IN-1)+3)
               D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               VECNOR(1) = (X2-X1) / D
               VECNOR(2) = (Y2-Y1) / D
               VECNOR(3) = (Z2-Z1) / D
               CALL PROVEC ( VECNOR, TGEX, VP )
               CALL PROVEC ( VP, TGEX, VECNOR )
            ELSE
            CALL UTMESS('F','PKFOND','ON NE DOIT PAS ETRE LA, BUG !!!')
            ENDIF
         ELSE
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-2)), NO1 )
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), NO2 )
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF  )), NO3 )
            CALL PKFON1 ( ZR(IDCOOR), VECNOR, NO1, NO2, NO3 )
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-2)), IN )
            X1 = ZR(IDCOOR-1+3*(IN-1)+1)
            Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
            Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), IN )
            X2 = ZR(IDCOOR-1+3*(IN-1)+1)
            Y2 = ZR(IDCOOR-1+3*(IN-1)+2)
            Z2 = ZR(IDCOOR-1+3*(IN-1)+3)
            D1 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF)), IN )
            X1 = ZR(IDCOOR-1+3*(IN-1)+1)
            Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
            Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
            D2 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
            D  = MIN(D1,D2)
         ENDIF

         PREC = D * PRECV

C ------ CALCUL DE L'INTERSECTION DU PLAN AVEC LES NOEUDS DES MAILLES
C        DEFINISSANT LA LEVRE SUPERIEURE

         CALL WKVECT ( '&&PKFOND_INTERS_SUP', 'V V I', NBNOE, JINTS )

         CALL CGNOP0 ( NBNOLS, ZR(JCOORS), X0, VECNOR, PREC, NBTRLS,
     +                 ZI(JINTS))
         DO 30 IN = 1 , NBTRLS
            ZI(JINTS+IN-1) = ZI(JNOLS+ZI(JINTS+IN-1)-1)
 30      CONTINUE

C ------ CALCUL DE L'INTERSECTION DU PLAN AVEC LES NOEUDS DES MAILLES
C        DEFINISSANT LA LEVRE INFERIEURE

         CALL WKVECT ( '&&PKFOND_INTERS_INF', 'V V I', NBNOE, JINTI )

         CALL CGNOP0 ( NBNOLI, ZR(JCOORI), X0, VECNOR, PREC, NBTRLI,
     +                 ZI(JINTI))
         DO 32 IN = 1 , NBTRLI
            ZI(JINTI+IN-1) = ZI(JNOLI+ZI(JINTI+IN-1)-1)
 32      CONTINUE

         PREC = 10.D0 * PREC

C ------ NOEUDS TROUVES ET ABSCISSES CURVILIGNES

         IF ( NBTRLS .LT. 3 ) THEN
            CALL UTDEBM('A',NOMCMD,'IL MANQUE DES POINTS DANS LE PLAN'//
     +    ' DEFINI PAR LA LEVRE SUPERIEURE ET PERPENDICULAIRE AU NOEUD')
            CALL UTIMPK('S',' DU FOND DE FISSURE ',1,ZK8(JNOFO+INF-1))
            CALL UTIMPK('L',' AUGMENTER ',1,'PREC_VIS_A_VIS')
            CALL UTFINM
            GOTO 202
         ELSE
            NBVAS = 1
            DMAX  = 0.D0
            NUMEXT = NUMORI
            X1 = ZR(IDCOOR-1+3*(NUMORI-1)+1)
            Y1 = ZR(IDCOOR-1+3*(NUMORI-1)+2)
            Z1 = ZR(IDCOOR-1+3*(NUMORI-1)+3)
            CALL WKVECT ( NUMSUP , 'V V I' , NBTRLS, KNULS )
            ZI(KNULS) = NUMORI
            DO 110 IN = 1 , NBTRLS
              IF ( ZI(JINTS+IN-1) .EQ. NUMORI ) GOTO 110
               X2 = ZR(IDCOOR-1+3*(ZI(JINTS+IN-1)-1)+1)
               Y2 = ZR(IDCOOR-1+3*(ZI(JINTS+IN-1)-1)+2)
               Z2 = ZR(IDCOOR-1+3*(ZI(JINTS+IN-1)-1)+3)
               D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               IF ( D .LE. RMAX ) THEN
                  DO 112 II = 1 , NBVAS
                     IF ( ZI(KNULS+II-1) .EQ. ZI(JINTS+IN-1) ) GOTO 110
 112              CONTINUE
                  NBVAS = NBVAS + 1
                  ZI(KNULS+NBVAS-1)  = ZI(JINTS+IN-1)
                  IF ( D .GT. DMAX ) THEN
                     DMAX = D
                     NUMEXT = ZI(JINTS+IN-1)
                  ENDIF
               ENDIF
 110        CONTINUE
         ENDIF
         CALL OREINO ( NOMA, ZI(KNULS), NBVAS, NUMORI, NUMEXT,
     +                       ZR(IDCOOR), CRITN, PREC,IERA, IRET )
         IF ( IRET .NE. 0 ) GOTO 202

         CALL WKVECT ( NOESUP , 'V V K8', NBVAS, KNOLS  )
         CALL WKVECT ( ABSSUP , 'V V R8', NBVAS, JABSCS )
         CALL JENUNO(JEXNUM(NOMNOE,ZI(KNULS)), ZK8(KNOLS) )
         ZR(JABSCS) = 0.0D0
         X1 = ZR(IDCOOR-1+3*(ZI(KNULS)-1)+1)
         Y1 = ZR(IDCOOR-1+3*(ZI(KNULS)-1)+2)
         Z1 = ZR(IDCOOR-1+3*(ZI(KNULS)-1)+3)
         IF ( NIV .EQ. 2 ) WRITE(IFM,1010) ZK8(KNOLS), ZR(JABSCS)
         DO 114 IN = 2 , NBVAS
            X2 = ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+1)
            Y2 = ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+2)
            Z2 = ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+3)
          ZR(JABSCS+IN-1) = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
            IF ( NIV .EQ. 2 ) THEN
              CALL JENUNO(JEXNUM(NOMNOE,ZI(KNULS+IN-1)),ZK8(KNOLS+IN-1))
              WRITE(IFM,1010) ZK8(KNOLS+IN-1), ZR(JABSCS+IN-1)
            ENDIF
 114     CONTINUE

         IF ( NBTRLI .LT. 3 ) THEN
            CALL UTDEBM('A',NOMCMD,'IL MANQUE DES POINTS DANS LE PLAN'//
     +    ' DEFINI PAR LA LEVRE INFERIEURE ET PERPENDICULAIRE AU NOEUD')
            CALL UTIMPK('S',' DU FOND DE FISSURE ',1,ZK8(JNOFO+INF-1))
            CALL UTIMPK('L',' AUGMENTER ',1,'PREC_VIS_A_VIS')
            CALL UTFINM
            GOTO 202
         ELSE
            NBVAI = 1
            DMAX  = 0.D0
            NUMEXT = NUMORI
            X1 = ZR(IDCOOR-1+3*(NUMORI-1)+1)
            Y1 = ZR(IDCOOR-1+3*(NUMORI-1)+2)
            Z1 = ZR(IDCOOR-1+3*(NUMORI-1)+3)
            CALL WKVECT ( NUMINF , 'V V I' , NBTRLI, KNULI )
            ZI(KNULI) = NUMORI
            DO 120 IN = 1 , NBTRLI
              IF ( ZI(JINTI+IN-1) .EQ. NUMORI ) GOTO 120
               X2 = ZR(IDCOOR-1+3*(ZI(JINTI+IN-1)-1)+1)
               Y2 = ZR(IDCOOR-1+3*(ZI(JINTI+IN-1)-1)+2)
               Z2 = ZR(IDCOOR-1+3*(ZI(JINTI+IN-1)-1)+3)
               D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               IF ( D .LE. RMAX ) THEN
                  DO 122 II = 1 , NBVAI
                     IF ( ZI(KNULI+II-1) .EQ. ZI(JINTI+IN-1) ) GOTO 120
 122              CONTINUE
                  NBVAI = NBVAI + 1
                  ZI(KNULI+NBVAI-1)  = ZI(JINTI+IN-1)
                  IF ( D .GT. DMAX ) THEN
                     DMAX = D
                     NUMEXT = ZI(JINTI+IN-1)
                  ENDIF
               ENDIF
 120        CONTINUE
         ENDIF
         CALL OREINO ( NOMA, ZI(KNULI), NBVAI, NUMORI, NUMEXT,
     +                       ZR(IDCOOR), CRITN, PREC,IERA, IRET )
         IF ( IRET .NE. 0 ) GOTO 202

         CALL WKVECT ( NOEINF , 'V V K8', NBVAI, KNOLI  )
         CALL WKVECT ( ABSINF , 'V V R8', NBVAI, JABSCI )
         CALL JENUNO(JEXNUM(NOMNOE,ZI(KNULI)), ZK8(KNOLI) )
         ZR(JABSCI) = 0.0D0
         X1 = ZR(IDCOOR-1+3*(ZI(KNULI)-1)+1)
         Y1 = ZR(IDCOOR-1+3*(ZI(KNULI)-1)+2)
         Z1 = ZR(IDCOOR-1+3*(ZI(KNULI)-1)+3)
         IF ( NIV .EQ. 2 ) WRITE(IFM,1020) ZK8(KNOLI), ZR(JABSCI)
         DO 124 IN = 2 , NBVAI
            X2 = ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+1)
            Y2 = ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+2)
            Z2 = ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+3)
          ZR(JABSCI+IN-1) = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
            IF ( NIV .EQ. 2 ) THEN
              CALL JENUNO(JEXNUM(NOMNOE,ZI(KNULI+IN-1)),ZK8(KNOLI+IN-1))
              WRITE(IFM,1020) ZK8(KNOLI+IN-1), ZR(JABSCI+IN-1)
            ENDIF
 124     CONTINUE

         IF ( NBVAS .NE. NBVAI ) THEN
            CALL UTDEBM('A',NOMCMD,'DIFFERENCE DE POINTS ENTRE LA '//
     +          ' LEVRE INFERIEURE ET SUPERIEURE POUR TRAITER LE NOEUD')
            CALL UTIMPK('S',' DU FOND DE FISSURE ',1,ZK8(JNOFO+INF-1))
            CALL UTIMPI('L',' NOMBRE DE POINTS SUPERIEURE ',1,NBVAS)
            CALL UTIMPI('L',' NOMBRE DE POINTS INFERIEURE ',1,NBVAI)
            CALL UTFINM
            GOTO 202
         ELSE
            NBVAL = NBVAS
         ENDIF

C ------ EXTRAIRE DANS LA TABLE LES DEPLACEMENTS AUX NOEUDS

         CALL WKVECT ( DXSUP , 'V V R8', NBVAL, JDXS )
         CALL WKVECT ( DYSUP , 'V V R8', NBVAL, JDYS )
         CALL WKVECT ( DZSUP , 'V V R8', NBVAL, JDZS )
         CALL WKVECT ( DXINF , 'V V R8', NBVAL, JDXI )
         CALL WKVECT ( DYINF , 'V V R8', NBVAL, JDYI )
         CALL WKVECT ( DZINF , 'V V R8', NBVAL, JDZI )
         DO 130 IN = 1 , NBVAL
            CALL TBLIVA ( DEPSU2, 1, 'NOEUD', IBID, R8B, CBID,
     +                    ZK8(KNOLS+IN-1), 'RELATIF', EPSI, 'DX', K8B,
     +                    IBID, ZR(JDXS+IN-1), CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               CALL UTDEBM('A',NOMCMD,'PROBLEME POUR RECUPERER ')
               CALL UTIMPK('S','LA COMPOSANTE ',1,'DX')
               CALL UTIMPK('S','POUR LE NOEUD ',1,ZK8(KNOLS+IN-1))
               CALL UTIMPK('S',' DANS LA TABLE ',1,DEPSUP)
               CALL UTFINM
               GOTO 202
            ENDIF
            CALL TBLIVA ( DEPSU2, 1, 'NOEUD', IBID, R8B, CBID,
     +                    ZK8(KNOLS+IN-1), 'RELATIF', EPSI, 'DY', K8B,
     +                    IBID, ZR(JDYS+IN-1), CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               CALL UTDEBM('A',NOMCMD,'PROBLEME POUR RECUPERER ')
               CALL UTIMPK('S','LA COMPOSANTE ',1,'DY')
               CALL UTIMPK('S','POUR LE NOEUD ',1,ZK8(KNOLS+IN-1))
               CALL UTIMPK('S',' DANS LA TABLE ',1,DEPSUP)
               CALL UTFINM
               GOTO 202
            ENDIF
            CALL TBLIVA ( DEPSU2, 1, 'NOEUD', IBID, R8B, CBID,
     +                    ZK8(KNOLS+IN-1), 'RELATIF', EPSI, 'DZ', K8B,
     +                    IBID, ZR(JDZS+IN-1), CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               CALL UTDEBM('A',NOMCMD,'PROBLEME POUR RECUPERER ')
               CALL UTIMPK('S','LA COMPOSANTE ',1,'DZ')
               CALL UTIMPK('S','POUR LE NOEUD ',1,ZK8(KNOLS+IN-1))
               CALL UTIMPK('S',' DANS LA TABLE ',1,DEPSUP)
               CALL UTFINM
               GOTO 202
            ENDIF
            CALL TBLIVA ( DEPIN2, 1, 'NOEUD', IBID, R8B, CBID,
     +                    ZK8(KNOLI+IN-1), 'RELATIF', EPSI, 'DX', K8B,
     +                    IBID, ZR(JDXI+IN-1), CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               CALL UTDEBM('A',NOMCMD,'PROBLEME POUR RECUPERER ')
               CALL UTIMPK('S','LA COMPOSANTE ',1,'DX')
               CALL UTIMPK('S','POUR LE NOEUD ',1,ZK8(KNOLI+IN-1))
               CALL UTIMPK('S',' DANS LA TABLE ',1,DEPINF)
               CALL UTFINM
               GOTO 202
            ENDIF
            CALL TBLIVA ( DEPIN2, 1, 'NOEUD', IBID, R8B, CBID,
     +                    ZK8(KNOLI+IN-1), 'RELATIF', EPSI, 'DY', K8B,
     +                    IBID, ZR(JDYI+IN-1), CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               CALL UTDEBM('A',NOMCMD,'PROBLEME POUR RECUPERER ')
               CALL UTIMPK('S','LA COMPOSANTE ',1,'DY')
               CALL UTIMPK('S','POUR LE NOEUD ',1,ZK8(KNOLI+IN-1))
               CALL UTIMPK('S',' DANS LA TABLE ',1,DEPINF)
               CALL UTFINM
               GOTO 202
            ENDIF
            CALL TBLIVA ( DEPIN2, 1, 'NOEUD', IBID, R8B, CBID,
     +                    ZK8(KNOLI+IN-1), 'RELATIF', EPSI, 'DZ', K8B,
     +                    IBID, ZR(JDZI+IN-1), CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               CALL UTDEBM('A',NOMCMD,'PROBLEME POUR RECUPERER ')
               CALL UTIMPK('S','LA COMPOSANTE ',1,'DZ')
               CALL UTIMPK('S','POUR LE NOEUD ',1,ZK8(KNOLI+IN-1))
               CALL UTIMPK('S',' DANS LA TABLE ',1,DEPINF)
               CALL UTFINM
               GOTO 202
            ENDIF
 130     CONTINUE

C        --- ON VERIFIE QUE LES NOEUDS SONT EN VIS_A_VIS ---

         PRECN = PRECV / DMAX
         DO 50 IN = 2 , NBVAL
            D = ( ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+1) -
     +            ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+1) ) ** 2
            D = D + ( ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+2) -
     +                ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+2) ) ** 2
            D = D + ( ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+3) -
     +                ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+3) ) ** 2
            IF ( SQRT(D) .GT. PRECN ) THEN
               CALL UTMESS('A',NOMCMD,
     +                     'LES NOEUDS NE SONT PAS EN VIS_A_VIS')
               GOTO 202
            ENDIF
 50      CONTINUE

C        --- REPERE LOCAL ---

         IN = NBVAL
         VO(1) = ( ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+1) +
     +             ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+1) ) / 2
         VO(2) = ( ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+2) +
     +             ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+2) ) / 2
         VO(3) = ( ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+3) +
     +             ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+3) ) / 2
         IN = 1
         VE(1) = ( ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+1) +
     +             ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+1) ) / 2
         VE(2) = ( ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+2) +
     +             ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+2) ) / 2
         VE(3) = ( ZR(IDCOOR-1+3*(ZI(KNULS+IN-1)-1)+3) +
     +             ZR(IDCOOR-1+3*(ZI(KNULI+IN-1)-1)+3) ) / 2
         CALL PKCHGR ( VO, VE, VECTY, NBVAL, ZR(JDXS), ZR(JDYS),
     +            ZR(JDZS), ZR(JDXI), ZR(JDYI), ZR(JDZI), ZR(JABSCS) )

C ------ ON CALCULE LES K1, K2, K3

         CALL PKCALC ( NDIM, NBVAL, ABSSUP, DXSUP, DYSUP, DZSUP,
     +                 ABSINF, DXINF, DYINF, DZINF,
     +                 COEFD, COEFD3, COEFG, COEFG3, KG1(3), KG2(3) )

         IF ( INF .EQ. 1 ) THEN
            ABSC = 0.D0
         ELSE
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1-IPAS)), NUMORI )
            X1 = ZR(IDCOOR-1+3*(NUMORI-1)+1)
            Y1 = ZR(IDCOOR-1+3*(NUMORI-1)+2)
            Z1 = ZR(IDCOOR-1+3*(NUMORI-1)+3)
            ABSC = ABSC +
     +             SQRT((X0(1)-X1)**2 + (X0(2)-Y1)**2 + (X0(3)-Z1)**2)
         ENDIF
         KG1(2) = ABSC
         KG2(2) = ABSC
         K8B = ZK8(JNOFO+INF-1)

         KG1(1) = DINST
         KG2(1) = DINST

         CALL TBAJLI ( NOMRES, NBPAR1, NOMPA1, 1, KG1, CBID, K8B, 0 )
         CALL TBAJLI ( NOMRES, NBPAR1, NOMPA1, 2, KG2, CBID, K8B, 0 )

 202     CONTINUE

         CALL JEDETR ( '&&PKFOND_INTERS_SUP' )
         CALL JEDETR ( '&&PKFOND_INTERS_INF' )
         CALL JEDETR ( NOESUP )
         CALL JEDETR ( NUMSUP )
         CALL JEDETR ( ABSSUP )
         CALL JEDETR ( NOEINF )
         CALL JEDETR ( NUMINF )
         CALL JEDETR ( ABSINF )
         CALL JEDETR ( DXSUP )
         CALL JEDETR ( DYSUP )
         CALL JEDETR ( DZSUP )
         CALL JEDETR ( DXINF )
         CALL JEDETR ( DYINF )
         CALL JEDETR ( DZINF )

 200  CONTINUE

         IF ( EXIST ) THEN
            CALL DETRSD ( 'TABLE', DEPSU2 )
            CALL DETRSD ( 'TABLE', DEPIN2 )
         ENDIF

 100  CONTINUE

      CALL JEDETR ( MESNOE )
      CALL JEDETR ( '&&PKDEPL.DEPL_SUP'     )
      CALL JEDETR ( '&&PKDEPL.DEPL_INF'     )
      CALL JEDETR ( '&&PKFOND_LIST_NOEUD'     )
      CALL JEDETR ( '&&PKFOND_NOEU_LEV_SUP'   )
      CALL JEDETR ( '&&PKFOND_NOEU_LEV_INF'   )
      CALL JEDETR ( '&&PKFOND_COOR_LEV_SUP'   )
      CALL JEDETR ( '&&PKFOND_COOR_LEV_INF'   )
      CALL JEDETR ( '&&PKFOND_MAILLE_LEV_SUP' )
      CALL JEDETR ( '&&PKFOND_MAILLE_LEV_INF' )
      IF ( EXIST )  CALL JEDETR ( '&&PKFOND.INSTANT' )

 1000 FORMAT(/,'--> TRAITEMENT DU NOEUD DU FOND DE FISSURE: ',A8)
 1010 FORMAT('   NOEUD RETENU POUR LA LEVRE SUP: ',A8,3X,1P,E12.5)
 1020 FORMAT('   NOEUD RETENU POUR LA LEVRE INF: ',A8,3X,1P,E12.5)
 1100 FORMAT(/,'==> INSTANT: ',1P,E12.5)

      CALL JEDEMA ( )
      END
