      SUBROUTINE OP0188 ( IER )
      IMPLICIT   NONE
      INTEGER             IER

C RESPONSABLE GALENNE E.GALENNE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/01/2006   AUTEUR REZETTE C.REZETTE 
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
C
C     OPERATEUR POST_K1_K2_K3
C
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      IBID, N1, N2, NBPAR1, NBPAR2, NBINST, JINST,
     +             NBVAL, JABSCS, JDXS, JDYS, JDZS, JABSCI, JDXI, JDYI,
     +             NDIM, I, JDZI, NFON, IFM , NIV, NRE, ITCOEF,
     +             JCOXS, JCOYS, JCOZS, JCOXI, JCOYI, JCOZI
      PARAMETER  ( NBPAR1=10 , NBPAR2=8  )
      REAL*8       RMAX, COEFD, COEFG, COEFG3, COEFD3, PREC, PRECV,
     +             KG2(9),KG1(9),KG3(9),VECTY(3),VO(3),VE(3),RMAXEM,
     +             DINST
      COMPLEX*16   CBID
      LOGICAL      EXIST
      CHARACTER*2  TYPPA1(NBPAR1), TYPPA2(NBPAR2)
      CHARACTER*8  K8B, NOMRES, FOND, DEPSUP, CRIT, SYMECH
      CHARACTER*16 NOMCMD, CONCEP, MOTFAC,
     +             NOMPA1(NBPAR1), NOMPA2(NBPAR2)
      CHARACTER*19 DEPSU2, DEPIN2
      CHARACTER*24 ABSSUP, DXSUP, DYSUP, DZSUP, ABSINF,
     +             DEPINF, DXINF, DYINF, DZINF, TCOEF,
     +             COORXS, COORYS, COORZS, COORXI, COORYI, COORZI
C
      DATA  NOMPA1 / 'INST' , 'METHODE' , 'K1_MAX' , 'K1_MIN' ,
     +               'K2_MAX' , 'K2_MIN' , 'K3_MAX' , 'K3_MIN' ,
     +               'G_MAX' , 'G_MIN' /
      DATA  TYPPA1 / 'R', 'I', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
      DATA  NOMPA2 / 'INST' , 'METHODE' , 'K1_MAX' , 'K1_MIN' ,
     +               'K2_MAX' , 'K2_MIN' , 'G_MAX' ,  'G_MIN' /
      DATA  TYPPA2 / 'R' , 'I' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R'  /
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
C     RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )
C
C     ------------------------------------------------------------------
C                           FOND_FISS
C     ------------------------------------------------------------------
C
        CALL GETVID ( ' ', 'RESULTAT', 1,1,1,K8B, NRE )
        CALL GETVID ( ' ', 'FOND_FISS', 1,1,1, FOND, NFON )
        IF ((NRE.NE.0).AND.(NFON.EQ.0)) THEN
          CALL UTMESS('F','POST_K1_K2_K3','SI LE MOT CLE RESULTAT  '//
     +     'EST RENSEIGNE IL FAUT FOND_FISS')
        ENDIF
        IF ( NFON .NE. 0 ) THEN
           CALL PKFOND ( FOND )
           GOTO 8888
        ENDIF


C
C     ------------------------------------------------------------------
C             LA TABLE DE DEPLACEMENT DE LA LEVRE SUPERIEURE
C     ------------------------------------------------------------------
C
      CALL GETVID ( ' ', 'TABL_DEPL_SUP', 1,1,1, DEPSUP, N1 )
C
C     ------------------------------------------------------------------
C                     CHOIX DE ABSC_CURV_MAXI
C     ------------------------------------------------------------------
C
      RMAX = RMAXEM()
      CALL GETVR8 ( ' ', 'ABSC_CURV_MAXI', 1,1,1, RMAX , N1 )
C
      CALL GETVR8 ( ' ', 'PREC_VIS_A_VIS', 1,1,1, PRECV, N1 )
C
C     ------------------------------------------------------------------
C                        CHOIX DU REPERE
C     ------------------------------------------------------------------
C
      CALL GETVR8 ( ' ', 'VECT_K1', 1,1,3, VECTY, N1 )
C
C     ------------------------------------------------------------------
C                     CARACTERISTIQUES MATERIAUX
C     ------------------------------------------------------------------
C
      CALL PKMATE ( NDIM, COEFD, COEFD3, COEFG, COEFG3, TCOEF, ITCOEF )
C
C    ------------------------------------------------------------------
C                     VERIFICATION DE LA TABLE 'DEPSUP'
C     ------------------------------------------------------------------
      CALL TBEXP2(DEPSUP,'DX')
      CALL TBEXP2(DEPSUP,'DY')
      CALL TBEXP2(DEPSUP,'COOR_X')
      CALL TBEXP2(DEPSUP,'COOR_Y')
      CALL TBEXP2(DEPSUP,'ABSC_CURV')
      IF (NDIM .EQ. 3)THEN
         CALL TBEXP2(DEPSUP,'DZ')
         CALL TBEXP2(DEPSUP,'COOR_Z')
      ENDIF      
C
C     ------------------------------------------------------------------
C                   LES INSTANTS DE POST-TRAITEMENT
C     ------------------------------------------------------------------
      CALL TBEXIP ( DEPSUP, 'INST', EXIST, K8B )
      IF ( EXIST ) THEN
         MOTFAC = ' '
         CALL TBUTNU ( MOTFAC, 1, '&&OP0188.INSTANT', NBINST, DEPSUP,
     +                                                 PREC, CRIT )
         CALL JEVEUO ( '&&OP0188.INSTANT', 'L', JINST )
      ELSE
         NBINST = 1
      ENDIF
C
C     ------------------------------------------------------------------
C                             LA TABLE
C     ------------------------------------------------------------------
      CALL TBCRSD ( NOMRES, 'G' )
      IF ( NDIM .EQ. 3 ) THEN
         IF ( EXIST ) THEN
            CALL TBAJPA ( NOMRES, NBPAR1, NOMPA1, TYPPA1 )
         ELSE
            CALL TBAJPA ( NOMRES, NBPAR1-1, NOMPA1(2), TYPPA1(2) )
         ENDIF
      ELSE
         IF ( EXIST ) THEN
            CALL TBAJPA ( NOMRES, NBPAR2, NOMPA2, TYPPA2 )
         ELSE
            CALL TBAJPA ( NOMRES, NBPAR2-1, NOMPA2(2), TYPPA2(2) )
         ENDIF
      ENDIF

C
C --- SYMETRIE DU CHARGEMENT --------------------------
C

      CALL GETVTX(' ','SYME_CHAR',0,1,1,SYMECH,IBID)
      IF (SYMECH .NE. 'SANS' ) THEN
           CALL PKSYME (NOMRES,DEPSUP, RMAX, VECTY, NDIM, COEFD,
     +                COEFD3,COEFG,COEFG3,JINST,NBINST,EXIST,SYMECH)
           GOTO 8888
       ENDIF
C
C     ------------------------------------------------------------------
C             LA TABLE DE DEPLACEMENT DE LA LEVRE INFERIEURE
C     ------------------------------------------------------------------
C
      CALL GETVID ( ' ', 'TABL_DEPL_INF', 1,1,1, DEPINF, N2 )
C
      IF ( N1*N2 .EQ. 0 ) THEN
         CALL UTMESS('F',NOMCMD,'LES MOTS CLES "TABL_DEPL_SUP" ET '//
     +                    '"TABL_DEPL_INF" SONT OBLIGATOIRES')
      ENDIF
C
C    ------------------------------------------------------------------
C                     VERIFICATION DE LA TABLE 'DEPINF'
C     ------------------------------------------------------------------
      CALL TBEXP2(DEPINF,'DX')
      CALL TBEXP2(DEPINF,'DY')
      CALL TBEXP2(DEPINF,'COOR_X')
      CALL TBEXP2(DEPINF,'COOR_Y')
      CALL TBEXP2(DEPINF,'ABSC_CURV')
      IF (NDIM .EQ. 3)THEN
         CALL TBEXP2(DEPINF,'DZ')
         CALL TBEXP2(DEPINF,'COOR_Z')
      ENDIF      
C
C     ------------------------------------------------------------------
C
      ABSSUP = '&&OP0188.ABSC_CURV_SUP'
      ABSINF = '&&OP0188.ABSC_CURV_INF'
      DXSUP  = '&&OP0188.DX_SUP'
      DYSUP  = '&&OP0188.DY_SUP'
      DZSUP  = '&&OP0188.DZ_SUP'
      DXINF  = '&&OP0188.DX_INF'
      DYINF  = '&&OP0188.DY_INF'
      DZINF  = '&&OP0188.DZ_INF'
      COORXS = '&&OP0188.COOR_X_SUP'
      COORYS = '&&OP0188.COOR_Y_SUP'
      COORZS = '&&OP0188.COOR_Z_SUP'
      COORXI = '&&OP0188.COOR_X_INF'
      COORYI = '&&OP0188.COOR_Y_INF'
      COORZI = '&&OP0188.COOR_Z_INF'
      DEPSU2 = '&&OP0188.DEPL_SUP'
      DEPIN2 = '&&OP0188.DEPL_INF'
C     ------------------------------------------------------------------
C
C     --- BOUCLE SUR LES INSTANTS ---
C
      DO 100 I = 1 , NBINST
         CALL JEMARQ()
         CALL JERECU('V')
         IF ( EXIST ) THEN
            DINST = ZR(JINST+I-1)
            CALL TBEXTB ( DEPSUP, 'V', DEPSU2, 1, 'INST', 'EQ',
     +                    IBID, DINST, CBID, K8B, PREC, CRIT )

            CALL TBEXTB ( DEPINF, 'V', DEPIN2, 1, 'INST', 'EQ',
     +                    IBID, DINST, CBID, K8B, PREC, CRIT )
         ELSE
            DINST = 0.D0
            DEPSU2 = DEPSUP
            DEPIN2 = DEPINF
         ENDIF
         IF ( NIV .EQ. 2 )  WRITE(IFM,1100) DINST
C
C        --- ON RECUPERE LES "ABSC_CURV", LES "DEPL" ET LES "COOR" ---
C
         CALL PKRECU ( DEPSU2, ABSSUP, DXSUP, DYSUP, DZSUP,
     +                    DEPIN2, ABSINF, DXINF, DYINF, DZINF,
     +                    COORXS, COORYS, COORZS,
     +                    COORXI, COORYI, COORZI,
     +                    NBVAL , NDIM  , PRECV , RMAX  )
C
         CALL JEVEUO ( ABSSUP, 'L', JABSCS )
         CALL JEVEUO ( DXSUP , 'E', JDXS   )
         CALL JEVEUO ( DYSUP , 'E', JDYS   )
         CALL JEVEUO ( DZSUP , 'E', JDZS   )
         CALL JEVEUO ( COORXS, 'L', JCOXS  )
         CALL JEVEUO ( COORYS, 'L', JCOYS  )
         CALL JEVEUO ( COORZS, 'L', JCOZS  )
C
         CALL JEVEUO ( ABSINF, 'L', JABSCI )
         CALL JEVEUO ( DXINF , 'E', JDXI   )
         CALL JEVEUO ( DYINF , 'E', JDYI   )
         CALL JEVEUO ( DZINF , 'E', JDZI   )
         CALL JEVEUO ( COORXI, 'L', JCOXI  )
         CALL JEVEUO ( COORYI, 'L', JCOYI  )
         CALL JEVEUO ( COORZI, 'L', JCOZI  )
C
C ------ REPERE LOCAL
C
         VO(1) = ( ZR(JCOXS+NBVAL-1) + ZR(JCOXI+NBVAL-1) ) / 2
         VO(2) = ( ZR(JCOYS+NBVAL-1) + ZR(JCOYI+NBVAL-1) ) / 2
         VO(3) = ( ZR(JCOZS+NBVAL-1) + ZR(JCOZI+NBVAL-1) ) / 2
C
         VE(1) = ( ZR(JCOXS) + ZR(JCOXI) ) / 2
         VE(2) = ( ZR(JCOYS) + ZR(JCOYI) ) / 2
         VE(3) = ( ZR(JCOZS) + ZR(JCOZI) ) / 2
C
         CALL PKCHGR ( VO, VE, VECTY, NBVAL, ZR(JDXS), ZR(JDYS),
     +             ZR(JDZS), ZR(JDXI), ZR(JDYI), ZR(JDZI), ZR(JABSCS),
     +            SYMECH)
C
C     ------------------------------------------------------------------
C                       CALCUL DES K1, K2, K3
C     ------------------------------------------------------------------
C
         KG1(1) = DINST
         KG2(1) = DINST
         KG3(1) = DINST
         CALL PKCALC ( NDIM, NBVAL, ABSSUP, DXSUP, DYSUP,
     +                 DZSUP, DXINF, DYINF, DZINF,
     +                 COEFD,COEFD3,COEFG,COEFG3,KG1(2),KG2(2),KG3(2))
C
         IF ( NDIM .EQ. 3 ) THEN
            IF ( EXIST ) THEN

               CALL TBAJLI ( NOMRES, NBPAR1, NOMPA1, 1, KG1,
     +                       CBID, K8B, 0 )
               CALL TBAJLI ( NOMRES, NBPAR1, NOMPA1, 2, KG2,
     +                       CBID, K8B, 0 )
               CALL TBAJLI ( NOMRES, NBPAR1, NOMPA1, 3, KG3,
     +                       CBID, K8B, 0 )
            ELSE

               CALL TBAJLI ( NOMRES, NBPAR1-1, NOMPA1(2), 1, KG1(2),
     +                       CBID, K8B, 0 )
               CALL TBAJLI ( NOMRES, NBPAR1-1, NOMPA1(2), 2, KG2(2),
     +                       CBID, K8B, 0 )
               CALL TBAJLI ( NOMRES, NBPAR1-1, NOMPA1(2), 3, KG3(2),
     +                       CBID, K8B, 0)
            ENDIF
         ELSE
            IF ( EXIST ) THEN
               CALL TBAJLI ( NOMRES, NBPAR2, NOMPA2, 1, KG1,
     +                       CBID, K8B, 0 )
               CALL TBAJLI ( NOMRES, NBPAR2, NOMPA2, 2, KG2,
     +                       CBID, K8B, 0 )
               CALL TBAJLI ( NOMRES, NBPAR2, NOMPA2, 3, KG3,
     +                       CBID, K8B, 0 )
            ELSE
               CALL TBAJLI ( NOMRES, NBPAR2-1, NOMPA2(2), 1, KG1(2),
     +                       CBID, K8B, 0 )
               CALL TBAJLI ( NOMRES, NBPAR2-1, NOMPA2(2), 2, KG2(2),
     +                       CBID, K8B, 0 )
               CALL TBAJLI ( NOMRES, NBPAR2-1, NOMPA2(2), 3, KG3(2),
     +                       CBID, K8B, 0 )
            ENDIF
         ENDIF
C
         IF ( EXIST ) THEN
            CALL DETRSD ( 'TABLE', DEPSU2 )
            CALL DETRSD ( 'TABLE', DEPIN2 )
         ENDIF
         CALL JEDETR ( ABSSUP )
         CALL JEDETR ( DXSUP  )
         CALL JEDETR ( DYSUP  )
         CALL JEDETR ( DZSUP  )
         CALL JEDETR ( ABSINF )
         CALL JEDETR ( DXINF  )
         CALL JEDETR ( DYINF  )
         CALL JEDETR ( DZINF  )
         CALL JEDETR ( COORXS )
         CALL JEDETR ( COORYS )
         CALL JEDETR ( COORZS )
         CALL JEDETR ( COORXI )
         CALL JEDETR ( COORYI )
         CALL JEDETR ( COORZI )
C
         CALL JEDEMA()
 100  CONTINUE
C
C
 8888 CONTINUE
C
      CALL TITRE
C
 9999 CONTINUE
 1100 FORMAT(/,'==> INSTANT: ',1P,E12.5)
      CALL JEDEMA ( )
      END
