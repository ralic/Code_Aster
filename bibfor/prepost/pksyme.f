      SUBROUTINE PKSYME (NOMRES, DEPSUP, RMAX, VECTY, NDIM, COEFD,   
     +              COEFD3,COEFG,COEFG3, JINST, NBINST, EXIST, SYMECH)
      IMPLICIT   NONE
      CHARACTER*8     NOMRES, DEPSUP, SYMECH
      REAL*8          RMAX, COEFD, COEFD3, COEFG, COEFG3, VECTY(3)
      INTEGER         NDIM, JINST,NBINST
      LOGICAL         EXIST
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 01/02/2005   AUTEUR GALENNE E.GALENNE 
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
C     POST_K1_K2_K3 : calcul dans le cas d'un maillage symetrique
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
      INTEGER      IBID, N1, N2, NBPAR1, NBPAR2, 
     +             NBVAL, JABSCS, JDXS, JDYS, JDZS, JABSCI, JDXI, JDYI,
     +             I, JDZI, NFON, IFM, NIV, NRE,
     +             JCOXS, JCOYS, JCOZS, JCOXI, JCOYI, JCOZI
      PARAMETER  ( NBPAR1=10 , NBPAR2=8  )
      REAL*8       PREC, PRECV, 
     +             KG2(9),KG1(9),KG3(9),VO(3),VE(3),RMAXEM,
     +             DINST
      COMPLEX*16   CBID
      CHARACTER*8  K8B, FOND, CRIT
      CHARACTER*16 NOMCMD, CONCEP, MOTFAC,
     +             NOMPA1(NBPAR1), NOMPA2(NBPAR2)
      CHARACTER*19 DEPSU2, DEPIN2
      CHARACTER*24 ABSSUP, DXSUP, DYSUP, DZSUP, 
     +             DXINF, DYINF, DZINF,
     +             COORXS, COORYS, COORZS
C
      DATA  NOMPA1 / 'INST' , 'METHODE' , 'K1_MAX' , 'K1_MIN' , 
     +               'K2_MAX' , 'K2_MIN' , 'K3_MAX' , 'K3_MIN' ,
     +               'G_MAX' , 'G_MIN' /
      DATA  NOMPA2 / 'INST' , 'METHODE' , 'K1_MAX' , 'K1_MIN' , 
     +               'K2_MAX' , 'K2_MIN' , 'G_MAX' ,  'G_MIN' /
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
C
C     RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )
C     ------------------------------------------------------------------
      
      ABSSUP = '&&PKSYME.ABSC_CURV_SUP'
      DXSUP  = '&&PKSYME.DX_SUP'
      DYSUP  = '&&PKSYME.DY_SUP'
      DZSUP  = '&&PKSYME.DZ_SUP'
      DXINF  = '&&PKSYME.DX_INF'
      DYINF  = '&&PKSYME.DY_INF'
      DZINF  = '&&PKSYME.DZ_INF'
      COORXS = '&&PKSYME.COOR_X_SUP'
      COORYS = '&&PKSYME.COOR_Y_SUP'
      COORZS = '&&PKSYME.COOR_Z_SUP'
      DEPSU2 = '&&PKSYME.DEPL_SUP'
C
C     --- BOUCLE SUR LES INSTANTS ---
C
      DO 100 I = 1 , NBINST
         CALL JEMARQ()
         IF ( EXIST ) THEN
            DINST = ZR(JINST+I-1)
            CALL TBEXTB ( DEPSUP, 'V', DEPSU2, 1, 'INST', 'EQ',
     +                    IBID, DINST, CBID, K8B, PREC, CRIT )
         ELSE
            DINST = 0.D0
            DEPSU2 = DEPSUP
         ENDIF
         IF ( NIV .EQ. 2 )  WRITE(IFM,1100) DINST
C
C        --- ON RECUPERE LES "ABSC_CURV", LES "DEPL" ET LES "COOR" ---
C
         CALL PKREC2 ( DEPSU2, ABSSUP, DXSUP, DYSUP, DZSUP,
     +                    COORXS, COORYS, COORZS,
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
C         CALL JEVEUO ( ABSINF, 'L', JABSCI )
         CALL WKVECT ( DXINF , 'V V R', NBVAL, JDXI  )
         CALL WKVECT ( DYINF , 'V V R', NBVAL, JDYI  )
         CALL WKVECT ( DZINF , 'V V R', NBVAL, JDZI  )
C         CALL JEVEUO ( DXINF , 'E', JDXI   )
C         CALL JEVEUO ( DYINF , 'E', JDYI   )
C         CALL JEVEUO ( DZINF , 'E', JDZI   )
C         CALL JEVEUO ( COORXI, 'L', JCOXI  )
C         CALL JEVEUO ( COORYI, 'L', JCOYI  )
C         CALL JEVEUO ( COORZI, 'L', JCOZI  )
C
C ------ REPERE LOCAL
C
         VO(1) =  ZR(JCOXS+NBVAL-1) 
         VO(2) =  ZR(JCOYS+NBVAL-1) 
         VO(3) =  ZR(JCOZS+NBVAL-1) 
C
         VE(1) =  ZR(JCOXS) 
         VE(2) =  ZR(JCOYS) 
         VE(3) =  ZR(JCOZS) 
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
         ENDIF
         CALL JEDETR ( ABSSUP )
         CALL JEDETR ( DXSUP  )
         CALL JEDETR ( DYSUP  )
         CALL JEDETR ( DZSUP  )
         CALL JEDETR ( DXINF  )
         CALL JEDETR ( DYINF  )
         CALL JEDETR ( DZINF  )
         CALL JEDETR ( COORXS )
         CALL JEDETR ( COORYS )
         CALL JEDETR ( COORZS )
C
         CALL JEDEMA()
 100  CONTINUE
C
C
 9999 CONTINUE
 1100 FORMAT(/,'==> INSTANT: ',1P,E12.5)
      CALL JEDEMA ( )
      END
