      SUBROUTINE PKREC2 ( DEPSUP, ABSSUP, DXSUP, DYSUP, DZSUP,
     +                    COORXS, COORYS, COORZS,
     +                    NBVAL , NDIM  , PRECV , RMAX  )
      IMPLICIT   NONE
      INTEGER             NBVAL, NDIM
      REAL*8              PRECV, RMAX
      CHARACTER*(*)       DEPSUP, ABSSUP, DXSUP, DYSUP, DZSUP,
     +                    COORXS, COORYS, COORZS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 30/01/2006   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C     OPERATEUR POST_K1_K2_K3 - CAS SYMETRIQUE 
C     ON RECUPERE LES "ABSC_CURV", "DEPL" ET "COOR" de LEVRE_SUP
C
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
      INTEGER      IBID, I, NBS, NBI, NBRS, NBRI, JABSCS, JABSCI, 
     +             JCOXS, JCOYS, JCOZS, JCOXI, JCOYI, JCOZI, JDZS, JDZI
      REAL*8       RMAXN,  RMAXEM, PRECN, D
      CHARACTER*8  K8B
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
C --- LEVRE SUPERIEURE
C
      CALL TBEXVE ( DEPSUP, 'ABSC_CURV', ABSSUP, 'V', NBS, K8B )
      CALL JEVEUO ( ABSSUP , 'L', JABSCS )
C
      CALL TBEXVE ( DEPSUP, 'DX', DXSUP, 'V', IBID, K8B )
C
      CALL TBEXVE ( DEPSUP, 'DY', DYSUP, 'V', IBID, K8B )
C
      CALL TBEXVE ( DEPSUP, 'COOR_X', COORXS, 'V', IBID, K8B )
      CALL JEVEUO ( COORXS , 'L', JCOXS )
C
      CALL TBEXVE ( DEPSUP, 'COOR_Y', COORYS, 'V', IBID, K8B )
      CALL JEVEUO ( COORYS , 'L', JCOYS )
C
      IF ( NDIM .EQ. 3 ) THEN
         CALL TBEXVE ( DEPSUP, 'DZ'    , DZSUP , 'V', IBID, K8B )
         CALL TBEXVE ( DEPSUP, 'COOR_Z', COORZS, 'V', IBID, K8B )
         CALL JEVEUO ( COORZS, 'L', JCOZS )
      ELSE
         CALL WKVECT ( DZSUP , 'V V R', NBS, JDZS  )
         CALL WKVECT ( COORZS, 'V V R', NBS, JCOZS )
      ENDIF
C
C        --- ON VERIFIE QUE LES "ABSC_CURV" SONT CROISSANTES ---
C
      DO 30 I = 1 , NBS-1
         IF ( ZR(JABSCS+I-1) .GT. ZR(JABSCS+I) ) THEN
             CALL UTMESS('F','PKREC2','LES "ABSC_CURV" DE LA TABLE '//
     +                          DEPSUP(1:8)//'NE SONT PAS CROISSANTES')
         ENDIF
 30   CONTINUE
C     --- LES NOEUDS DES LEVRES < RMAX  ET  TESTS  ---
C
      IF ( RMAX .EQ. RMAXEM() ) THEN
         RMAXN = ZR(JABSCS+NBS-1)
      ELSE
         RMAXN = RMAX
      ENDIF
      NBRS = NBS
      DO 40 I = 1 , NBS
         IF ( ZR(JABSCS+I-1) .GT. RMAXN ) THEN
            NBRS = I - 1
            GOTO 42
         ENDIF
 40   CONTINUE
 42   CONTINUE
      IF ( NBRS .LT. 3 ) THEN
         CALL UTMESS('F','PKREC2',
     +             'IL FAUT AU MOINS 3 NOEUDS SUR LA LEVRE SUPERIEURE')
      ENDIF
C
      NBVAL = NBRS
C
      CALL JEDEMA ( )
      END
