      SUBROUTINE PECAG3 ( NDIM, NSYMX, NSYMY, NOMA, MOTCLE, NBMAIL,
     +                    NOMMAI, VALPAR )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             NDIM,                             NBMAIL
      REAL*8                      VALPAR(*)
      CHARACTER*8         NOMMAI(*),          NOMA
      CHARACTER*(*)                                  MOTCLE
      LOGICAL                   NSYMX, NSYMY
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/12/98   AUTEUR JMBHH01 J.M.PROIX 
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
C     OPERATEUR   POST_ELEM
C     TRAITEMENT DU MOT CLE-FACTEUR "CARA_GEOM"
C     ------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8  K8B
      CHARACTER*24 MLGNNO, MLGGMA, MLGNMA, MLGVAL, MLGCOX
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
      MLGGMA = NOMA//'.GROUPEMA'
      MLGCOX = NOMA//'.CONNEX'
      MLGVAL = NOMA//'.COORDO    .VALE'
      CALL JEVEUO(MLGVAL,'L',JCOOR)
      CALL JELIRA (MLGVAL,'LONMAX',NBNOEU,K8B)
      NBNOEU = NBNOEU / 3
C
      IF ( NDIM .EQ. 2 ) THEN
         CDX = VALPAR(13)
         CDY = VALPAR(14)
         ALPHA = R8DGRD() * VALPAR(20)
         COSA = COS ( ALPHA )
         SINA = SIN ( ALPHA )
      ELSE
         CALL UTMESS('F','POST_ELEM',
     &                    'ON NE TRAITE QUE DES PROBLEMES 2D.')
         CDX = VALPAR(19)
         CDY = VALPAR(20)
      ENDIF
      XMAX = -R8MAEM()
      XMIN =  R8MAEM()
      YMAX = -R8MAEM()
      YMIN =  R8MAEM()
      ZMAX = -R8MAEM()
      ZMIN =  R8MAEM()
      RMAX = -R8MAEM()
C
      IF ( MOTCLE(1:4) .EQ. 'TOUT' ) THEN
         DO 10 I = 1 , NBNOEU
            X0 = ZR(JCOOR-1+3*(I-1)+1) - CDX
            Y0 = ZR(JCOOR-1+3*(I-1)+2) - CDY
            X = X0*COSA + Y0*SINA
            Y = Y0*COSA - X0*SINA
            R = SQRT ( X*X + Y*Y )
            XMAX = MAX ( XMAX , X )
            XMIN = MIN ( XMIN , X )
            YMAX = MAX ( YMAX , Y )
            YMIN = MIN ( YMIN , Y )
            ZMAX = 0.D0
            ZMIN= 0.D0
            RMAX = MAX ( RMAX , R )
 10      CONTINUE
C
      ELSEIF ( MOTCLE(1:6) .EQ. 'MAILLE' ) THEN
         DO 20 IM = 1 , NBMAIL
            CALL JENONU (JEXNOM(NOMA//'.NOMMAI',NOMMAI(IM)),IBID)
            CALL JEVEUO (JEXNUM(MLGCOX,IBID),'L',JDES)
            CALL JELIRA (JEXNUM(MLGCOX,IBID),'LONMAX',NBNO,K8B)
            DO 22 IN = 1 , NBNO
               NUNO = ZI(JDES+IN-1)
               X0 = ZR(JCOOR-1+3*(NUNO-1)+1)-CDX
               Y0 = ZR(JCOOR-1+3*(NUNO-1)+2)-CDY
               X = X0*COSA + Y0*SINA 
               Y = Y0*COSA - X0*SINA
               R = SQRT ( X*X + Y*Y )
               XMAX = MAX ( XMAX , X )
               XMIN = MIN ( XMIN , X )
               YMAX = MAX ( YMAX , Y )
               YMIN = MIN ( YMIN , Y )
               ZMAX = 0.D0
               ZMIN = 0.D0
               RMAX = MAX ( RMAX , R )
 22         CONTINUE
 20      CONTINUE
C
      ELSEIF ( MOTCLE(1:8) .EQ. 'GROUP_MA' ) THEN
         DO 30 IG = 1 , NBMAIL
            CALL JEVEUO (JEXNOM(MLGGMA,NOMMAI(IG)),'L',JGRO)
            CALL JELIRA (JEXNOM(MLGGMA,NOMMAI(IG)),'LONMAX',NBMA,K8B)
            DO 32 IM = 1 , NBMA
               NUMAIL = ZI(JGRO+IM-1)
               CALL JEVEUO (JEXNUM(MLGCOX,NUMAIL),'L',JDES)
               CALL JELIRA (JEXNUM(MLGCOX,NUMAIL),'LONMAX',NBNO,K8B)
               DO 34 IN = 1 , NBNO
                  NUNO = ZI(JDES+IN-1)
                  X0 = ZR(JCOOR-1+3*(NUNO-1)+1)-CDX
                  Y0 = ZR(JCOOR-1+3*(NUNO-1)+2)-CDY
                  X = X0*COSA + Y0*SINA 
                  Y = Y0*COSA - X0*SINA 
                  R = SQRT ( X*X + Y*Y )
                  XMAX = MAX ( XMAX , X )
                  XMIN = MIN ( XMIN , X )
                  YMAX = MAX ( YMAX , Y )
                  YMIN = MIN ( YMIN , Y )
                  ZMAX = 0.D0
                  ZMIN = 0.D0
                  RMAX = MAX ( RMAX , R )
 34            CONTINUE
 32         CONTINUE
 30      CONTINUE
      ENDIF
C
      IF ( NSYMX ) THEN
         X0 = 1.D0
         Y0 = 0.D0
         X = X0*COSA + Y0*SINA
         Y = Y0*COSA - X0*SINA
         IF (ABS(ABS(X)-1.D0).LE.1.D-5) THEN
           TAMP = MAX( ABS(YMIN) , ABS(YMAX) )
           YMIN = -TAMP
           YMAX =  TAMP
         ELSE
           TAMP = MAX( ABS(XMIN) , ABS(XMAX) )
           XMIN = -TAMP
           XMAX =  TAMP
         ENDIF
      ENDIF
      IF ( NSYMY ) THEN
         X0 = 0.D0
         Y0 = 1.D0
         X = X0*COSA + Y0*SINA
         Y = Y0*COSA - X0*SINA
         IF (ABS(ABS(Y)-1.D0).LE.1.D-5) THEN
           TAMP = MAX( ABS(XMIN) , ABS(XMAX) )
           XMIN = -TAMP
           XMAX =  TAMP
         ELSE
           TAMP = MAX( ABS(YMIN) , ABS(YMAX) )
           YMIN = -TAMP
           YMAX =  TAMP
         ENDIF
      ENDIF
      IF ( NDIM .EQ. 2 ) THEN
         VALPAR( 7) = XMAX
         VALPAR( 8) = YMAX
         VALPAR( 9) = XMIN
         VALPAR(10) = YMIN
         VALPAR(11) = RMAX
      ELSE
         VALPAR(11) = XMAX
         VALPAR(12) = YMAX
         VALPAR(13) = ZMAX
         VALPAR(14) = XMIN
         VALPAR(15) = YMIN
         VALPAR(16) = ZMIN
         VALPAR(17) = RMAX
      ENDIF
C
      CALL JEDEMA ( )
C
      END
