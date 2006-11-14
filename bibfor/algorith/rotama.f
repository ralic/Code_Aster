      SUBROUTINE ROTAMA ( GEOMI , PT , D , ANGL , BIDIM )
      IMPLICIT   NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/11/2006   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     BUT : ROTATION D'AXE QUELCONQUE D'UN MAILLAGE
C
C     IN :
C            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE A TOURNER
C            NBNO   : NOMBRE DE NOEUDS DE GEOMI
C            PT     : POINT DE L'AXE DE ROTATION
C            D    : DECTION DE L'AXE DE ROTATION
C            ANGL   : ANGLE DE ROTATION
C            BIDIM  : BOOLEEN VRAI SI GEOMETRIE 2D
C     OUT:
C            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE ACTUALISE
C
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER       N1, I, IADCOO
      LOGICAL       BIDIM
      CHARACTER*8   K8BID
      CHARACTER*19  GEOMI
      CHARACTER*24  COORJV
      REAL*8        ANGL, NORMD, PT(3), D(3),
     &              P1MX, P1MY, P1MZ, CA, SA, P1M, PREC, R8DGRD, DNRM2
C ----------------------------------------------------------------------
C
      CALL MATFPE(-1)
C
      CALL JEMARQ()
      COORJV=GEOMI(1:19)//'.VALE'
      CALL JEVEUO(COORJV,'E',IADCOO)
      CALL JELIRA(COORJV,'LONMAX',N1,K8BID)
      PREC=1.D-14
      N1=N1/3
      ANGL=ANGL*R8DGRD()
      CA=COS(ANGL)
      SA=SIN(ANGL)
      IADCOO=IADCOO-1
C     -- ON TRAITE LE CAS 2D SEPAREMENT POUR OPTIMISER :
      IF ( BIDIM ) THEN
         DO 10 I=1,N1
            P1MX=ZR(IADCOO+3*(I-1)+1)-PT(1)
            P1MY=ZR(IADCOO+3*(I-1)+2)-PT(2)
            ZR(IADCOO+3*(I-1)+1)=PT(1)+CA*P1MX-SA*P1MY
            ZR(IADCOO+3*(I-1)+2)=PT(2)+CA*P1MY+SA*P1MX
 10      CONTINUE
      ELSE
         IF ( DNRM2(3,D,1) .LT. PREC ) THEN
            CALL U2MESS('F','ALGORITH10_48')
         ELSE
            P1M=DNRM2(3,D,1)
            D(1)=D(1)/P1M
            D(2)=D(2)/P1M
            D(3)=D(3)/P1M
            DO 20 I=1,N1
               P1MX=ZR(IADCOO+3*(I-1)+1)-PT(1)
               P1MY=ZR(IADCOO+3*(I-1)+2)-PT(2)
               P1MZ=ZR(IADCOO+3*(I-1)+3)-PT(3)
               P1M=P1MX*D(1)+P1MY*D(2)+P1MZ*D(3)
               ZR(IADCOO+3*(I-1)+1)=PT(1)+
     &         CA*P1MX+(1-CA)*P1M*D(1)+SA*(D(2)*P1MZ-D(3)*P1MY)
               ZR(IADCOO+3*(I-1)+2)=PT(2)+
     &         CA*P1MY+(1-CA)*P1M*D(2)+SA*(D(3)*P1MX-D(1)*P1MZ)
               ZR(IADCOO+3*(I-1)+3)=PT(3)+
     &         CA*P1MZ+(1-CA)*P1M*D(3)+SA*(D(1)*P1MY-D(2)*P1MX)
 20         CONTINUE
         ENDIF
      ENDIF
      CALL JEDEMA()
      CALL MATFPE(1)
C
      END
