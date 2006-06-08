      SUBROUTINE ASCTUB ( MAILLA )
      IMPLICIT   NONE        
      CHARACTER*8         MAILLA
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/03/99   AUTEUR AUBHHMB M.BONNAMY 
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
C     OPERATEUR: "MODI_MAILLAGE" , MOTCLE FACTEUR "PLAQ_TUBE"
C
C     REALISE LA TRANSFORMATION PLAQUE-TUBE
C
C-----------------------------------------------------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
C
C     RM        = RAYON MOYEN DU TUBE
C     AZIMUT    = ANGLE DE ROTATION DU TUBE
C     L_TUBE_P1 = LONGUEUR DE L'EMBOUT DROIT INFERIEUR   
C
C-----------------------------------------------------------------------
C
      INTEGER       NBNO, ICOOR, IDIME, N1, INO, NDIM
      REAL*8        RM, AZIM, THETA, RHO, AZIMR, PI, R8PI, XP, YP, 
     +              LTCHAR, DEXT, EP
      CHARACTER*24  COORD, DIME 
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETVR8 ( 'PLAQ_TUBE', 'DEXT'      , 1,1,1, DEXT     , N1 )
      CALL GETVR8 ( 'PLAQ_TUBE', 'EPAIS'     , 1,1,1, EP       , N1 )
      CALL GETVR8 ( 'PLAQ_TUBE', 'AZIMUT'    , 1,1,1, AZIM     , N1 )
      CALL GETVR8 ( 'PLAQ_TUBE', 'L_TUBE_P1' , 1,1,1, LTCHAR   , N1 ) 
C 
      RM = (DEXT-EP)/2.D0
C
      COORD  = MAILLA//'.COORDO    .VALE'
      DIME   = MAILLA//'.DIME           '
C      
      CALL JEVEUO (COORD,'E',ICOOR)      
      CALL JEVEUO (DIME ,'L',IDIME)
      NBNO  = ZI(IDIME)
      NDIM  = ZI(IDIME+5)
C
      PI = R8PI()
      AZIMR = AZIM*PI/180.D0
C      
C     TRI DE GROUPES DE NOEUDS
C
      CALL ASCTRI(MAILLA, RM)
C
      DO 100 INO=1, NBNO
         XP = ZR(ICOOR+NDIM*(INO-1))
         YP = ZR(ICOOR+NDIM*(INO-1)+1)
         RHO = XP
         IF ((YP+AZIMR*RM).GT.(0.D0)) THEN
            THETA = YP/RM + AZIMR
         ELSE
            THETA = 2.D0*PI + YP/RM + AZIMR
         END IF
         ZR(ICOOR+NDIM*(INO-1))   =   RHO*SIN(THETA)
         ZR(ICOOR+NDIM*(INO-1)+1) = - RHO*COS(THETA)
 100  CONTINUE
C
      CALL JEDEMA()
C
      END
