      SUBROUTINE ASCTUB ( MAILLA )
      IMPLICIT   NONE        
      INCLUDE 'jeveux.h'
      CHARACTER*8         MAILLA
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     OPERATEUR: "MODI_MAILLAGE" , MOTCLE FACTEUR "PLAQ_TUBE"
C
C     REALISE LA TRANSFORMATION PLAQUE-TUBE
C
C-----------------------------------------------------------------------
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
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETVR8 ( 'PLAQ_TUBE', 'DEXT'      , 1,IARG,1, DEXT     , N1 )
      CALL GETVR8 ( 'PLAQ_TUBE', 'EPAIS'     , 1,IARG,1, EP       , N1 )
      CALL GETVR8 ( 'PLAQ_TUBE', 'AZIMUT'    , 1,IARG,1, AZIM     , N1 )
      CALL GETVR8 ('PLAQ_TUBE','L_TUBE_P1',1,IARG,1,
     &             LTCHAR   , N1 )
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
