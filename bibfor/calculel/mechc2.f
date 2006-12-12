      SUBROUTINE MECHC2 ( CHGEOM, CHMASS )
      IMPLICIT  NONE
      CHARACTER*(*)       CHGEOM, CHMASS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
C     CREATION DE LA CARTE CAMASS POUR L'OPTION "ARCO_ELNO_SIGM"
C     ------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER      IBID, IOC, N1, N2, NA, NVEC, NREP, JDCC, JDVC,
     &             NBMA, JMAIL
      REAL*8       ANG(2), VECT(3), R8PI
      LOGICAL      AFAIRE, LTOUT
      CHARACTER*8  K8B, NOMA, MOTCLS(2), TYPMCL(2)
      CHARACTER*16 MOTCLE
      CHARACTER*19 CARTE
      CHARACTER*24 MESMAI
C DEB-------------------------------------------------------------------
C
      MOTCLE = 'REPE_COQUE'
      CARTE  = CHMASS
      NOMA   = CHGEOM(1:8)
C
      CALL GETFAC ( MOTCLE, NREP ) 
C
      AFAIRE = .FALSE.
      DO 100 IOC = 1 , NREP
         CALL GETVR8 ( MOTCLE, 'ANGL_REP', IOC,1,0, ANG , NA   )
         CALL GETVR8 ( MOTCLE, 'VECTEUR' , IOC,1,0, VECT, NVEC )
         IF ( NA+NVEC .NE. 0 )  AFAIRE = .TRUE.
 100  CONTINUE
      IF ( .NOT. AFAIRE ) THEN
         CALL U2MESS('F','CALCULEL5_55')
      ENDIF
C
      CALL ALCART ( 'V', CARTE, NOMA, 'CAMASS' )
      CALL JEVEUO ( CARTE//'.NCMP', 'E', JDCC )
      CALL JEVEUO ( CARTE//'.VALV', 'E', JDVC )
C
      ZK8(JDCC  ) = 'ALPHA'
      ZK8(JDCC+1) = 'BETA'
C
      MOTCLS(1) = 'GROUP_MA'
      MOTCLS(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&MECHC2.MES_MAILLES'
C
      DO 10 IOC = 1 , NREP
C
         CALL GETVR8 ( MOTCLE, 'ANGL_REP', IOC,1,0, ANG , NA   )
         CALL GETVR8 ( MOTCLE, 'VECTEUR' , IOC,1,0, VECT, NVEC )
         IF ( NA+NVEC .EQ. 0 )  GOTO 10
C
         CALL GETVID ( MOTCLE, 'MAILLE'  , IOC,1,0, K8B, N1 )
         CALL GETVID ( MOTCLE, 'GROUP_MA', IOC,1,0, K8B, N2 )
C
         IF ( N1+N2 .EQ. 0 ) THEN
            LTOUT = .TRUE. 
         ELSE
            CALL RELIEM(' ', NOMA, 'NU_MAILLE', MOTCLE, IOC, 2, 
     +                                  MOTCLS, TYPMCL, MESMAI, NBMA )
            IF ( NBMA.NE.0 )  CALL JEVEUO ( MESMAI, 'L', JMAIL )
            LTOUT = .FALSE. 
         ENDIF
C
         ANG(1) = 0.D0
         ANG(2) = 0.D0
C
         CALL GETVR8 ( MOTCLE, 'ANGL_REP', IOC,1,2, ANG , NA   )
         CALL GETVR8 ( MOTCLE, 'VECTEUR' , IOC,1,3, VECT, NVEC )
         IF (NVEC.NE.0) THEN
            CALL ANGVX ( VECT, ANG(1), ANG(2) )
            ANG(1) =  ANG(1)*180.D0/R8PI()
            ANG(2) = -ANG(2)*180.D0/R8PI()
         ENDIF
C
         ZR(JDVC  ) =  ANG(1)
         ZR(JDVC+1) = -ANG(2)
C
         IF ( LTOUT ) THEN
            CALL NOCART ( CARTE,1,K8B,K8B,IBID,K8B,IBID,K8B,2 )
         ELSE
            CALL NOCART (CARTE,3,K8B,'NUM',NBMA,K8B,ZI(JMAIL),K8B,2)
         ENDIF
C
         IF (.NOT. LTOUT)  CALL JEDETR ( MESMAI )
C
 10   CONTINUE
C
      CALL JEDETR ( CARTE//'.NCMP' )
      CALL JEDETR ( CARTE//'.VALV' )
C
      END
