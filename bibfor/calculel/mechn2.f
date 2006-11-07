      SUBROUTINE MECHN2 ( NOMA, CHNUMC, CHPLAN )
      IMPLICIT  NONE
      CHARACTER*8         NOMA
      CHARACTER*24              CHNUMC, CHPLAN
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 07/11/2006   AUTEUR CIBHHLV L.VIVAN 
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
C     CREE UNE CARTE POUR LES COQUES
C     ------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : MOTCLE : MOTCLE FACTEUR
C OUT : CHNUMC : NOM DE LA CARTE CREEE POUR "NUME_COUCHE","NIVE_COUCHE"
C OUT : CHNUMC : NOM DE LA CARTE CREEE POUR "PLAN"
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
C     ------------------------------------------------------------------
      INTEGER      IBID, IVAL(3), NX3, NCOU, NANGL, IOC, N1, NA, NVEC,
     +             NREP, NBMA, JMAIL, JDCC, JDVC, JDCP, JDVP
      REAL*8       R8B, RPLAN
      CHARACTER*3  ORDO
      CHARACTER*8  K8B, LICMP(3), MOTCLS(2), TYPMCL(2), PLAN
      CHARACTER*16 MOTCLE
      CHARACTER*24 MESMAI
      COMPLEX*16   C16B
C DEB-------------------------------------------------------------------
C
      MOTCLE = 'REPE_COQUE'
C
      LICMP(1) = 'NUMC'
      LICMP(2) = 'ORDO'
      LICMP(3) = 'ANGL'
C
      MOTCLS(1) = 'GROUP_MA'
      MOTCLS(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&MECHN2.MES_MAILLES'
C
      CALL GETFAC ( MOTCLE, NREP ) 
C
      IF ( NREP .EQ. 0 ) THEN
C
C ------ VALEURS PAR DEFAUT
C
         IVAL(1) = 1
         IVAL(2) = 0
         IVAL(3) = 0
         CALL MECACT ( 'V', CHNUMC, 'MAILLA', NOMA, 'NUMC_I', 3, LICMP,
     +                                            IVAL, R8B, C16B, K8B )
C
         RPLAN = DBLE(0)
         CALL MECACT ( 'V', CHPLAN, 'MAILLA', NOMA, 'FREQ_R', 1, 'FREQ',
     &                                          IBID, RPLAN, C16B, K8B )
C
      ELSE
C
C ------ AFFECTATION PAR LISTE DE MAILLES
C
         CALL ALCART ( 'V', CHNUMC, NOMA, 'NUMC_I' )
         CALL JEVEUO ( CHNUMC(1:19)//'.NCMP', 'E', JDCC )
         CALL JEVEUO ( CHNUMC(1:19)//'.VALV', 'E', JDVC )
         ZK8(JDCC)   = 'NUMC'
         ZK8(JDCC+1) = 'ORDO'
         ZK8(JDCC+2) = 'ANGL'
C
         CALL ALCART ( 'V', CHPLAN, NOMA, 'FREQ_R' )
         CALL JEVEUO ( CHPLAN(1:19)//'.NCMP', 'E', JDCP )
         CALL JEVEUO ( CHPLAN(1:19)//'.VALV', 'E', JDVP )
         ZK8(JDCP)   = 'FREQ'
C
         IBID = 0
         K8B  = ' '
         DO 10 IOC = 1 , NREP

            CALL RELIEM(' ', NOMA, 'NO_MAILLE', MOTCLE, IOC, 2, 
     +                                  MOTCLS, TYPMCL, MESMAI, NBMA )
            IF ( NBMA.NE.0 )  CALL JEVEUO ( MESMAI, 'L', JMAIL )

            CALL GETVIS ( MOTCLE, 'NUME_COUCHE', IOC,1,1, NCOU , N1 )
            CALL GETVTX ( MOTCLE, 'NIVE_COUCHE', IOC,1,1, ORDO , N1 )
            CALL GETVIS ( MOTCLE, 'ANGLE',       IOC,1,1, NANGL, N1 )
            IF (ORDO.EQ.'SUP') THEN
               NX3 = 1
            ELSEIF (ORDO.EQ.'MOY') THEN
               NX3 = 0
            ELSEIF (ORDO.EQ.'INF') THEN
               NX3 = -1
            ENDIF
            ZI(JDVC  ) = NCOU
            ZI(JDVC+1) = NX3
            ZI(JDVC+2) = NANGL
C
            IF ( NBMA .EQ. 0 ) THEN
               CALL NOCART ( CHNUMC,1,K8B,K8B,IBID,K8B,IBID,K8B,3 )
            ELSE
            CALL NOCART (CHNUMC,3,K8B,'NOM',NBMA,ZK8(JMAIL),IBID,K8B,3)
            ENDIF
C
            CALL GETVTX ( MOTCLE, 'PLAN', IOC,1,1, PLAN, N1 )
            IF (PLAN(1:4).EQ.'MAIL') THEN
               RPLAN = DBLE(0)
            ELSE IF (PLAN(1:3).EQ.'SUP') THEN
               RPLAN = DBLE(1)
            ELSE IF (PLAN(1:3).EQ.'INF') THEN
               RPLAN = DBLE(-1)
            ELSE IF (PLAN(1:3).EQ.'MOY') THEN
               RPLAN = DBLE(2)
            END IF
            ZR(JDVP) = RPLAN
C
            IF ( NBMA .EQ. 0 ) THEN
               CALL NOCART ( CHPLAN,1,K8B,K8B,IBID,K8B,IBID,K8B,1 )
            ELSE
            CALL NOCART (CHPLAN,3,K8B,'NOM',NBMA,ZK8(JMAIL),IBID,K8B,1)
            ENDIF
C
            IF ( NBMA.NE.0 )  CALL JEDETR ( MESMAI )
C
 10      CONTINUE
C
      ENDIF
C
      END
