      SUBROUTINE COPMOD ( BASEMO, CHAMZ, NEQ, NU, NBMODE, BMODAL )
      IMPLICIT   NONE
      INTEGER             NEQ, NBMODE
      REAL*8              BMODAL(NEQ*NBMODE)
      CHARACTER*8         BASEMO
      CHARACTER*(*)        NU
      CHARACTER*(*)       CHAMZ
C***********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/11/2008   AUTEUR PELLET J.PELLET 
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
C     FONCTION  :
C     RECOPIE DES MODES PROPRES CONTENU DANS LE CONCEPT MODE_MECA
C     DANS UN VECTEUR DE TRAVAIL
C
C    IN : BASEMO          NOM DU CONCEPT MODE_MECA
C    IN : NEQ             DIMENSION DU SYSTEME ASSEMBLE
C    IN : NBMODE          NB DE MODES DU CONCEPT MODE_MECA
C    IN : NU   / NUME_DDL (K14)
C              / PROF_CHNO (K19)
C    OUT :BMODAL          VECTEUR CONTENANT LES MODES
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       IDDEEQ, I, IRET, IADMOD ,IBID
      INTEGER VALI
      CHARACTER*16  CHAMP
      CHARACTER*19 NU19
      CHARACTER*24  NOMCHA, DEEQ, VALK(2)
C-----------------------------------------------------------------------
C
C
      CALL JEMARQ()
C
      CHAMP = CHAMZ

      NU19=NU
      IF (NU19(15:19).NE.' ') THEN
        DEEQ = NU19//'.DEEQ'
      ELSE
        DEEQ = NU19(1:14)//'.NUME.DEEQ'
      ENDIF
      CALL JEVEUO ( DEEQ, 'L', IDDEEQ )
C
      DO 10 I = 1 , NBMODE
         CALL RSEXCH ( BASEMO, CHAMP, I, NOMCHA, IRET )
         IF ( IRET .NE. 0 ) THEN
            VALK (1) = CHAMP
            VALK (2) = BASEMO
            VALI = I
            CALL U2MESG('F', 'ALGORITH12_66',2,VALK,1,VALI,0,0.D0)
         ENDIF
         CALL JEEXIN (NOMCHA(1:19)//'.VALE' , IBID)
         IF (IBID.GT.0) THEN
           NOMCHA = NOMCHA(1:19)//'.VALE'
         ELSE
           NOMCHA = NOMCHA(1:19)//'.CELV'
         END IF

         CALL JEVEUO ( NOMCHA, 'L', IADMOD )

         CALL DCOPY ( NEQ, ZR(IADMOD), 1, BMODAL((I-1)*NEQ+1), 1 )
         IF ( CHAMP .EQ. 'DEPL' ) THEN
            CALL ZERLAG ( BMODAL((I-1)*NEQ+1), NEQ, ZI(IDDEEQ) )
         ENDIF
   10 CONTINUE
C
      CALL JEDEMA()
      END
