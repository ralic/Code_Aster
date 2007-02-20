      SUBROUTINE  COMPMA (MAILLA,NBGR,NOMGR,NBTO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C***********************************************************************
C    P. RICHARD     DATE 17/01/92
C-----------------------------------------------------------------------
C  BUT: COMPTAGE DU NOMBRE DE MAILLES CORRESPONDANTS A UNE LISTE DE
C     GROUPEMA
C
C        NOTA BENE: LES MAILLES PEUVENT APPARAITRE PLUSIEURS FOIS
C
C-----------------------------------------------------------------------
C
C MAILLA /I/: NOM UTILISATEUR DU MAILLAGE
C NBGR     /I/: NOMBRE DE GROUPES DE MAILLES
C NOMGR    /I/: NOMS DES GROUPES DE MAILLE
C NBTO     /O/: NOMBRE DE MAILLES
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
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
C
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8 MAILLA,NOMGR(NBGR),NOMCOU
      CHARACTER*24 VALK(2)
      CHARACTER*32 JEXNOM
      CHARACTER*1 K1BID
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      IF(NBGR.EQ.0) THEN
        NBTO=0
        GOTO 9999
      ENDIF
C
C-------RECUPERATION DES POINTEURS DE GROU_MA---------------------------
C
      CALL JEEXIN(MAILLA//'.GROUPEMA',IER)
      IF(IER.EQ.0) THEN
          VALK (1) = MAILLA
        CALL U2MESG('E', 'ALGORITH12_55',1,VALK,0,0,0,0.D0)
      ENDIF
C
C-------COMPTAGE DES MAILLES DEFINIS PAR GROUPES------------------------
C
      NBTO=0
C
      DO 10 I=1,NBGR
        NOMCOU=NOMGR(I)
        CALL JENONU(JEXNOM(MAILLA//'.GROUPEMA',NOMCOU),NUM)
C
        IF(NUM.EQ.0) THEN
          VALK (1) = MAILLA
          VALK (2) = NOMCOU
          CALL U2MESG('E', 'ALGORITH12_56',2,VALK,0,0,0,0.D0)
        ENDIF
C
        CALL JELIRA(JEXNOM(MAILLA//'.GROUPEMA',NOMCOU),'LONMAX',
     +              NB,K1BID)
        NBTO=NBTO+NB
C
 10   CONTINUE
C
 9999 CONTINUE
      END
