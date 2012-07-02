      SUBROUTINE  COMPNO (MAILLA,NBGR,NOMGR,NBTO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
C
C***********************************************************************
C    P. RICHARD     DATE 13/07/90
C-----------------------------------------------------------------------
C  BUT: COMPTAGE DU NOMBRE DE NOEUDS CORRESPONDANTS A UNE LISTE DE
C     GROUPENO
C        NOTA BENE: LES NOEUDS PEUVENT APPARAITRE PLUSIEURS FOIS
C
C-----------------------------------------------------------------------
C
C MAILLA /I/: NOM UTILISATEUR DU MAILLAGE
C NBGR     /I/: NOMBRE DE GROUPES DE NOEUDS
C NOMGR    /I/: NOMS DES GROUPES DE NOEUDS
C NBTO     /O/: NOMBRE DE NOEUDS
C
C
C
C
      INCLUDE 'jeveux.h'
      CHARACTER*8 MAILLA,NOMGR(NBGR),NOMCOU
      CHARACTER*24 VALK(2)
      CHARACTER*1 K1BID
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IER ,NB ,NBGR ,NBTO ,NUM 
C-----------------------------------------------------------------------
      IF(NBGR.EQ.0) THEN
        NBTO=0
        GOTO 9999
      ENDIF
C
C-------RECUPERATION DES POINTEURS DE GROU_NO---------------------------
C
      CALL JEEXIN(MAILLA//'.GROUPENO',IER)
      IF(IER.EQ.0) THEN
          VALK (1) = MAILLA
        CALL U2MESG('F', 'ALGORITH12_57',1,VALK,0,0,0,0.D0)
      ENDIF
C
C-------COMPTAGE DES NOEUD DEFINIS PAR GROUPES--------------------------
C
      NBTO=0
C
      DO 10 I=1,NBGR
        NOMCOU=NOMGR(I)
        CALL JENONU(JEXNOM(MAILLA//'.GROUPENO',NOMCOU),NUM)
C
        IF(NUM.EQ.0) THEN
          VALK (1) = MAILLA
          VALK (2) = NOMCOU
          CALL U2MESG('F', 'ALGORITH12_58',2,VALK,0,0,0,0.D0)
        ENDIF
C
        CALL JELIRA(JEXNOM(MAILLA//'.GROUPENO',NOMCOU),'LONUTI',
     +              NB,K1BID)
        NBTO=NBTO+NB
C
 10   CONTINUE
C
 9999 CONTINUE
      END
