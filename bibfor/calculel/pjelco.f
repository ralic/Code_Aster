      SUBROUTINE PJELCO (MOA1,MOA2,CHAM1,CORRES,BASE)
      IMPLICIT   NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 24/10/2011   AUTEUR PELLET J.PELLET 
C RESPONSABLE PELLET J.PELLET
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     COMMANDE:  PROJ_CHAMP  METHODE:'ECLA_PG'
C BUT : CALCULER LA STRUCTURE DE DONNEE CORRESP_2_MAILLA
C       DANS LE CAS OU IL Y A UN CHAM_ELGA A TRAITER (CHAM1)
C ----------------------------------------------------------------------
      CHARACTER*8 MOA1,MOA2
      CHARACTER*16 CORRES
      CHARACTER*19 CHAM1
      CHARACTER*1 BASE
      CHARACTER*8 MA1P,MA2P
C     ----------------------------------------------

      CALL ASSERT(BASE.EQ.'V')


C     CREATION DU MAILLAGE 1 PRIME (MA1P)
C     REMPLISSAGE DU .PJEF_MP DANS LA SD CORRES
C     QUI EST LE NOM DU MAILLAGE 1 PRIME
C     ----------------------------------------------
      MA1P='&&PJELC1'
      CALL PJMA1P(MOA1,MA1P,CHAM1,CORRES)
      CALL CARGEO(MA1P)


C     CREATION DU MAILLAGE 2 PRIME (MA2P)
C     REMPLISSAGE DU .PJEF_EL DANS LA SD CORRES
C     QUI EST UN TABLEAU REFERENCANT, POUR CHAQUE ELGA,
C     SON NUMERO ET LE NUMERO DE LA MAILLE A LAQUELLE IL APPARTIENT
C     ----------------------------------------------
      MA2P='&&PJELC2'
      CALL PJMA2P(MOA2,MA2P,CORRES)

C     -- APPEL A LA ROUTINE "USUELLE" PJEFCO
C        AVEC LES DEUX MAILLAGES PRIME
C     ----------------------------------------------
      CALL PJEFCO(MA1P,MA2P,CORRES,'V')

      END
