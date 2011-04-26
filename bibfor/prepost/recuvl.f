      SUBROUTINE RECUVL ( NBVAL, TBINST, NBVAL2, TBINTH, NOREV, TBSCRV,
     +                    NOMDB, TBSCMB )
C
      IMPLICIT      NONE
      INTEGER       NBVAL, NBVAL2, NOREV, NOMDB
      CHARACTER*19  TBINST, TBINTH, TBSCRV, TBSCMB
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C
C
C ======================================================================
C ======================================================================
C --- BUT : RECUPERATION DES TABLES MECANIQUES, THERMIQUE ET -----------
C ------- : DU GROUPE DE NOEUDS CONSIDERES -----------------------------
C ======================================================================
C OUT : NBVAL  : NOMBRE D'INSTANT DE CALCUL MECANIQUE ------------------
C --- : TBINST : VECTEUR DES INSTANTS DE CALCUL MECANIQUE --------------
C --- : NBVAL2 : NOMBRE D'INSTANT DE CALCUL THERMIQUE ------------------
C --- : TBINTH : VECTEUR DES INSTANTS DE CALCUL THERMIQUE --------------
C --- : NOREV  : NOMBRE DE NOEUDS COTE REVETEMENT ----------------------
C --- : TBSCRV : VECTEUR DES ABSCISSES CURVILIGNES COTE REVETEMENT -----
C --- : NOMDB  : NOMBRE DE NOEUDS COTE METAL DE BASE -------------------
C --- : TBSCMB : VECTEUR DES ABSCISSES CURVILIGNES COTE METAL DE BASE --
C ======================================================================
      INTEGER       IBID
      CHARACTER*8   MOTFAC, K8B, TABREV, TABMDB, TABTHR
C ======================================================================
      CALL JEMARQ()
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      MOTFAC = 'K1D'
C ======================================================================
C --- RECUPERATION DES TABLES ASSOCIEES A K1D POUR L'ITERATION COURANTE-
C ======================================================================
      CALL GETVID ( MOTFAC , 'TABL_MECA_REV', 1,1,1, TABREV , IBID )
      CALL GETVID ( MOTFAC , 'TABL_MECA_MDB', 1,1,1, TABMDB , IBID )
      CALL GETVID ( MOTFAC , 'TABL_THER'    , 1,1,1, TABTHR , IBID )
C ======================================================================
C --- RECUPERATION DES LISTES D'INSTANT --------------------------------
C ======================================================================
      CALL TBEXV1 ( TABREV, 'INST', TBINST, 'V', NBVAL , K8B )
      CALL TBEXV1 ( TABTHR, 'INST', TBINTH, 'V', NBVAL2, K8B )
C ======================================================================
C --- RECUPERATION DE LA LISTE DES ABSCISSES CURVILIGNES ---------------
C --- COTE REVETEMENT --------------------------------------------------
C ======================================================================
      CALL TBEXV1 ( TABREV, 'ABSC_CURV', TBSCRV, 'V', NOREV, K8B )
C ======================================================================
C --- RECUPERATION DE LA LISTE DES ABSCISSES CURVILIGNES ---------------
C --- COTE METAL DE BASE -----------------------------------------------
C ======================================================================
      CALL TBEXV1 ( TABMDB, 'ABSC_CURV', TBSCMB, 'V', NOMDB, K8B )
C ======================================================================
C --- DESTRUCTION DES TABLES INUTILES ----------------------------------
C ======================================================================
      CALL JEDETR ( TABREV )
      CALL JEDETR ( TABMDB )
      CALL JEDETR ( TABTHR )
C ======================================================================
      CALL JEDEMA()
C ======================================================================
      END
