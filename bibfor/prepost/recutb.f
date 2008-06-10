      SUBROUTINE RECUTB ( IK1D, NOMGRN, TABREV, TABMDB, TABTHR )
C
      IMPLICIT      NONE
      INTEGER       IK1D, NBVAL, NOREV, NOMDB
      CHARACTER*8   TABREV, TABMDB, TABTHR
      CHARACTER*19  TBINST, TBSCRV, TBSCMB
      CHARACTER*32  NOMGRN
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 07/04/2003   AUTEUR DURAND C.DURAND 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : RECUPERATION DES TABLES MECANIQUES, THERMIQUE ET -----------
C ------- : DU GROUPE DE NOEUDS CONSIDERES -----------------------------
C ======================================================================
C IN  : IK1D   : NUMERO D'OCCURENCE ------------------------------------
C OUT : NOMGRN : NOM DU GROUPE DE NOEUDS -------------------------------
C --- : TABREV : TABLE MECANIQUE DU REVETEMENT -------------------------
C --- : TABMDB : TABLE MECANIQUE DU METAL DE BASE ----------------------
C --- : TABTHR : TABLE THERMIQUE ---------------------------------------
C ======================================================================
      INTEGER      IBID
      REAL*8       R8B
      CHARACTER*8  MOTFAC
C ======================================================================
      CALL JEMARQ()
C ======================================================================
C --- RECUPERATION DES TABLES ASSOCIEES A K1D POUR L'ITERATION COURANTE-
C ======================================================================
      MOTFAC = 'K1D'
      CALL GETVID ( MOTFAC , 'TABL_MECA_REV', IK1D,1,1, TABREV , IBID )
      CALL GETVID ( MOTFAC , 'TABL_MECA_MDB', IK1D,1,1, TABMDB , IBID )
      CALL GETVID ( MOTFAC , 'TABL_THER'    , IK1D,1,1, TABTHR , IBID )
      CALL GETVTX ( MOTFAC , 'INTITULE'     , IK1D,1,1, NOMGRN , IBID )
C ======================================================================
      CALL JEDEMA()
C ======================================================================
      END
