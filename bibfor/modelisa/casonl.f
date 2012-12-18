      SUBROUTINE CASONL ( CHAR, LIGRMO, NOMA, NDIM )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================

      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      INTEGER           NDIM
      CHARACTER*8       CHAR, NOMA
      CHARACTER*(*)     LIGRMO

C ----------------------------------------------------------------------
C                   SOURCE THERMIQUE NON LINEAIRE
C
C-----------------------------------------------------------------------
C  STOCKAGE DES SOURCES DANS UNE CARTE ALLOUEE SUR LE LIGREL DU MODELE
C-----------------------------------------------------------------------
C      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
C      LIGRMO : NOM DU LIGREL DE MODELE
C      NOMA   : NOM DU MAILLAGE
C      NDIM   : DIMENSION DU PROBLEME (2D/3D) POUR FILTRER LES MAILLES
C-----------------------------------------------------------------------
      INTEGER      IBID, NSOUR, JVALV, JNCMP, N1, IER, NCMP,
     &             IOCC, NBTOU, NBMA, JMA
      CHARACTER*8  K8B, TYPMCL(2)
      CHARACTER*16 MOTCLF, MOTCLE(2)
      CHARACTER*19 CARTE
      CHARACTER*24 MESMAI
      INTEGER      IARG
C ----------------------------------------------------------------------
      DATA MESMAI /'&&CASONL.MES_MAILLES'/
      DATA MOTCLE /'GROUP_MA','MAILLE'/
      DATA TYPMCL /'GROUP_MA','MAILLE'/
C ----------------------------------------------------------------------
      CALL JEMARQ()

      MOTCLF = 'SOUR_NL'
      NCMP   = 1
      CALL GETFAC ( MOTCLF, NSOUR )

      CARTE = CHAR//'.CHTH.SOUNL'
      CALL ALCART ( 'G', CARTE, NOMA, 'SOUR_F' )
      CALL JEVEUO ( CARTE//'.NCMP', 'E', JNCMP )
      CALL JEVEUO ( CARTE//'.VALV', 'E', JVALV )
      ZK8(JNCMP) = 'SOUR'


C --- DEFAUT: STOCKAGE D'UNE SOURCE NULLE SUR TOUT LE MAILLAGE

      ZK8(JVALV) = '&FOZERO'
      CALL NOCART ( CARTE, 1, ' ', 'NOM', 0, ' ', 0, LIGRMO, NCMP)


C --- STOCKAGE DES FONCTIONS SOURCES DANS LA CARTE

      DO 10 IOCC = 1, NSOUR
        CALL GETVID ( MOTCLF, 'SOUR', IOCC,IARG,1, ZK8(JVALV), N1)

        CALL GETVTX ( MOTCLF, 'TOUT', IOCC,IARG, 1, K8B, NBTOU )
        IF ( NBTOU .NE. 0 ) THEN
          CALL NOCART (CARTE,1,' ','NOM',0,' ', 0,LIGRMO,NCMP)

        ELSE
          CALL RELIEM(LIGRMO, NOMA, 'NO_MAILLE', MOTCLF, IOCC, 2,
     &                               MOTCLE, TYPMCL, MESMAI, NBMA )
          IF (NBMA.EQ.0) GOTO 10

          CALL JEVEUO ( MESMAI, 'L', JMA )
          CALL VETYMA ( NOMA, ZK8(JMA),NBMA, K8B,0, MOTCLF,NDIM,IER)
          CALL NOCART (CARTE,3,K8B,'NOM',NBMA,ZK8(JMA),IBID,' ',NCMP)
          CALL JEDETR ( MESMAI )
        ENDIF
 10   CONTINUE

      CALL JEDEMA()
      END
