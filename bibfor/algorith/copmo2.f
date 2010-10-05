      SUBROUTINE COPMO2 ( BASEMO, NEQ, NU, NBMODE, BMODAL )
      IMPLICIT NONE
      INTEGER             NEQ, NBMODE
      REAL*8              BMODAL(NEQ*NBMODE)
      CHARACTER*8         BASEMO
      CHARACTER*14        NU
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/10/2010   AUTEUR PELLET J.PELLET 
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
C
C     FONCTION  :
C     RECOPIE DES MODES PROPRES CONTENU DANS LE CONCEPT MODE_MECA
C     DANS UN VECTEUR DE TRAVAIL AVEC NUMEROTATION DIFFERENTE
C
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C        NOM        MODE                    ROLE
C  ________________ ____ ______________________________________________
C                         VARIABLES DU SYSTEME DYNAMIQUE MODAL
C  ________________ ____ ______________________________________________
C    BASEMO         <--   NOM DU CONCEPT MODE_MECA
C    NEQ            <--   DIMENSION DU SYSTEME ASSEMBLE
C    NBMODE         <--   NB DE MODES DU CONCEPT MODE_MECA
C    BMODAL          -->  VECTEUR CONTENANT LES MODES
C-----------------------------------------------------------------------
C      ----DEBUT DES COMMUNS JEVEUX--------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C      ----FIN DES COMMUNS JEVEUX----------

      INTEGER       IBID, IDDEEQ, I, IRET, JREF, JVAL
      CHARACTER*8   NOMA, MAILLA, TYP1
      CHARACTER*14  NU2
      CHARACTER*24  CREFE(2), CHAMP, NOMCHA
      CHARACTER*24 VALK(4)
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JEVEUO ( NU//'.NUME.DEEQ', 'L', IDDEEQ )
      CALL DISMOI('F','NOM_MAILLA',NU,'NUME_DDL',IBID,MAILLA,IRET)
C
      DO 10 I = 1,NBMODE

         CALL RSEXCH ( BASEMO, 'DEPL', I, NOMCHA, IRET )

         CALL JEVEUO ( NOMCHA(1:19)//'.REFE', 'L', JREF )
         NOMA = ZK24(JREF)(1:8)
         NU2  = ZK24(JREF+1)(1:14)
         CALL JELIBE(NOMCHA(1:19)//'.REFE')

C ------ TEST COMPARANT NOMA A MAILLAGE(NU)
         IF (NOMA.NE.MAILLA) THEN
            VALK (1) = NU
            VALK (2) = MAILLA
            VALK (3) = NU2
            VALK (4) = NOMA
            CALL U2MESG('F', 'ALGORITH12_62',4,VALK,0,0,0,0.D0)
         ENDIF

         IF ( NU .NE. NU2 ) THEN
            CALL JELIRA ( NOMCHA(1:19)//'.VALE', 'TYPE'  , IBID, TYP1 )
            CREFE(1) = NOMA
            CREFE(2) = NU
            CREFE(2)(15:19)='.NUME'
            CHAMP = '&&COPMO2.CHAMP'
            CALL VTCREA ( CHAMP, CREFE, 'V', TYP1, NEQ )
            CALL VTCOPY ( NOMCHA, CHAMP)
            CALL JEVEUO ( CHAMP(1:19)//'.VALE', 'L', JVAL )
            CALL DCOPY ( NEQ, ZR(JVAL), 1, BMODAL((I-1)*NEQ+1), 1 )
            CALL DETRSD ( 'CHAM_NO', CHAMP )
         ELSE
            CALL JEVEUO ( NOMCHA(1:19)//'.VALE', 'L', JVAL )
            CALL DCOPY ( NEQ,ZR(JVAL), 1, BMODAL((I-1)*NEQ+1), 1 )
            CALL JELIBE ( NOMCHA(1:19)//'.VALE' )
         ENDIF

         CALL ZERLAG ( BMODAL((I-1)*NEQ+1), NEQ, ZI(IDDEEQ) )

   10 CONTINUE

      CALL JEDEMA()
      END
