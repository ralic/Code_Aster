      SUBROUTINE RCMATE ( CHMAT, NOMAIL, NOMODE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8         CHMAT, NOMAIL, NOMODE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C  IN : CHMAT  : CHAMP MATERIAU PRODUIT
C  IN : NOMAIL : NOM DU MAILLAGE
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
      INTEGER IBID, NOCC, I, NM, NT, JNCMP, JVALV, NBMA,JMAIL,IER,NBCMP
      CHARACTER*4   OUI ,NOCP
      CHARACTER*8   K8B, NOMMAT, TYPMCL(2)
      CHARACTER*16  MOTCLE(2)
      CHARACTER*24  CHAMAT, MESMAI
      INTEGER      IARG
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CHAMAT = CHMAT//'.CHAMP_MAT'
C
      CALL ALCART ( 'G', CHAMAT, NOMAIL, 'NEUT_F')
      CALL JEVEUO ( CHAMAT(1:19)//'.NCMP', 'E', JNCMP )
      CALL JEVEUO ( CHAMAT(1:19)//'.VALV', 'E', JVALV )
      NOCP = 'X'
C
      CALL DISMOI('F','NB_CMP_MAX','NEUT_F','GRANDEUR',NBCMP,K8B,IER)
C
      DO 5 I=1,NBCMP
        CALL CODENT(I,'G',NOCP(2:4))
        ZK8(JNCMP+I-1) = NOCP
 5    CONTINUE
C
      CALL GETFAC ( 'AFFE' , NOCC )
C
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
      MESMAI = '&&RCMATE.MES_MAILLES'
C
      DO 10 I = 1 , NOCC
         CALL GETVID ( 'AFFE', 'MATER' , I,IARG,1,  NOMMAT, NM )
         IF (NM .LT. -1) NM = -NM
         CALL ASSERT(NM.LE.NBCMP)
         CALL GETVID ( 'AFFE', 'MATER' , I,IARG,NM, ZK8(JVALV), NM )
         CALL GETVTX ( 'AFFE', 'TOUT'  , I,IARG,1, OUI   , NT )
         IF ( NT .NE. 0 ) THEN
            CALL NOCART ( CHAMAT, 1, K8B, K8B, 0, K8B, IBID, ' ', NM )
         ELSE
            CALL RELIEM(NOMODE,NOMAIL,'NU_MAILLE','AFFE',I,2,MOTCLE(1),
     &                                      TYPMCL(1), MESMAI, NBMA )
            IF ( NBMA .NE. 0 ) THEN
               CALL JEVEUO ( MESMAI, 'L', JMAIL )
               CALL NOCART ( CHAMAT, 3, K8B, 'NUM', NBMA, K8B,
     &                                              ZI(JMAIL), ' ', NM )
               CALL JEDETR ( MESMAI )
            ENDIF
         ENDIF
 10   CONTINUE

      CALL JEDETR ( CHAMAT(1:19)//'.VALV' )
      CALL JEDETR ( CHAMAT(1:19)//'.NCMP' )

      CALL JEDEMA()
      END
