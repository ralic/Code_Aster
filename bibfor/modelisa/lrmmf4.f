      SUBROUTINE LRMMF4 ( NBRFAM, CARAFA, NBNOEU, NBMAIL,
     &                    NOMGRO, NUMGRO, NUMENT,
     &                    GRPNOE, GRPMAI, NBGRNO, NBGRMA,
     &                    INFMED, NIVINF, IFM )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 07/12/2009   AUTEUR REZETTE C.REZETTE 
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
C RESPONSABLE GNICOLAS G.NICOLAS
C-----------------------------------------------------------------------
C     LECTURE DU MAILLAGE - FORMAT MED - LES FAMILLES - 4
C     -    -     -                 -         -          -
C-----------------------------------------------------------------------
C
C ENTREES :
C   NBRFAM : NOMBRE DE FAMILLES POUR CE MAILLAGE
C   CARAFA : CARACTERISTIQUES DE CHAQUE FAMILLE
C     CARAFA(1,I) = NOMBRE DE GROUPES
C     CARAFA(2,I) = NOMBRE D'ATTRIBUTS
C     CARAFA(3,I) = NOMBRE D'ENTITES
C   NBNOEU : NOMBRE DE NOEUDS DU MAILLAGE
C   NBMAIL : NOMBRE DE MAILLES DU MAILLAGE
C   NOMGRO : COLLECTION DES NOMS DES GROUPES A CREER
C   NUMGRO : COLLECTION DES NUMEROS DES GROUPES A CREER
C   NUMENT : COLLECTION DES NUMEROS DES ENTITES DANS LES GROUPES
C SORTIES :
C  GRPNOE : OBJETS DES GROUPES DE NOEUDS
C  GRPMAI : OBJETS DES GROUPES DE MAILLES
C  NBGRNO : NOMBRE DE GROUPES DE NOEUDS
C  NBGRMA : NOMBRE DE GROUPES DE MAILLES
C DIVERS
C   INFMED : NIVEAU DES INFORMATIONS SPECIFIQUES A MED A IMPRIMER
C   NIVINF : NIVEAU DES INFORMATIONS GENERALES
C   IFM    : UNITE LOGIQUE DU FICHIER DE MESSAGE
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      INTEGER NBRFAM, CARAFA(3,NBRFAM)
      INTEGER NBNOEU, NBMAIL, NBGRNO, NBGRMA
      INTEGER INFMED
      INTEGER IFM, NIVINF
C
      CHARACTER*24 GRPNOE, GRPMAI
      CHARACTER*(*) NOMGRO, NUMGRO, NUMENT
C
C 0.2. ==> COMMUNS
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'LRMMF4' )
C
      INTEGER CODRET
      INTEGER IAUX, JAUX
      INTEGER NBNUFA, NUMGRP, NBGROU
      INTEGER NGNMAX, NGMMAX
      INTEGER JNOGNO, JNOGMA, JLGGNO, JLGGMA
      INTEGER JFNOMG, JFNUMG
C
      CHARACTER*8 SAUX08
      CHARACTER*8 NOMGRP
      CHARACTER*24 NOMGNO, NOMGMA, LONGNO, LONGMA
      CHARACTER*32 NOMJ
C
C     ------------------------------------------------------------------
C
      IF ( NIVINF.GE.2 ) THEN
C
        WRITE (IFM,1001) NOMPRO
 1001 FORMAT( 60('-'),/,'DEBUT DU PROGRAMME ',A)
C
      ENDIF
C
C====
C 1. ALLOCATIONS
C====
C               12   345678   9012345678901234
      NOMGNO = '&&'//NOMPRO//'.NOM_GR_NOE     '
      NOMGMA = '&&'//NOMPRO//'.NOM_GR_MAI     '
      LONGNO = '&&'//NOMPRO//'.LON_GR_NOE     '
      LONGMA = '&&'//NOMPRO//'.LON_GR_MAI     '
C
      NGNMAX = NBNOEU
      CALL WKVECT ( NOMGNO, 'V V K8', NGNMAX, JNOGNO )
      CALL WKVECT ( LONGNO, 'V V I' , NGNMAX, JLGGNO )
      NGMMAX = NBMAIL
      CALL WKVECT ( NOMGMA, 'V V K8', NGMMAX, JNOGMA )
      CALL WKVECT ( LONGMA, 'V V I' , NGMMAX, JLGGMA )
C
      NBGRNO = 0
      NBGRMA = 0
C
C====
C 2. ON PASSE TOUTES LES FAMILLES EN REVUE
C====
C
      DO 20 , IAUX = 1 , NBRFAM
C
C     SI OBJET DE COLL INEXISTANT -> PAS DE GROUPE DANS CETTE FAMILLE
C
        NOMJ = JEXNUM(NOMGRO,IAUX)
        CALL JEEXIN(NOMJ,CODRET)
C
        IF ( CODRET.GT.0 ) THEN
C
          NBNUFA = CARAFA(3,IAUX)
          CALL JEVEUO ( NOMJ, 'L', JFNOMG)
          CALL JELIRA ( NOMJ, 'LONMAX', NBGROU, SAUX08 )
          CALL JEVEUO ( JEXNUM(NUMGRO,IAUX), 'L', JFNUMG )
C
          IF ( INFMED.GE.3 ) THEN
            WRITE (IFM,2001) IAUX, NBGROU, NBNUFA
          ENDIF
 2001 FORMAT(
     &  /,'. FAMILLE DE RANG ',I4,
     &  /,'... NOMBRE DE GROUPES  : ',I9,
     &  /,'... NOMBRE D''ENTITES   : ',I9)
C
          DO 21 , JAUX = 1 , NBGROU
C
            NUMGRP = ZI (JFNUMG+JAUX-1)
            NOMGRP = ZK8(JFNOMG+JAUX-1)
C
C             CEST UN GROUPE DE NOEUDS  (>0)
C
            IF     ( NUMGRP.GT.0 ) THEN
              CALL LRMNGR( NGNMAX, NBGRNO,  NUMGRP, NOMGRP,
     &                     JNOGNO, JLGGNO,
     &                     NBNUFA, NOMGNO, LONGNO)
C
C             CEST UN GROUPE DE MAILLES (<0)
C
            ELSEIF ( NUMGRP.LT.0 ) THEN
              CALL LRMNGR( NGMMAX, NBGRMA, -NUMGRP, NOMGRP,
     &                     JNOGMA, JLGGMA,
     &                     NBNUFA, NOMGMA, LONGMA)
C
            ENDIF
C
   21     CONTINUE
C
        ENDIF
C
   20 CONTINUE
C
C====
C 3. CREATION COLLECTIONS FINALES
C                    GROUPNO X -> NO I,NO J...
C                    GROUPMA Y -> MA I,MA J...
C====
C
      IF ( NBGRNO.NE.0 .OR. NBGRMA.NE.0 ) THEN
C
        CALL LRMGRP ( GRPNOE, NBGRNO, JNOGNO, JLGGNO,
     &                GRPMAI, NBGRMA, JNOGMA, JLGGMA,
     &                NOMGRO, NUMGRO, NUMENT, NBRFAM )
C
      ENDIF
C
C====
C 6. LA FIN
C====
C
      CALL JEDETR (NOMGNO)
      CALL JEDETR (NOMGMA)
      CALL JEDETR (LONGNO)
      CALL JEDETR (LONGMA)
C
      IF ( NIVINF.GE.2 ) THEN
C
        WRITE (IFM,6001) NOMPRO
 6001 FORMAT(/,'FIN DU PROGRAMME ',A,/,60('-'))
C
      ENDIF
C
      END
