      SUBROUTINE OP0011 ( IER )
      IMPLICIT  NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 01/04/2005   AUTEUR VABHHTS J.PELLET 
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

C======================================================================

C                       OPERATEUR NUME_DDL
C======================================================================
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C======================================================================
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      INTEGER      NLIMAT
      PARAMETER   (NLIMAT=100)
      INTEGER      IER,IFM,NBID,NBMAT,NIV,NBCHA,IACHA,JNSLV
      REAL*8       RTBLOC, JEVTBL
      CHARACTER*2  BASE,BASSLV
      CHARACTER*8  K8B, TLIMAT(NLIMAT), NUUTI, RENUM, MO, METHOD
      CHARACTER*9  STOCKA
      CHARACTER*14 NUDEV
      CHARACTER*16 TYPE,OPER
      CHARACTER*19 CH19, SOLVEU
      CHARACTER*24 CHARGE
C----------------------------------------------------------------------
      CALL INFMAJ

      CALL INFNIV(IFM,NIV)
C----------------------------------------------------------------------
      CHARGE = '&&OP0011.CHARGES   .LCHA'
      BASE ='GG'

C --- RECUPERATION DU CONCEPT RESULTAT ET DE SON NOM UTILISATEUR :
C     ----------------------------------------------------------
      CALL GETRES(NUUTI,TYPE,OPER)
      NUDEV = NUUTI

      CALL GETVTX ( ' ', 'METHODE', 0,1,1, METHOD, NBID )
      CALL GETVTX ( ' ', 'RENUM'  , 0,1,1, RENUM,  NBID )

C --- DETERMINATION DU NOM DU STOCKAGE DE LA MATRICE :
C     ----------------------------------------------
      IF (    METHOD.EQ.'MULT_FRO' .OR. METHOD.EQ.'GCPC'
     &   .OR. METHOD.EQ.'MUMPS') THEN
        STOCKA = 'MORSE'
      ELSE IF (METHOD.EQ.'LDLT') THEN
        STOCKA = 'LIGN_CIEL'
      ELSE
        CALL ASSERT(.FALSE.)
      END IF


C     -- CREATION D'UNE SD SOLVEUR :
C     --------------------------------
      IF (METHOD.EQ.'MUMPS') THEN
         SOLVEU=NUUTI//'.SOLVEUR'
         BASSLV='G'
      ELSE
         SOLVEU = '&&OP0011.SOLVEUR'
         BASSLV='V'
      END IF
      CALL CRSOLV ( METHOD, RENUM, SOLVEU, BASSLV )


C --- TRAITEMENT DU MOT CLE MATR_RIGI OU MODELE :
C     -----------------------------------------
      CALL GETVID ( ' ', 'MATR_RIGI', 0,1,0, K8B, NBMAT )
C
      IF ( NBMAT .EQ. 0 ) THEN
         CALL GETVID ( ' ', 'MODELE', 1,1,1, MO, NBID )
         CALL GETVID ( ' ', 'CHARGE', 1,1,0, K8B,NBCHA)
         NBCHA = -NBCHA
         IF (NBCHA.NE.0) THEN
            CALL WKVECT ( CHARGE, 'V V K24', NBCHA, IACHA )
            CALL GETVID (' ', 'CHARGE', 1,1,NBCHA, ZK24(IACHA), NBID)
         ENDIF
         CALL NUMERO (' ',MO, CHARGE(1:19), SOLVEU, BASE, NUDEV )
         CALL JEDETR ( CHARGE )
         GOTO 20
      ENDIF

      NBMAT = -NBMAT
      CALL GETVID ( ' ', 'MATR_RIGI', 0,1,NBMAT, TLIMAT, NBMAT )

C --- CALCUL DE LA NUMEROTATION PROPREMENT DITE :
C     -----------------------------------------
      CALL NUMDDL ( NUDEV, 'G', NBMAT, TLIMAT, RENUM, STOCKA )

C --- CREATION ET CALCUL DU DESCRIPTEUR DU STOCKAGE DE LA MATRICE :
C     -----------------------------------------------------------
      IF (   METHOD.EQ.'MULT_FRO' .OR. METHOD.EQ.'GCPC'
     &  .OR. METHOD.EQ.'MUMPS') THEN
         CALL PROMOR ( NUDEV, 'S', 'G' )
      ELSE IF (METHOD.EQ.'LDLT') THEN
         RTBLOC=JEVTBL()
         CALL PROLCI ( NUDEV, RTBLOC, 'S', 'G' )
      END IF

C --- RENUMEROTATION MINIMUM DEGRE POUR LA MULTIFRONTALE
C --- ET CALCUL DES POINTEURS :
C     -----------------------
      IF (METHOD.EQ.'MULT_FRO' ) CALL MLTPRE ( NUDEV, 'G', RENUM )



C --- CREATION DE L'OBJET .NSLV :
C     -------------------------------------
      IF (METHOD.EQ.'MUMPS' ) THEN
         CALL WKVECT(NUDEV//'.NSLV','G V K24',1,JNSLV)
         ZK24(JNSLV-1+1)=SOLVEU
      END IF

 20   CONTINUE

C --- MENAGE :
C     ------
      CH19 = NUDEV
      CALL JEDETR(CH19(1:14)//'.NEWN')
      CALL JEDETR(CH19(1:14)//'.OLDN')
      CALL JEDETR(CH19//'.ADNE')
      CALL JEDETR(CH19//'.ADLI')
      END
