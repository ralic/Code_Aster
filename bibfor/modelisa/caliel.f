      SUBROUTINE CALIEL (FONREZ, CHARGZ)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/07/2010   AUTEUR FLEJOU J-L.FLEJOU 
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
      CHARACTER*(*) CHARGZ, FONREZ
C -------------------------------------------------------
C     MODELISATION DU RACCORD ENTRE DES ELEMENTS
C     AYANT DES MODELISATIONS DIFFERENTES PAR DES RELATIONS
C     LINEAIRES ENTRE DDLS.
C     CES RELATIONS SONT AFFECTEES A LA CHARGE CHARGZ.
C     TYPES DES RACCORDS TRAITES :
C       1) RACCORD POUTRE-3D PAR DES RELATIONS LINEAIRES
C          ENTRE LES NOEUDS DES MAILLES DE SURFACE MODELISANT
C          LA TRACE DE LA SECTION DE LA POUTRE SUR LE MASSIF 3D
C          ET LE NOEUD DE LA POUTRE DONNE PAR L'UTILISATEUR
C
C       2) RACCORD POUTRE-COQUE PAR DES RELATIONS LINEAIRES
C          ENTRE LES NOEUDS DES MAILLES DE BORD DE COQUE MODELISANT
C          LA TRACE DE LA SECTION DE LA POUTRE SUR A COQUE
C          ET LE NOEUD DE LA POUTRE DONNE PAR L'UTILISATEUR
C -------------------------------------------------------
C  FONREZ        - IN    - K4   - : 'REEL' OU 'FONC'
C  CHARGZ        - IN    - K8   - : NOM DE LA SD CHARGE
C                - JXVAR -      -
C -------------------------------------------------------
C
C -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       19/03/04 (OB): PAR ADHERENCE A NUEFFE
C--------------------------------------------------------------------

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      COMMON  / KVARJE /ZK8(1),ZK16(1),ZK24(1),ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ------
C
C --------- VARIABLES LOCALES ---------------------------
C
      CHARACTER*8   MOD, K8BID, CHARGE, MO8BLA
      CHARACTER*14  NUMDDL
      CHARACTER*16  MOTFAC, OPTION
      CHARACTER*19  LIGRMO, LISREL,K19B
      INTEGER       IOCC,NLIAI,ILMOCH,IOP,IBID,IER
C
C --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
C
      CALL JEMARQ()
      CHARGE = CHARGZ
      MOTFAC = 'LIAISON_ELEM'
      MO8BLA = '        '
      K19B   = ' '
C
      CALL GETFAC(MOTFAC,NLIAI)
      IF (NLIAI.EQ.0) GOTO 9999
C
C --- NOM DE LA LISTE DE RELATIONS
C
      LISREL = '&&CALIEL.RLLISTE'
C
C --- MODELE ASSOCIE AU LIGREL DE CHARGE
C     ----------------------------------
      CALL DISMOI('F','NOM_MODELE',CHARGE(1:8),'CHARGE',IBID,MOD,IER)
C
C ---  LIGREL DU MODELE
C
      LIGRMO = MOD(1:8)//'.MODELE'
C
C --- CREATION SUR LA VOLATILE DU NUMEDDL ASSOCIE AU LIGREL
C --- DU MODELE
C     -----------------------------------------------------
      CALL WKVECT('&&CALIEL.LIGRMO','V V K24',1,ILMOCH)
      ZK24(ILMOCH) = LIGRMO
      NUMDDL = '&&CALIEL.NUMED'
      CALL NUEFFE('&&CALIEL.LIGRMO','VV',NUMDDL,'SANS',
     +             MO8BLA,K19B,IBID)
C
      DO 10 IOCC = 1, NLIAI
         CALL GETVTX(MOTFAC,'OPTION'     ,IOCC,1,1,OPTION,IOP)
         IF     (OPTION.EQ.'3D_POU') THEN
             CALL RAPO3D(NUMDDL, IOCC, FONREZ, LISREL, CHARGZ)
         ELSEIF (OPTION.EQ.'3D_TUYAU') THEN
             CALL RAPO3D(NUMDDL, IOCC, FONREZ, LISREL, CHARGZ)
         ELSEIF (OPTION.EQ.'PLAQ_POUT_ORTH') THEN
             CALL RAPO3D(NUMDDL, IOCC, FONREZ, LISREL, CHARGZ)
         ELSEIF (OPTION.EQ.'COQ_POU') THEN
             CALL RAPOCO(NUMDDL, IOCC, FONREZ, LISREL, CHARGZ)
         ELSEIF (OPTION.EQ.'COQ_TUYAU') THEN
             CALL RAPOCO(NUMDDL, IOCC, FONREZ, LISREL, CHARGZ)
         ENDIF
 10   CONTINUE
C
C     -- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
C     ---------------------------------------------
      CALL AFLRCH(LISREL,CHARGE)
C
      CALL JEDETC('V','&&CALIEL',1)
C
 9999 CONTINUE
      CALL JEDEMA()
      END
