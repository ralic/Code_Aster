      SUBROUTINE ORTREP (MATER, NDIM, COOR, REPERE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C ======================================================================
C.======================================================================
      IMPLICIT NONE
C
C      ORTREP   -- RECUPERATION DES DONNEES UTILISATEUR
C                  DEFINISSANT LE REPERE D'ORTHOTROPIE
C                  RELATIF A L'ELEMENT COURANT
C                  I.E. OU BIEN ON DONNE LES 3 ANGLES
C                       NAUTIQUES DEFINISSANT LE REPERE
C                       D'ORTHOTROPIE
C                       OU BIEN DANS LE CAS OU L'ELEMENT APPARTIENT
C                       A UNE STRUCTURE A SYMETRIE CYLINDRIQUE
C                       ON DONNE L'AXE DE SYMETRIE (LA DIRECTION
C                       ETANT DEFINIE PAR LES 2 PREMIERS ANGLES
C                       NAUTIQUES ET L'AXE DEFINI PAR CETTE DIRECTION
C                       ET UN POINT DONNE PAR L'UTILISATEUR)
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MATER          IN      I        MATERIAU
C    NDIM           IN      I        DIMENSION DE LA MODELISATION
C    COOR           IN      R        COORDONNEE DU POINT
C                                    (CAS CYLINDRIQUE)
C    REPERE(7)      OUT     R        VALEURS DEFINISSANT LE REPERE
C                                    D'ORTHOTROPIE
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           REAL*8       REPERE(7),COOR(3)
           INTEGER      MATER,NDIM

C -----  VARIABLES LOCALES
           INTEGER      NBRES,I,IRET,ICAMAS
           PARAMETER         ( NBRES=9 )
      INTEGER ICODRE(NBRES)
           CHARACTER*16 PHENOM
           REAL*8       P(3,3),XG(3),YG(3),ORIG(3),DIRE(3)
           REAL*8       ALPHA,BETA,R8DGRD,ANGMAS(3)
C.========================= DEBUT DECLARATIONS NORMALISEES  JEVEUX ====
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
C.========================= FIN DECLARATIONS NORMALISEES  JEVEUX ====
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C
C ---- INITIALISATIONS :
C      ----------------

      DO 10 I = 1, 7
         REPERE(I) = 0.0D0
 10   CONTINUE
C
      CALL TECACH('NNN','PCAMASS',1,ICAMAS,IRET)
C
      IF (IRET.NE.0) THEN
C     --------------------
         REPERE(1) = 1.D0
C
      ELSE
C     ----
C
C ---- TRAITEMENT DU CAS 3D :
C      ====================
        IF (NDIM.EQ.3) THEN
C
C ----   RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM
C        -------------------------------------------------
         CALL RCCOMA(MATER,'ELAS',PHENOM,ICODRE)
C
         IF (PHENOM.EQ.'ELAS_ORTH'.OR.PHENOM.EQ.'ELAS_ISTR') THEN
C
            CALL JEVECH('PCAMASS','L',ICAMAS)
C
            REPERE(1) = ZR(ICAMAS)
C
            IF (ZR(ICAMAS).GT.0.0D0) THEN
C
C ----      ANGLES NAUTIQUES
C           ----------------
                REPERE(2) = ZR(ICAMAS+1)*R8DGRD()
                REPERE(3) = ZR(ICAMAS+2)*R8DGRD()
                REPERE(4) = ZR(ICAMAS+3)*R8DGRD()
            ELSE
C
C-----      LES INFORMATIONS FOURNIES SONT POUR UN REPERE
C-----      CYLINDRIQUES. ON TRANSFORME DIRECTEMENT
C-----      EN REPERE LOCAL CARTESIEN
              ALPHA=ZR(ICAMAS+1)*R8DGRD()
              BETA =ZR(ICAMAS+2)*R8DGRD()
              DIRE(1) = COS(ALPHA)*COS(BETA)
              DIRE(2) = SIN(ALPHA)*COS(BETA)
              DIRE(3) = -SIN(BETA)
              ORIG(1)=ZR(ICAMAS+4)
              ORIG(2)=ZR(ICAMAS+5)
              ORIG(3)=ZR(ICAMAS+6)
              CALL UTRCYL(COOR,DIRE,ORIG,P)
              DO 1 I=1,3
                XG(I)=P(1,I)
                YG(I)=P(2,I)
    1         CONTINUE
              CALL ANGVXY(XG, YG, ANGMAS)
              REPERE(1)=1.D0
              REPERE(2)=ANGMAS(1)
              REPERE(3)=ANGMAS(2)
              REPERE(4)=ANGMAS(3)
           ENDIF
        ENDIF
C
C ---- TRAITEMENT DU CAS 2D :
C      ====================
        ELSEIF (NDIM.EQ.2) THEN
C
C ----   RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM
C        -------------------------------------------------
         CALL RCCOMA(MATER,'ELAS',PHENOM,ICODRE)
C
         IF (PHENOM.EQ.'ELAS_ORTH'.OR.PHENOM.EQ.'ELAS_ISTR') THEN
C
            CALL JEVECH('PCAMASS','L',ICAMAS)
C
            REPERE(1) = ZR(ICAMAS)
C
            IF (ZR(ICAMAS).GT.0.0D0) THEN
C
C ----      ANGLE NAUTIQUE
C           --------------
                REPERE(2) = ZR(ICAMAS+1)*R8DGRD()
C
           ELSE
              REPERE(1)=1.D0
              CALL U2MESS('F','ELEMENTS2_38')
           ENDIF
         ENDIF
        ENDIF
      ENDIF
C     -----
C
C.============================ FIN DE LA ROUTINE ======================
      END
