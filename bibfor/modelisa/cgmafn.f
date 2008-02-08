      SUBROUTINE CGMAFN (MOFAZ, IOCC, NOMAZ, LISMAZ, NBMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C TOLE CRP_6
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C       CGMAFN -- TRAITEMENT DE L'OPTION FACE_NORMALE
C                 DU MOT FACTEUR CREA_GROUP_MA DE
C                 LA COMMANDE DEFI_GROUP
C
C      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_MA CONSTITUE
C      DE TOUTES LES MAILLES 'SURFACIQUES' DONT LA NORMALE
C      CALCULEE A PARTIR DES 3 PREMIERS NOEUDS DE L'ELEMENT
C      PAR N = 12 X 13 EST PARALLELE AU VECTEUR DEFINI PAR
C      L'UTILISATEUR PAR LES MOTS CLES ANGL_NAUT OU VECT_NORMALE.
C      ON DIRA QUE LES 2 VECTEURS SONT PARALLELES SI L'ANGLE
C      FORME PAR CES 2 VECTEURS EST INFERIEUR A LA VALEUR
C      DONNEE PAR L'UTILISATEUR APRES LE MOT CLE ANGL_PREC.
C      LA VALEUR PAR DEFAUT DE CET ANGLE EST EGALE A 0.5 DEGRE.
C
C -------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES
C                                   SURFACIQUES DE NORMALE PARALLELE AU
C                                   VECTEUR DEFINI PAR L'UTILISATEUR
C  NBMA          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
C -------------------------------------------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
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
C -----  ARGUMENTS
      CHARACTER*(*) MOFAZ, NOMAZ, LISMAZ
C
C --------- VARIABLES LOCALES ---------------------------
      CHARACTER*1    K1BID
      CHARACTER*4    CDIM
      CHARACTER*8    NOMA, K8BID, NOMAIL, NOMTYP, OUINON
      CHARACTER*16   MOTFAC
      CHARACTER*24   LISMAI
      CHARACTER*24   VALK
C
      REAL*8         ANGLE(3), VECNOR(3), COOR(3,9)
      INTEGER        VALI(3)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      MOTFAC = MOFAZ
      NOMA   = NOMAZ
      LISMAI = LISMAZ
C
      ZERO     = 0.0D0
      UNDEMI   = 0.5D0
      UN       = 1.0D0
C
      ANGLE(1)  = ZERO
      ANGLE(2)  = ZERO
      ANGLE(3)  = ZERO
C
      VECNOR(1) = ZERO
      VECNOR(2) = ZERO
      VECNOR(3) = ZERO
C
      A         = ZERO
      B         = ZERO
      C         = ZERO
C
      EPS       = 100.0D0*R8PREM()
C
      NBMA      = 0
C
C --- RECUPERATION DE LA DIMENSION DU MAILLAGE :
C     ----------------------------------------
      CALL DISMOI('F','Z_CST',NOMA,'MAILLAGE',NDIM,K8BID,IER)
      IF ( K8BID(1:3) .EQ. 'OUI' ) THEN
         NDIM = 2
      ELSE
         NDIM = 3
      ENDIF
C
C --- RECUPERATION DE LA DIRECTION FOURNIE PAR L'UTILISATEUR
C --- ET COINCIDANT AVEC LA NORMALE DES ELEMENTS SURFACIQUES
C --- QUE L'ON SOUHAITE RECUPERER :
C     ---------------------------
      CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,1,0,R8BID,NANGLE)
      IF (NANGLE.EQ.0) THEN
          CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,1,0,R8BID,NVECT)
          IF (NVECT.EQ.0) THEN
              CALL U2MESS('F','MODELISA3_80')
          ELSE
              NVECT = -NVECT
              NVECT =  MIN (NVECT,NDIM)
              CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,1,NVECT,VECNOR,NV)
         IF ( ABS(NV) .NE. NDIM ) THEN
           VALK = MOTFAC
           VALI (1) = IOCC
           CALL U2MESG('F+','MODELISA9_36',1,VALK,1,VALI,0,0.D0)
           IF ( NDIM .EQ. 2 ) THEN
             CALL U2MESG('F+','MODELISA9_24',0,' ',0,0,0,0.D0)
           ELSE
             CALL U2MESG('F+','MODELISA9_25',0,' ',0,0,0,0.D0)
           ENDIF
           VALI (1) = ABS(NV)
           VALI (2) = NDIM
           VALK = 'VECT_NORMALE'
           CALL U2MESG('F','MODELISA9_39',1,VALK,2,VALI,0,0.D0)
         ENDIF
          ENDIF
      ELSE
          NANGLE = -NANGLE
          NDIM1  =  NDIM - 1
          NANGLE =  MIN (NANGLE,NDIM1)
          CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,1,NANGLE,ANGLE,NV)
         IF ( ABS(NV) .NE. NDIM1 ) THEN
           VALK = MOTFAC
           VALI (1) = IOCC
           CALL U2MESG('F+','MODELISA9_40',1,VALK,1,VALI,0,0.D0)
           IF ( NDIM .EQ. 2 ) THEN
             CALL U2MESG('F+','MODELISA9_24',0,' ',0,0,0,0.D0)
           ELSE
             CALL U2MESG('F+','MODELISA9_25',0,' ',0,0,0,0.D0)
           ENDIF
           VALI (1) = ABS(NV)
           VALI (2) = NDIM1
           VALK = 'ANGL_NAUT'
           CALL U2MESG('F','MODELISA9_43',1,VALK,2,VALI,0,0.D0)
         ENDIF
C
          IF (NDIM.EQ.2) THEN
              ANGLE(1) = ANGLE(1)*R8DGRD()
C
              VECNOR(1) =  COS(ANGLE(1))
              VECNOR(2) =  SIN(ANGLE(1))
              VECNOR(3) =  ZERO
          ELSEIF (NDIM.EQ.3) THEN
              ANGLE(1) = ANGLE(1)*R8DGRD()
              ANGLE(2) = ANGLE(2)*R8DGRD()
C
              VECNOR(1) =  COS(ANGLE(1))*COS(ANGLE(2))
              VECNOR(2) =  SIN(ANGLE(1))*COS(ANGLE(2))
              VECNOR(3) = -SIN(ANGLE(2))
          ENDIF
      ENDIF
C
      XNORM2 = VECNOR(1)*VECNOR(1) + VECNOR(2)*VECNOR(2) +
     &         VECNOR(3)*VECNOR(3)
C
      IF (XNORM2.EQ.ZERO) THEN
          CALL U2MESS('F','MODELISA3_81')
      ENDIF
C
      XNORM = SQRT(XNORM2)
C
      VECNOR(1) = VECNOR(1)/XNORM
      VECNOR(2) = VECNOR(2)/XNORM
      VECNOR(3) = VECNOR(3)/XNORM
C
C --- RECUPERATION DE L'ANGLE MAX TOLERE ENTRE LA DIRECTION
C --- FOURNIE PAR L'UTILISATEUR ET LA DIRECTION NORMALE A
C --- L'ELEMENT :
C     ---------
      CALL GETVR8(MOTFAC,'ANGL_PREC',IOCC,1,0,ANGPRE,NBANG)
      IF (NBANG.EQ.0) THEN
         ANGPRE = UNDEMI*R8DGRD()
      ELSE
         CALL GETVR8(MOTFAC,'ANGL_PREC',IOCC,1,1,ANGPRE,NB)
         ANGPRE = ANGPRE*R8DGRD()
      ENDIF
C
C --- ON REGARDE SI L'ON TIENT COMPTE OU NON DU FAIT QUE LA NORMALE
C --- FOURNIE PAR L'UTILISATEUR ET LA DIRECTION NORMALE A
C --- L'ELEMENT ONT LA MEME ORIENTATION :
C     ---------------------------------
      CALL GETVTX(MOTFAC,'VERI_SIGNE',IOCC,1,0,OUINON,NBOUI)
      IF (NBOUI.EQ.0) THEN
          OUINON = 'OUI'
      ELSE
         CALL GETVTX(MOTFAC,'VERI_SIGNE',IOCC,1,1,OUINON,NBO)
      ENDIF
C
C --- RECUPERATION DE LA DIMENSION DE L'ESPACE DES COORDONNEES :
C     --------------------------------------------------------
      CALL JELIRA(NOMA//'.COORDO    .VALE','DOCU',IBID,CDIM)
C
      IF (CDIM.EQ.'2   ') THEN
          NDIM=2
      ELSE IF (CDIM.EQ.'3   ') THEN
          NDIM=3
      END IF
C
C --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
C     ---------------------------------------------
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMAI,K8BID,IER)
C
C --- ALLOCATION DU VECTEUR DES NOMS DES MAILLES DE SURFACE DE
C --- NORMALE PARALLELE AU VECTEUR VECNOR :
C     -----------------------------------
      CALL WKVECT(LISMAI,'V V I',NBMAI,IDLIMA)
C
C --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
C     --------------------------------------------------
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',IDCOOR)
C
C --- PARCOURS DES MAILLES DU MAILLAGE :
C     --------------------------------
      DO 10 IMA = 1, NBMAI
C
C ---     RECUPERATION DU NOM DE LA MAILLE :
C         --------------------------------
           CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',IMA),NOMAIL)
C
C ---     RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C         -------------------------------------------
           CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
           CALL JEVEUO (JEXNUM(NOMA//'.CONNEX',IBID),'L',IDNOEU)
C
C ---     RECUPERATION DU NOMBRE DE CONNECTIVITES DE LA MAILLE :
C         ----------------------------------------------------
           CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
           CALL JELIRA (JEXNUM(NOMA//'.CONNEX',IBID),'LONMAX',NBNO,
     &                  K1BID)
C
C ---     RECUPERATION DU TYPE DE LA MAILLE :
C         ---------------------------------
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
            CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
            JTYP=IATYMA-1+IBID
            ITYP = ZI(JTYP)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYP),NOMTYP)
C
C ---     CAS DU 2D
C ---     LES MAILLES SURFACIQUES SONT DES SEG2 OU DES SEG3 :
C         -------------------------------------------------
            IF (NDIM.EQ.2.AND.NOMTYP(1:3).EQ.'SEG') THEN
                INO1 = ZI(IDNOEU+1-1)
                INO2 = ZI(IDNOEU+2-1)
C
                COOR(1,1)=ZR(IDCOOR-1+3*(INO1-1)+1)
                COOR(2,1)=ZR(IDCOOR-1+3*(INO1-1)+2)
                COOR(1,2)=ZR(IDCOOR-1+3*(INO2-1)+1)
                COOR(2,2)=ZR(IDCOOR-1+3*(INO2-1)+2)
C
C ---         CALCUL DES COMPOSANTES A ET B DU VECTEUR NORMAL
C ---         A L'ELEMENT :
C             -----------
                CALL CANOR2(COOR,A,B)
C
C ---     CAS DU 3D
C ---     LES MAILLES SURFACIQUES SONT DES TRIA3 OU DES TRIA6
C ---     OU DES TRIA9 OU DES QUAD4 OU DES QUAD8 :
C         --------------------------------------
            ELSEIF (NDIM.EQ.3.AND.
     &      (NOMTYP(1:4).EQ.'TRIA'.OR.NOMTYP(1:4).EQ.'QUAD')) THEN
                INO1 = ZI(IDNOEU+1-1)
                INO2 = ZI(IDNOEU+2-1)
                INO3 = ZI(IDNOEU+3-1)
C
                COOR(1,1)=ZR(IDCOOR-1+3*(INO1-1)+1)
                COOR(2,1)=ZR(IDCOOR-1+3*(INO1-1)+2)
                COOR(3,1)=ZR(IDCOOR-1+3*(INO1-1)+3)
C
                COOR(1,2)=ZR(IDCOOR-1+3*(INO2-1)+1)
                COOR(2,2)=ZR(IDCOOR-1+3*(INO2-1)+2)
                COOR(3,2)=ZR(IDCOOR-1+3*(INO2-1)+3)
C
                COOR(1,3)=ZR(IDCOOR-1+3*(INO3-1)+1)
                COOR(2,3)=ZR(IDCOOR-1+3*(INO3-1)+2)
                COOR(3,3)=ZR(IDCOOR-1+3*(INO3-1)+3)
C
C ---         CALCUL DES COMPOSANTES A, B ET C DU VECTEUR NORMAL
C ---         A L'ELEMENT :
C             -----------
                CALL CANOR3(COOR,A,B,C)
C
C ---     LA MAILLE N'EST PAS DU TYPE SOUHAITE :
C         ------------------------------------
            ELSE
                GOTO 10
            ENDIF
C
C ---     CALCUL DE L'ANGLE FORME PAR LE VECTEUR NORMAL A L'ELEMENT
C ---     ET LA DIRECTION FOURNIE PAR L'UTILISATEUR :
C         -----------------------------------------
            XNOREL = SQRT(A*A + B*B + C*C)
C
C ---     CAS OU L'ON TIENT COMPTE  DU FAIT QUE LA NORMALE FOURNIE
C ---     PAR L'UTILISATEUR ET LA DIRECTION NORMALE A L'ELEMENT
C ---     DOIVENT AVOIR LA MEME ORIENTATION :
C         ---------------------------------
            IF (OUINON(1:3).EQ.'OUI') THEN
                 PSCA = A*VECNOR(1) + B*VECNOR(2) + C*VECNOR(3)
                 IF (PSCA.LE.ZERO) GOTO 10
            ENDIF
C
            PSCA   = ABS(A*VECNOR(1) + B*VECNOR(2) + C*VECNOR(3))/XNOREL
            IF (PSCA.GT.UN) THEN
                PSCA   = PSCA - EPS
            ENDIF
            ANG    = ACOS(PSCA)
C
C ---       SI LE VECTEUR NORMAL A L'ELEMENT ET LA DIRECTION FOURNIE
C ---       PAR L'UTILISATEUR  SONT PARALLELES, ON AFFECTE LA MAILLE
C ---       COURANTE A LA LISTE DE MAILLES QUI SERA AFFECTEE AU
C ---       GROUP_MA :
C           --------
            IF (ABS(ANG).LT.ABS(ANGPRE)) THEN
                NBMA = NBMA + 1
                ZI(IDLIMA+NBMA-1) = IMA
            ENDIF
 10   CONTINUE
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
