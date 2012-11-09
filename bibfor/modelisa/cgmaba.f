      SUBROUTINE CGMABA (MOFAZ, IOCC, NOMAZ, LISMAZ, NBMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C.======================================================================
      IMPLICIT NONE
C
C       CGMABA -- TRAITEMENT DE L'OPTION BANDE
C                 DU MOT FACTEUR CREA_GROUP_MA DE
C                 LA COMMANDE DEFI_GROUP
C
C      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_MA CONSTITUE
C      DE TOUTES LES MAILLES DONT UN NOEUD AU MOINS APPARTIENT
C      A UNE BANDE DEFINIE PAR SON PLAN MILIEU ET LA DISTANCE
C      DES PLANS SUPERIEUR ET INFERIEUR A CE PLAN MILIEU.
C      LE PLAN MILIEU EST DEFINI PAR UN POINT APPARTENANT A CE
C      PLAN ET UN VECTEUR QUI LUI EST PERPENDICULAIRE.
C
C -------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES DONT UN
C                                   NOEUD AU MOINS APPARTIENT A LA
C                                   BANDE DEFINIE PAR L'UTILISATEUR
C  NBMA          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
C -------------------------------------------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
C
C -----  ARGUMENTS
      CHARACTER*(*) MOFAZ, NOMAZ, LISMAZ
C
C --------- VARIABLES LOCALES ---------------------------
      INTEGER VALI(2)
      CHARACTER*1    K1BID
      CHARACTER*8    NOMA, K8BID, NOMAIL
      CHARACTER*16   MOTFAC, MOCLE(3)
      CHARACTER*24   LISMAI
      CHARACTER*24   VALK
      CHARACTER*16   SELEC
C
      REAL*8         X0(3), X(3), XX0(3), VECNOR(3), ANGLE(2)
      INTEGER      IARG
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C-----------------------------------------------------------------------
      INTEGER IBID ,IDCOOR ,IDLIMA ,IDNOEU ,IER ,IMA ,INO
      INTEGER IOCC ,IRET ,NANGLE ,NB ,NBMA ,NBMAI ,NBNO
      INTEGER NBNOD ,NDIM ,NDIM1 ,NDIST ,NUMNOE ,NV ,NVECT

      REAL*8 D ,DIST ,R8BID ,R8DGRD ,XNORM ,XNORM2 ,ZERO

C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      MOTFAC    = MOFAZ
      NOMA      = NOMAZ
      LISMAI    = LISMAZ
C
      ZERO      = 0.0D0
C
      X0(1)     = ZERO
      X0(2)     = ZERO
      X0(3)     = ZERO
C
      X(1)      = ZERO
      X(2)      = ZERO
      X(3)      = ZERO
C
      XX0(1)    = ZERO
      XX0(2)    = ZERO
      XX0(3)    = ZERO
C
      VECNOR(1) = ZERO
      VECNOR(2) = ZERO
      VECNOR(3) = ZERO
C
C
      NBMA   = 0

C
C --- RECUPERATION DU TYPE DE VERIFICATION A APPLIQUER :
C     --------------------------------------------------
      CALL GETVTX(MOTFAC,'CRIT_NOEUD',IOCC,IARG,1,SELEC,IBID)
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
C --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
C     --------------------------------------------------
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',IDCOOR)
C
C --- RECUPERATION DU POINT SITUE SUR LE PLAN MILIEU :
C     ----------------------------------------------
      MOCLE(1) = 'POINT'
      MOCLE(2) = 'NOEUD_CENTRE'
      MOCLE(3) = 'GROUP_NO_CENTRE'
      CALL UTCONO ( MOTFAC, MOCLE, IOCC, NOMA, NDIM, X0, IRET )
C
C --- RECUPERATION DE LA DEMI-LARGEUR DE LA BANDE :
C     -------------------------------------------
      CALL GETVR8(MOTFAC,'DIST',IOCC,IARG,0,DIST,NDIST)
      IF (NDIST.EQ.0) THEN
          CALL U2MESS('F','MODELISA3_67')
      ELSE
         CALL GETVR8(MOTFAC,'DIST',IOCC,IARG,1,DIST,NB)
         IF (DIST.LE.ZERO) THEN
             CALL U2MESS('F','MODELISA3_68')
         ENDIF
      ENDIF
C
C --- RECUPERATION DE LA DIRECTION PERPENDICULAIRE AU PLAN MILIEU
C --- DE LA BANDE :
C     -----------
      CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,IARG,0,R8BID,NANGLE)
      IF (NANGLE.EQ.0) THEN
          CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,IARG,0,R8BID,NVECT)
          IF (NVECT.EQ.0) THEN
              CALL U2MESS('F','MODELISA3_69')
          ELSE
              NVECT = -NVECT
              IF (NDIM.EQ.3.AND.NVECT.NE.3) THEN
                  CALL U2MESS('F','MODELISA3_70')
              ELSEIF (NDIM.EQ.2.AND.NVECT.NE.2) THEN
                  CALL U2MESS('F','MODELISA3_71')
              ELSE
                  CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,IARG,NVECT,
     &                        VECNOR,
     &                        NV)
              ENDIF
          ENDIF
      ELSE
          NANGLE = -NANGLE
          NDIM1  =  NDIM - 1
          NANGLE =  MIN (NANGLE,NDIM1)
          CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,IARG,NANGLE,ANGLE,NV)
         IF ( ABS(NV) .NE. NDIM1 ) THEN
           VALK = MOTFAC
           VALI (1) = IOCC
           CALL U2MESG('F+','MODELISA9_32',1,VALK,1,VALI,0,0.D0)
           IF ( NDIM .EQ. 2 ) THEN
             CALL U2MESS('F+','MODELISA9_24')
           ELSE
             CALL U2MESS('F+','MODELISA9_25')
           ENDIF
           VALI (1) = ABS(NV)
           VALI (2) = NDIM1
           CALL U2MESG('F','MODELISA9_35',0,' ',2,VALI,0,0.D0)
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
          CALL U2MESS('F','MODELISA3_72')
      ENDIF
C
      XNORM = SQRT(XNORM2)
C
      VECNOR(1) = VECNOR(1)/XNORM
      VECNOR(2) = VECNOR(2)/XNORM
      VECNOR(3) = VECNOR(3)/XNORM
C
C --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
C     ---------------------------------------------
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMAI,K8BID,IER)
C
C --- ALLOCATION DU VECTEUR DES NOMS DES MAILLES APPARTENANT AU
C --- CYLINDRE :
C     --------
      CALL WKVECT(LISMAI,'V V I',NBMAI,IDLIMA)
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
C ---      COMPTE NOMBRE DES NOEUDS D'UN MAILLE DANS LE SPHERE :
C          ----------------------------------------------------
           NBNOD = 0
C
C ---     BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
C         -----------------------------------------
           DO 20 INO = 1, NBNO
C
C ---        NUMERO DU NOEUD :
C            ---------------
                NUMNOE = ZI(IDNOEU+INO-1)
C
C ---        COORDONNEES DU NOEUD :
C            --------------------
                X(1)   =  ZR(IDCOOR-1+3*(NUMNOE-1)+1)
                X(2)   =  ZR(IDCOOR-1+3*(NUMNOE-1)+2)
                X(3)   =  ZR(IDCOOR-1+3*(NUMNOE-1)+3)
C
                XX0(1) = X(1) - X0(1)
                XX0(2) = X(2) - X0(2)
                XX0(3) = X(3) - X0(3)
C
C ---        CALCUL DE LA DISTANCE DU NOEUD COURANT AU PLAN MILIEU :
C            -----------------------------------------------------
                D    = XX0(1)*VECNOR(1) + XX0(2)*VECNOR(2) +
     &                 XX0(3)*VECNOR(3)
C
C ---      SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A AU MOINS UN NOEUD
C          -------------------------------------------------------------
                IF (SELEC.EQ.'AU_MOINS_UN') THEN
C
C ---            SI LE NOEUD COURANT EST DANS LA BANDE, ON AFFECTE
C ---            LA MAILLE COURANTE A LA LISTE DE MAILLES QUI SERA
C ---            AFFECTEE AU GROUP_MA :
C                --------------------
                  IF (ABS(D).LE.DIST) THEN
                     NBMA = NBMA + 1
                     ZI(IDLIMA+NBMA-1) = IMA
                     GOTO 10
                  ENDIF
C
                ELSE IF ((SELEC.EQ.'TOUS').OR.(SELEC.EQ.'MAJORITE'))THEN
C ---            SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A TOUT OU
C ---            MAJORITE, COMPTER LES NOMBRES DES NOEUDS D'UNE MAILLE
C ---            DANS LA BANDE:
C                -------------------------------------------------
                  IF (ABS(D).LE.DIST) THEN
                    NBNOD=NBNOD+1
                  ENDIF
                ENDIF
C
 20        CONTINUE
C
           IF(SELEC.EQ.'TOUS') THEN
             IF(NBNOD.EQ.NBNO) THEN
               NBMA = NBMA + 1
               ZI(IDLIMA+NBMA-1) = IMA
               GOTO 10
             ENDIF
           ENDIF
C
           IF (SELEC.EQ.'MAJORITE') THEN
             IF(NBNOD.GE.(NBNO+1)/2) THEN
               NBMA = NBMA + 1
               ZI(IDLIMA+NBMA-1) = IMA
               GOTO 10
             ENDIF
           ENDIF
C
 10   CONTINUE
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
