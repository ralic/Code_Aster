      SUBROUTINE CGMACY (MOFAZ, IOCC, NOMAZ, LISMAZ, NBMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_6
C.======================================================================
      IMPLICIT NONE
C
C       CGMACY -- TRAITEMENT DE L'OPTION CYLINDRE
C                 DU MOT FACTEUR CREA_GROUP_MA DE
C                 LA COMMANDE DEFI_GROUP
C
C      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_MA CONSTITUE
C      DE TOUTES LES MAILLES DONT UN NOEUD AU MOINS APPARTIENT
C      A CYLINDRE DONT L'AXE ET LE RAYON SONT DEFINIS PAR
C      L'UTILISATEUR.
C
C -------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES DONT UN
C                                   NOEUD AU MOINS APPARTIENT AU
C                                   CYLINDRE DEFINI PAR L'UTILISATEUR
C  NBMA          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
C -------------------------------------------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
      INCLUDE 'jeveux.h'
C
C -----  ARGUMENTS
      CHARACTER*(*) MOFAZ, NOMAZ, LISMAZ
C
C --------- VARIABLES LOCALES ---------------------------
      CHARACTER*1    K1BID
      CHARACTER*8    NOMA, K8BID, NOMAIL
      CHARACTER*16   MOTFAC, MOCLE(3)
      CHARACTER*24   LISMAI
      CHARACTER*16   SELEC

C
      REAL*8         X0(3), X(3), XX0(3), AXE(3), ANGLE(2)
      INTEGER      IARG
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C-----------------------------------------------------------------------
      INTEGER IBID ,IDCOOR ,IDLIMA ,IDNOEU ,IER ,IMA ,INO 
      INTEGER IOCC ,IRET ,NANGLE ,NB ,NBMA ,NBMAI ,NBNO 
      INTEGER NBNOD ,NDIM ,NRAYON ,NUMNOE ,NV ,NVECT 
      REAL*8 ANG ,D2 ,EPS ,PSCA ,R8BID ,R8DGRD ,R8PREM 
      REAL*8 RAYON ,UN ,XNORM ,XNORM2 ,XNOXX0 ,XNOXX2 ,ZERO 

C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      MOTFAC = MOFAZ
      NOMA   = NOMAZ
      LISMAI = LISMAZ
C
C --- RECUPERATION DU TYPE DE VERIFICATION A APPLIQUER :
C     --------------------------------------------------
      CALL GETVTX(MOTFAC,'CRIT_NOEUD',IOCC,IARG,1,SELEC,IBID)
C
      ZERO  = 0.0D0
      UN    = 1.0D0
C
      X0(1)  = ZERO
      X0(2)  = ZERO
      X0(3)  = ZERO
C
      X(1)   = ZERO
      X(2)   = ZERO
      X(3)   = ZERO
C
      XX0(1) = ZERO
      XX0(2) = ZERO
      XX0(3) = ZERO
C
      AXE(1) = ZERO
      AXE(2) = ZERO
      AXE(3) = ZERO
C
      RAYON  = ZERO
C
      EPS    = 100.0D0*R8PREM()
C
      NBMA   = 0
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
      IF (NDIM.NE.3) THEN
          CALL U2MESS('F','MODELISA3_73')
      ENDIF
C
C --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
C     --------------------------------------------------
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',IDCOOR)
C
C --- RECUPERATION DU POINT SITUE SUR L'AXE DU CYLINDRE :
C     -------------------------------------------------
      MOCLE(1) = 'POINT'
      MOCLE(2) = 'NOEUD_CENTRE'
      MOCLE(3) = 'GROUP_NO_CENTRE'
      CALL UTCONO ( MOTFAC, MOCLE, IOCC, NOMA, NDIM, X0, IRET )
C
C --- RECUPERATION DU RAYON DU CYLINDRE :
C     ---------------------------------
      CALL GETVR8(MOTFAC,'RAYON',IOCC,IARG,0,RAYON,NRAYON)
      IF (NRAYON.EQ.0) THEN
          CALL U2MESS('F','MODELISA3_74')
      ELSE
         CALL GETVR8(MOTFAC,'RAYON',IOCC,IARG,1,RAYON,NB)
         IF (RAYON.LE.ZERO) THEN
             CALL U2MESS('F','MODELISA3_75')
         ENDIF
      ENDIF
C
C --- RECUPERATION DE LA DIRECTION DEFINISSANT L'AXE DU CYLINDRE :
C     ----------------------------------------------------------
      CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,IARG,0,R8BID,NANGLE)
      IF (NANGLE.EQ.0) THEN
          CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,IARG,0,R8BID,NVECT)
          IF (NVECT.EQ.0) THEN
              CALL U2MESS('F','MODELISA3_76')
          ELSE
             NVECT = -NVECT
             IF (NVECT.NE.3) THEN
                CALL U2MESS('F','MODELISA3_77')
              ELSE
                 CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,IARG,NVECT,
     &                       AXE,NV)
              ENDIF
          ENDIF
      ELSE
          NANGLE = -NANGLE
          IF (NANGLE.NE.2) THEN
             CALL U2MESS('F','MODELISA3_78')
          ENDIF
          CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,IARG,NANGLE,ANGLE,NV)
C
          ANGLE(1) = ANGLE(1)*R8DGRD()
          ANGLE(2) = ANGLE(2)*R8DGRD()
C
          AXE(1) =  COS(ANGLE(1))*COS(ANGLE(2))
          AXE(2) =  SIN(ANGLE(1))*COS(ANGLE(2))
          AXE(3) = -SIN(ANGLE(2))
      ENDIF
C
      XNORM2 = AXE(1)*AXE(1) + AXE(2)*AXE(2) + AXE(3)*AXE(3)
C
      IF (XNORM2.EQ.ZERO) THEN
          CALL U2MESS('F','MODELISA3_79')
      ENDIF
C
      XNORM = SQRT(XNORM2)
C
      AXE(1) = AXE(1)/XNORM
      AXE(2) = AXE(2)/XNORM
      AXE(3) = AXE(3)/XNORM
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

C ---      COMPTE NOMBRE DES NOEUDS D'UN MAILLE DANS LE CYLINDRE :
C          ------------------------------------------------------
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
                XNOXX2 = XX0(1)*XX0(1) + XX0(2)*XX0(2) + XX0(3)*XX0(3)
C
C ---       SI LE NOEUD COURANT EST CONFONDU AVEC LE NOEUD DE
C ---       L'AXE DU CYLINDRE, ON AFFECTE  LA MAILLE COURANTE A LA
C ---       LISTE DE MAILLES QUI SERA AFFECTEE AU GROUP_MA :
C           ----------------------------------------------
                IF (XNOXX2.EQ.ZERO) THEN
                    NBMA = NBMA + 1
                    ZI(IDLIMA+NBMA-1) = IMA
                    GOTO 10
                ELSE
C
                    XNOXX0 = SQRT (XNOXX2)
C
                    XX0(1) = XX0(1)/XNOXX0
                    XX0(2) = XX0(2)/XNOXX0
                    XX0(3) = XX0(3)/XNOXX0
C
C ---         CALCUL DE L'ANGLE FORME PAR L'AXE DU CYLINDRE
C ---         AVEC LE VECTEUR POSITION COURANT XX0 :
C             ------------------------------------
                    PSCA = ABS(XX0(1)*AXE(1) + XX0(2)*AXE(2) +
     &                         XX0(3)*AXE(3))
                    IF (PSCA .GT.UN) THEN
                        PSCA = PSCA - EPS
                    ENDIF
                    ANG  = ACOS(PSCA)
C
C ---         CALCUL DE LA DISTANCE DU NOEUD COURANT A L'AXE
C ---         DU CYLINDRE :
C             -----------
                    D2 =  ((X(1)-X0(1))*(X(1)-X0(1))
     &                   + (X(2)-X0(2))*(X(2)-X0(2))
     &                   + (X(3)-X0(3))*(X(3)-X0(3)))*SIN(ANG)*SIN(ANG)

C ---      SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A AU MOINS UN NOEUD
C          -------------------------------------------------------------
C
                    IF (SELEC.EQ.'AU_MOINS_UN') THEN
C
C ---         SI LE NOEUD COURANT EST DANS LE CYLINDRE, ON AFFECTE
C ---         LA MAILLE COURANTE A LA LISTE DE MAILLES QUI SERA
C ---         AFFECTEE AU GROUP_MA :
C             --------------------
                      IF (D2.LE.RAYON*RAYON) THEN
                          NBMA = NBMA + 1
                          ZI(IDLIMA+NBMA-1) = IMA
                          GOTO 10
                      ENDIF

C ---         SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A TOUT OU
C ---         MAJORITE, COMPTER LE NOMBRE DES NOEUDS D'UNE MAILLE
C ---         DANS LE CYLINDRE :
C             ------------------------------------------------
                    ELSE IF((SELEC.EQ.'TOUS').OR.
     &                       (SELEC.EQ.'MAJORITE'))THEN
C
                      IF (D2.LE.RAYON*RAYON) THEN
                        NBNOD=NBNOD+1
                      ENDIF
C
                    ENDIF
C
                ENDIF
C
 20        CONTINUE
C
           IF (SELEC.EQ.'TOUS') THEN
             IF (NBNOD.EQ.NBNO) THEN
               NBMA = NBMA + 1
               ZI(IDLIMA+NBMA-1) = IMA
               GOTO 10
             ENDIF
           ENDIF
C
           IF (SELEC.EQ.'MAJORITE') THEN
             IF (NBNOD.GE.(NBNO+1)/2) THEN
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
