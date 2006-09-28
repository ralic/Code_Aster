      SUBROUTINE CGNOES (MOFAZ, IOCC, NOMAZ, LISNOZ, NBNO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C       CGNOES -- TRAITEMENT DE L'OPTION ENV_SPHERE
C                 DU MOT FACTEUR CREA_GROUP_NO DE
C                 LA COMMANDE DEFI_GROUP
C
C      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_NO CONSTITUE
C      DE TOUS LES NOEUDS APPARTENANT A L'ENVELOPPE D'UNE SPHERE
C      DEFINIE PAR L'UTILISATEUR.
C
C -------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
C                                   APPARTENANT A L'ENVELOPPE
C                                   DE LA SPHERE.
C  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
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
      CHARACTER*(*) MOFAZ, NOMAZ, LISNOZ
C
C --------- VARIABLES LOCALES ---------------------------
      CHARACTER*1    K1BID
      CHARACTER*8    NOMA, K8BID, NOMAIL, NOMCEN
      CHARACTER*16   MOTFAC, MOCLE(3)
      CHARACTER*24   LISNOE
C
      REAL*8         X0(3), X(3)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      MOTFAC = MOFAZ
      NOMA   = NOMAZ
      LISNOE = LISNOZ
C
      ZERO  = 0.0D0
C
      X0(1) = ZERO
      X0(2) = ZERO
      X0(3) = ZERO
C
      X(1)  = ZERO
      X(2)  = ZERO
      X(3)  = ZERO
C
      RAYON = ZERO
C
      NBNO  = 0
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
C --- RECUPERATION DU CENTRE DE LA SPHERE (OU DU CERCLE) :
C     --------------------------------------------------
      MOCLE(1) = 'POINT'
      MOCLE(2) = 'NOEUD_CENTRE'
      MOCLE(3) = 'GROUP_NO_CENTRE'
      CALL UTCONO ( MOTFAC, MOCLE, IOCC, NOMA, NDIM, X0, IRET )
C
C --- RECUPERATION DU RAYON DE LA SPHERE :
C     ----------------------------------
      CALL GETVR8(MOTFAC,'RAYON',IOCC,1,0,RAYON,NRAYON)
      IF (NRAYON.EQ.0) THEN
          CALL U2MESS('F','MODELISA3_82')
      ELSE
         CALL GETVR8(MOTFAC,'RAYON',IOCC,1,1,RAYON,NB)
         IF (RAYON.LE.ZERO) THEN
             CALL U2MESS('F','MODELISA3_83')
         ENDIF
      ENDIF
C
C --- RECUPERATION DE LA DEMI-EPAISSEUR DE L'ENVELOPPE :
C     ------------------------------------------------
      CALL GETVR8(MOTFAC,'PRECISION',IOCC,1,0,PREC,NPREC)
      IF (NPREC.EQ.0) THEN
             CALL U2MESS('F','MODELISA3_90')
      ELSE
         CALL GETVR8(MOTFAC,'PRECISION',IOCC,1,1,PREC,NB)
         IF (PREC.LE.ZERO) THEN
             CALL U2MESS('F','MODELISA3_91')
         ENDIF
      ENDIF
C
C --- RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE :
C     ---------------------------------------------
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOE,K8BID,IER)
C
C --- ALLOCATION DU VECTEUR DES NOMS DES NOEUDS  APPARTENANT
C --- A L'ENVELOPPE DE LA SPHERE :
C     --------------------------
      CALL WKVECT(LISNOE,'V V I',NBNOE,IDLINO)
C
C --- PARCOURS DES NOEUDS DU MAILLAGE :
C     --------------------------------
      NBNO = 0
      DO 10 INO = 1, NBNOE
C
C ---     COORDONNEES DU NOEUD :
C         --------------------
           X(1) =  ZR(IDCOOR-1+3*(INO-1)+1)
           X(2) =  ZR(IDCOOR-1+3*(INO-1)+2)
           IF (NDIM.EQ.3) THEN
               X(3) =  ZR(IDCOOR-1+3*(INO-1)+3)
           ENDIF
C
C ---     DISTANCE DU NOEUD COURANT AU CENTRE DE LA SPHERE :
C         ------------------------------------------------
           D2 =  (X(1)-X0(1))*(X(1)-X0(1))
     &         + (X(2)-X0(2))*(X(2)-X0(2))
     &         + (X(3)-X0(3))*(X(3)-X0(3))
C
C ---     SI LE NOEUD COURANT APPARTIENT A L'ENVELOPPE DE LA SPHERE
C ---     ON L'AFFECTE A LA LISTE DE NOEUDS QUI SERA AFFECTEE
C ---     AU GROUP_NO :
C         -----------
           DIST = SQRT(D2)
           IF (ABS(DIST-RAYON).LE.PREC) THEN
                NBNO = NBNO + 1
                ZI(IDLINO+NBNO-1) = INO
           ENDIF
C
 10   CONTINUE
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
