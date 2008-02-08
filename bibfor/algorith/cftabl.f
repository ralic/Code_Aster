      SUBROUTINE CFTABL (INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                   RESOCO,TYPOPE,POSIT,LIAISO,TYPLIA)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT     NONE
      INTEGER      INDIC
      INTEGER      NBLIAC
      INTEGER      AJLIAI
      INTEGER      SPLIAI
      INTEGER      LLF
      INTEGER      LLF1
      INTEGER      LLF2
      INTEGER      POSIT
      INTEGER      LIAISO
      CHARACTER*1  TYPOPE
      CHARACTER*2  TYPLIA
      CHARACTER*24 RESOCO
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : ALGOCL/ALGOCO/ALGOCP/CFADH/CFNEG/ELPIV1
C                       ELPIV2/FRO2GD/FROLGD/FROPGD/FROGDP
C ----------------------------------------------------------------------
C
C MISE A JOUR DES VECTEURS DE LIAISONS CONTACT ET/OU FROTTEMENT
C LE NOMBRE DE LIAISONS EST MIS A JOUR DANS LA ROUTINE
C
C OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
C             -1 ON A ENLEVE UNE LIAISON
C I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
C I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON CORRECTE DU CALCUL
C              DE LA MATRICE DE CONTACT ACM1AT
C I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
C I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
C              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
C               DIRECTIONS SIMULTANEES (EN 3D)
C I/O LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C I/O LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C                'E': RESOCO(1:14)//'.LIAC'
C                'E': RESOCO(1:14)//'.CONVEC'
C IN  TYPOPE : TYPE D'OPERATION DANS LE VECTEUR DES LIAISONS
C                'A' : AJOUTER UNE LIAISON
C                'S' : SUPPRIMER UNE LIAISON
C IN  POSIT  : POSITION POUR AJOUTER UNE LIAISON DANS LE
C              VECTEUR DES LIAISONS ACTIVES
C IN  LIAISO : INDICE DE LA LIAISON A AJOUTER OU SUPPRIMER
C IN  TYPLIA : TYPE DE LA LIAISON
C                'C0': CONTACT
C                'F0': FROTTEMENT (2D) OU FROTTEMENT SUIVANT LES DEUX
C                  DIRECTIONS SIMULTANEES (3D)
C                'F1': FROTTEMENT SUIVANT LA PREMIERE DIRECTION (3D)
C                'F2': FROTTEMENT SUIVANT LA SECONDE DIRECTION (3D)
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER       II,LIAISP,POSIT2
      CHARACTER*1   TYPEAJ,TYPESP
      CHARACTER*2   TYPEC0,TYPEF0,TYPEF1,TYPEF2
      CHARACTER*19  LIAC,CONVEC
      INTEGER       JLIAC,JVECC
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- APPEL JEVEUX POUR LA MISE A JOUR DES VECTEURS DE LIAISONS --------
C ======================================================================
      LIAC   = RESOCO(1:14)//'.LIAC'
      CONVEC = RESOCO(1:14)//'.CONVEC'
      CALL JEVEUO (LIAC,  'E',JLIAC )
      CALL JEVEUO (CONVEC,'E',JVECC )
C ======================================================================
C --- INITIALISATION DES VARIABLES TYPE DE CONTACT ---------------------
C ======================================================================
      TYPEAJ = 'A'
      TYPESP = 'S'
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
C ======================================================================
      IF (TYPOPE.EQ.TYPEAJ) THEN
C ======================================================================
C --- ON AJOUTE UNE LIAISON --------------------------------------------
C ======================================================================
         INDIC  = 1
         ZK8(JVECC-1+POSIT) = TYPLIA
         ZI (JLIAC-1+POSIT) = LIAISO
         IF (TYPLIA.EQ.TYPEC0) THEN
C ======================================================================
C --- LIAISON DE CONTACT -----------------------------------------------
C ======================================================================
            NBLIAC = NBLIAC + 1
         ELSE IF (TYPLIA.EQ.TYPEF0) THEN
C ======================================================================
C --- LIAISON DE FROTTEMENT ADHERENT (DEUX DIRECTIONS) -----------------
C ======================================================================
            LLF    = LLF    + 1
            DO 111 II = 1, NBLIAC + LLF +LLF1 + LLF2 - 1
               IF (ZI(JLIAC-1+II).EQ.LIAISO) GOTO 112
 111        CONTINUE
            CALL ASSERT(.FALSE.)
 112        CONTINUE
         ELSE IF (TYPLIA.EQ.TYPEF1) THEN
C ======================================================================
C --- LIAISON DE FROTTEMENT ADHERENT (1ERE DIRECTION ) -----------------
C ======================================================================
            LLF1   = LLF1   + 1
         ELSE IF (TYPLIA.EQ.TYPEF2) THEN
C ======================================================================
C --- LIAISON DE FROTTEMENT ADHERENT (2EME DIRECTION ) -----------------
C ======================================================================
            LLF2   = LLF2   + 1
         ENDIF
C ======================================================================
      ELSE IF (TYPOPE.EQ.TYPESP) THEN
C ======================================================================
C --- ON SUPPRIME UNE LIAISON ------------------------------------------
C ======================================================================
         LIAISP =  0
         INDIC  = -1
         IF (TYPLIA.EQ.TYPEC0) THEN
C ======================================================================
C --- LA LIAISON A SUPPRIMER EST UNE LIAISON DE CONTACT ----------------
C --- ON SUPPRIME LA LIAISON DE FROTTEMENT ADHERENT ASSOCIEE -----------
C ======================================================================
            POSIT2 = NBLIAC + LLF + LLF1 + LLF2 + 1
            NBLIAC = NBLIAC - 1
            AJLIAI = AJLIAI - 1
            DO 10 II = POSIT + 1, NBLIAC + LLF + LLF1 + LLF2 + 1
               IF (ZI(JLIAC-1+II).EQ.LIAISO) THEN
                  AJLIAI = AJLIAI - 1
                  POSIT2 = II
                  LIAISP = 1
                  IF (ZK8(JVECC-1+II).EQ.TYPEF0) THEN
C ======================================================================
C --- LA LIAISON ASSOCIEE EST UNE LIAISON DE TYPE LLF ------------------
C ======================================================================
                     LLF  = LLF  - 1
                  ELSE IF (ZK8(JVECC-1+II).EQ.TYPEF1) THEN
C ======================================================================
C --- LA LIAISON ASSOCIEE EST UNE LIAISON DE TYPE LLF1 -----------------
C ======================================================================
                     LLF1 = LLF1 - 1
                  ELSE IF (ZK8(JVECC-1+II).EQ.TYPEF2) THEN
C ======================================================================
C --- LA LIAISON ASSOCIEE EST UNE LIAISON DE TYPE LLF2 -----------------
C ======================================================================
                     LLF2 = LLF2 - 1
                  ENDIF
                  GOTO 20
               ENDIF
 10         CONTINUE
 20         CONTINUE
            DO 30 II = POSIT, (POSIT2-1) - 1
               ZI (JLIAC-1+II) = ZI (JLIAC-1+II+1)
               ZK8(JVECC-1+II) = ZK8(JVECC-1+II+1)
 30         CONTINUE
            DO 40 II = POSIT2 - 1, NBLIAC + LLF + LLF1 + LLF2
               ZI (JLIAC-1+II) = ZI (JLIAC-1+II+1+LIAISP)
               ZK8(JVECC-1+II) = ZK8(JVECC-1+II+1+LIAISP)
 40         CONTINUE
         ELSE
            AJLIAI = AJLIAI - 1
            IF (TYPLIA.EQ.TYPEF0) THEN
C ======================================================================
C --- LA LIAISON A SUPPRIMER EST UNE LIAISON DE FROTTEMENT DE TYPE LLF--
C ======================================================================
               LLF  = LLF  - 1
            ELSE IF (TYPLIA.EQ.TYPEF1) THEN
C ======================================================================
C --- LA LIAISON A SUPPRIMER EST UNE LIAISON DE FROTTEMENT DE TYPE LLF1-
C ======================================================================
               LLF1 = LLF1 - 1
            ELSE IF (TYPLIA.EQ.TYPEF2) THEN
C ======================================================================
C --- LA LIAISON A SUPPRIMER EST UNE LIAISON DE FROTTEMENT DE TYPE LLF2-
C ======================================================================
               LLF2 = LLF2 - 1
            ENDIF
            DO 50 II = POSIT, NBLIAC + LLF + LLF1 + LLF2
               ZI (JLIAC-1+II) = ZI (JLIAC-1+II+1)
               ZK8(JVECC-1+II) = ZK8(JVECC-1+II+1)
 50         CONTINUE
         ENDIF
      ENDIF
C ======================================================================
C --- MISE A JOUR DE L'INDICATEUR POUR LA FACTORISATION DE LA MATRICE --
C --- DE CONTACT -------------------------------------------------------
C ======================================================================
      SPLIAI = MIN(SPLIAI,POSIT-1)
      SPLIAI = MIN(SPLIAI,NBLIAC+LLF+LLF1+LLF2)
      IF (AJLIAI.LT.0) AJLIAI = 0
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
