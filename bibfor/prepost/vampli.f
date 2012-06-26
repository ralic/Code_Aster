      SUBROUTINE VAMPLI(VWORK, TDISP, LISTE, NBT, NBORDR, NUMINI,
     &                  NBP, NUMPAQ, TSPAQ, NOMCRI, NOMMAI,
     &                  NOMOPT, CXSR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/06/2012   AUTEUR TRAN V-X.TRAN 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE JMBHH01 J.M.PROIX
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      INTEGER      TDISP, NBP, LISTE(NBP), NBT, NBORDR, NUMINI
      INTEGER      NUMPAQ, TSPAQ
      REAL*8       VWORK(TDISP)
      CHARACTER*8  NOMMAI
      CHARACTER*16 NOMCRI, NOMOPT
      CHARACTER*19 CXSR
C ---------------------------------------------------------------------
C BUT: CALCULER LA VARIATION D'AMPLITUDE MAXIMALE
C ---------------------------------------------------------------------
C ARGUMENTS:
C VWORK     IN    R  : VECTEUR DE TRAVAIL CONTENANT
C                      L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                      ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                      DU <<PAQUET>> DE MAILLES.
C TDISP     IN    I  : DIMENSION DU VECTEUR VWORK
C LISTE     IN    I  : LISTE COMPLETE DES NOEUDS OU DES POINTS DE GAUSS
C                      PAR MAILLE A TRAITER.
C NBT       IN    I  : NOMBRE TOTAL DE POINT DE GAUSS OU DE NOEUDS
C                      A TRAITER.
C NBORDR    IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
C                      STRUCTURE DE DONNEES RESULTAT.
C NUMINI    IN    I  : NUMERO DE LA 1ERE MAILLE DU <<PAQUET>> DE
C                      MAILLES COURANT OU DU 1ER NOEUD DU <<PAQUET>> DE
C                      NOEUDS COURANT.
C NBP       IN    I  : NOMBRE DE MAILLES DANS LE <<PAQUET>> DE
C                      MAILLES COURANT.OU NOMBRE DE NOEUDS DANS LE
C                      <<PAQUET>> DE NOEUDS COURANT.
C NUMPAQ    IN    I  : NUMERO DU PAQUET DE MAILLES COURANT.
C TSPAQ     IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                      COURANT.
C NOMCRI    IN    K16: NOM DE LA MESURE D'AMPLITUDE.
C NOMMAI    IN    K8 : NOM DU MAILLAGE.
C NOMOPT    IN    K16: POST-TRAITEMENT AUX NOEUDS OU AUX POINTS DE GAUSS
C CXSR      IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
C                      RESULTATS :
C                           X = N ==> CNSR = RESULTATS AUX NOEUDS
C                           X = E ==> CESR = RESULTATS AUX ELEMENTS
C
C REMARQUE :
C  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
C    MAILLES DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
C-----------------------------------------------------------------------
      INTEGER       NNOINI, NBNOP, NBNOT, JCNRD, JCNRL, JCNRV
      INTEGER       L, CNBNO, KWORK, SOMNOW, IBIDNO, NUNOE, INOP
      INTEGER       DECAL, I, J, ADRSI, ADRSJ, K, ICMP
      INTEGER       JAD, NMAINI, NBMAP, NBPGT, JCERD, JCERL, JCERV
      INTEGER       NBPG, NBPGP, SOMPGW, IMAP, IPG
C
      REAL*8        VRESU(24)
      REAL*8        VAVMIS, VATRES, VMIS, TRES, TRAC, DETR
      REAL*8        TENSI(6), TENSJ(6), DTENS(6)
C
      CHARACTER*19 CNSR, CESR

C-----------------------------------------------------------------------
C
      CALL JEMARQ()

      VAVMIS = 0.0D0
      VATRES = 0.0D0

      IF ( NOMOPT .EQ. 'DOMA_NOEUD' ) THEN

         CNSR = CXSR
         NNOINI = NUMINI
         NBNOP = NBP
         NBNOT = NBT

C  OBTENTION DES ADRESSES '.CNSD', '.CNSL' ET '.CNSV' DU CHAMP SIMPLE
C  DESTINE A RECEVOIR LES RESULTATS : VMIS ET TRESCA

         CALL JEVEUO(CNSR//'.CNSD','L',JCNRD)
         CALL JEVEUO(CNSR//'.CNSL','E',JCNRL)
         CALL JEVEUO(CNSR//'.CNSV','E',JCNRV)


C   CONSTRUCTION DU VECTEUR : CONTRAINTE = F(NUMERO D'ORDRE) EN CHAQUE
C   NOEUDS DU PAQUET DE MAILLES.
         L = 1
         CNBNO = 0
         KWORK = 0
         SOMNOW = 0
         IBIDNO = 1

C  BOUCLE SUR LES NOEUDS

         DO 10 INOP=NNOINI, NNOINI+(NBNOP-1)

            NUNOE = LISTE(INOP)

            IF ( INOP .GT. NNOINI ) THEN
               KWORK = 1
               SOMNOW = SOMNOW + 1
            ENDIF

            CNBNO = CNBNO + 1
            IF ( (L*INT(NBNOT/10.0D0)) .LT. CNBNO ) THEN
               L = L + 1
            ENDIF


C  CALCUL DE LA VARIATION D'AMPLITUDE


C  IL Y A 6 COMPOSANTES POUR LES CONTRAINTES ==> DECAL=6
            DECAL = 18


            DO 30 I=1, (NBORDR-1)

               DO 40 J=(I+1), NBORDR

                  ADRSI = (I-1)*TSPAQ + KWORK*SOMNOW*DECAL
     &                                + (IBIDNO-1)*DECAL 

                  ADRSJ = (J-1)*TSPAQ + KWORK*SOMNOW*DECAL
     &                                + (IBIDNO-1)*DECAL 

C   TENSI/J(1) = CPXXI/J   TENSI/J(2) = CPYYI/J   TENSI/J(3) = CPZZI/J
C   TENSI/J(4) = CPXYI/J   TENSI/J(5) = CPXZI/J   TENSI/J(6) = CPYZI/J

                  TENSI(1) = VWORK(ADRSI + 1)
                  TENSI(2) = VWORK(ADRSI + 2)
                  TENSI(3) = VWORK(ADRSI + 3)
                  TENSI(4) = VWORK(ADRSI + 4)
                  TENSI(5) = VWORK(ADRSI + 5)
                  TENSI(6) = VWORK(ADRSI + 6)

                  TENSJ(1) = VWORK(ADRSJ + 1)
                  TENSJ(2) = VWORK(ADRSJ + 2)
                  TENSJ(3) = VWORK(ADRSJ + 3)
                  TENSJ(4) = VWORK(ADRSJ + 4)
                  TENSJ(5) = VWORK(ADRSJ + 5)
                  TENSJ(6) = VWORK(ADRSJ + 6)


                  DO 50 K=1, 6
                     DTENS(K) = TENSI(K) - TENSJ(K)
 50               CONTINUE

                  CALL RVINVT(DTENS,VMIS,TRES,TRAC,DETR)


                     
                  IF (VMIS .GT. VAVMIS) THEN
                     VAVMIS = VMIS
                  ENDIF

                  IF (TRES .GT. VATRES) THEN
                     VATRES = TRES
                  ENDIF

 40            CONTINUE

 30         CONTINUE


            DO 60 ICMP=1, 24
               VRESU(ICMP) = 0.0D0
 60         CONTINUE
            VRESU(23) = VAVMIS
            VRESU(24) = VATRES

C  AFFECTATION DES RESULTATS DANS UN CHAM_ELEM SIMPLE

            DO 70 ICMP=1, 24
               JAD = 24*(NUNOE-1) + ICMP
               ZL(JCNRL - 1 + JAD) = .TRUE.
               ZR(JCNRV - 1 + JAD) = VRESU(ICMP)
 70         CONTINUE

 10      CONTINUE


C  POUR LES GROUPES DE MAILLES

      ELSEIF ( NOMOPT .EQ. 'DOMA_ELGA' ) THEN

         CESR = CXSR
         NMAINI = NUMINI
         NBMAP = NBP
         NBPGT = NBT


C  OBTENTION DES ADRESSES '.CESD', '.CESL' ET '.CESV' DU CHAMP SIMPLE
C  DESTINE A RECEVOIR LES RESULTATS : DOMMAGE_MAX, COORDONNEES VECTEUR
C  NORMAL CORRESPONDANT

         CALL JEVEUO(CESR//'.CESD','L',JCERD)
         CALL JEVEUO(CESR//'.CESL','E',JCERL)
         CALL JEVEUO(CESR//'.CESV','E',JCERV)


C  CONSTRUCTION DU VECTEUR : CISAILLEMENT = F(NUMERO D'ORDRE) EN CHAQUE
C  POINT DE GAUSS DU PAQUET DE MAILLES.
         L = 1
         NBPG = 0
         NBPGP = 0
         KWORK = 0
         SOMPGW = 0

C BOUCLE SUR LES MAILLES

         DO 100 IMAP=NMAINI, NMAINI+(NBMAP-1)
            IF ( IMAP .GT. NMAINI ) THEN
               KWORK = 1
               SOMPGW = SOMPGW + LISTE(IMAP-1)
            ENDIF
            NBPG = LISTE(IMAP)

C SI LA MAILLE COURANTE N'A PAS DE POINTS DE GAUSS, LE PROGRAMME
C PASSE DIRECTEMENT A LA MAILLE SUIVANTE.
            IF (NBPG .EQ. 0) THEN
              GOTO 100
            ENDIF

            NBPGP = NBPGP + NBPG
            IF ( (L*INT(NBPGT/10.0D0)) .LT. NBPGP ) THEN
               L = L + 1
            ENDIF

C  BOUCLE SUR LES POINTS DE GAUSS

            DO 110 IPG=1, NBPG

C  CALCUL DE LA VARIATION D'AMPLITUDE

C  IL Y A 6 COMPOSANTES POUR LES CONTRAINTES ==> DECAL=6
               DECAL = 18


C  BOUCLE SUR LES NUMEROS D'ORDRES

               DO 130 I=1, (NBORDR-1)

                  DO 140 J=(I+1), NBORDR

                     ADRSI = (I-1)*TSPAQ + KWORK*SOMPGW*DECAL
     &                                   + (IPG-1)*DECAL 

                     ADRSJ = (J-1)*TSPAQ + KWORK*SOMPGW*DECAL
     &                                   + (IPG-1)*DECAL 


C   TENSI/J(1) = CPXXI/J   TENSI/J(2) = CPYYI/J   TENSI/J(3) = CPZZI/J
C   TENSI/J(4) = CPXYI/J   TENSI/J(5) = CPXZI/J   TENSI/J(6) = CPYZI/J

                     TENSI(1) = VWORK(ADRSI + 1)
                     TENSI(2) = VWORK(ADRSI + 2)
                     TENSI(3) = VWORK(ADRSI + 3)
                     TENSI(4) = VWORK(ADRSI + 4)
                     TENSI(5) = VWORK(ADRSI + 5)
                     TENSI(6) = VWORK(ADRSI + 6)

                     TENSJ(1) = VWORK(ADRSJ + 1)
                     TENSJ(2) = VWORK(ADRSJ + 2)
                     TENSJ(3) = VWORK(ADRSJ + 3)
                     TENSJ(4) = VWORK(ADRSJ + 4)
                     TENSJ(5) = VWORK(ADRSJ + 5)
                     TENSJ(6) = VWORK(ADRSJ + 6)


                     DO 150 K=1, 6
                        DTENS(K) = TENSI(K) - TENSJ(K)
 150                 CONTINUE

                     CALL RVINVT(DTENS,VMIS,TRES,TRAC,DETR)


                     IF (VMIS .GT. VAVMIS) THEN
                        VAVMIS = VMIS
                     ENDIF

                     IF (TRES .GT. VATRES) THEN
                        VATRES = TRES
                     ENDIF

 140              CONTINUE

 130           CONTINUE

C 11. CONSTRUCTION D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
C     POUR CHAQUE POINT DE GAUSS DE CHAQUE MAILLE LE DOMMAGE_MAX ET LE
C     VECTEUR NORMAL ASSOCIE.

               DO 160 ICMP=1, 24
                  VRESU(ICMP) = 0.0D0
 160           CONTINUE
               VRESU(23) = VAVMIS
               VRESU(24) = VATRES

C 12. AFFECTATION DES RESULTATS DANS UN CHAM_ELEM SIMPLE

               DO 170 ICMP=1, 24
                  CALL CESEXI('C',JCERD,JCERL,IMAP,IPG,1,ICMP,JAD)

                  CALL ASSERT (JAD .NE. 0)
                  JAD = ABS(JAD)
                  ZL(JCERL - 1 + JAD) = .TRUE.
                  ZR(JCERV - 1 + JAD) = VRESU(ICMP)

 170           CONTINUE

 110        CONTINUE

 100     CONTINUE

      ENDIF


C MENAGE

C PAS DE MENAGE DANS CETTE ROUTINE

      CALL JEDEMA()
      END
