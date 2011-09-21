      SUBROUTINE CALEMN(MOTFAZ, NOMAZ, IOCC, LISI1Z, LONLI1,
     &                  LISI2Z, LONLI2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      CALEMN   -- CONSTITUTION DE 2 LISTES DE NOMS (K8) DE NOEUDS
C                  LUES RESPECTIVEMENT APRES LES MOTS-CLES
C                  NOEUD_1, GROUP_NO_1, MAILLE_1 OU GROUP_MA_1
C                  D'UNE-PART ET
C                  NOEUD_2, GROUP_NO_2, MAILLE_2 OU GROUP_MA_2
C                  D'AUTRE-PART
C                  CES LISTES NE SONT PAS REDONDANTES
C                  (I.E. LES DOUBLONS SONT ELIMINES)
C                  POUR L'INSTANT, LE SEUL MOT-FACTEUR AUTORISE EST
C                  LIAISON_GROUP
C                  LES NOMS DE CES 2 LISTES SONT LISI1Z ET LISI2Z
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MOTFAZ         IN     K*       MOT-CLE FACTEUR
C                                   = 'LIAISON_GROUP' A CE JOUR
C    NOMAZ          IN     K*       NOM DU MAILLAGE
C    IOCC           IN     I        NUMERO D'OCCURENCE DU MOT FACTEUR
C    LISI1Z         OUT    K*       NOM DE LA LISTE DES NOMS (K8)
C                                   DE NOEUDS LUS APRES LES MOTS-CLES
C                                   NOEUD_1 OU GROUP_NO_1 OU
C                                   MAILLE_1 OU GROUP_MA_1
C    LONLI1         OUT    I        LONGUEUR DE LA LISTE PRECEDENTE
C    LISI2Z         OUT    K*       NOM DE LA LISTE DES NOMS (K8)
C                                   DE NOEUDS LUS APRES LES MOTS-CLES
C                                   NOEUD_2 OU GROUP_NO_2 OU
C                                   MAILLE_2 OU GROUP_MA_2
C    LONLI2         OUT    I        LONGUEUR DE LA LISTE PRECEDENTE
C
C.========================= DEBUT DES DECLARATIONS ====================
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C -----  ARGUMENTS
           CHARACTER*(*)     MOTFAZ, NOMAZ, LISI1Z, LISI2Z
C -----  VARIABLES LOCALES
           CHARACTER*8   K8BID,  NOMA, TYPEM
           CHARACTER*16  MOTCLE, TYMOCL, MOTFAC
           CHARACTER*24  LISIN1, LISIN2
      INTEGER      IARG
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
      MOTFAC = MOTFAZ
      NOMA   = NOMAZ
      LISIN1 = LISI1Z
      LISIN2 = LISI2Z
C
      CALL GETFAC(MOTFAC,NLIAI)
      IF (NLIAI.EQ.0) GOTO 9999
C
      N1 = 0
      N2 = 0
      N3 = 0
      N4 = 0
      N5 = 0
      N6 = 0
      N7 = 0
      N8 = 0
C
C --- DETERMINATION DU MOT-CLE A TRAITER POUR LA PREMIERE LISTE
C --- DE NOEUDS (I.E. 'GROUP_NO_1' OU 'NOEUD_1' OU 'GROUP_MA_1'
C --- OU 'MAILLE_1') :
C     --------------
      CALL GETVTX(MOTFAC,'GROUP_NO_1',IOCC,IARG,0,K8BID,N1)
      IF (N1.NE.0) THEN
         MOTCLE = 'GROUP_NO_1'
         TYMOCL = 'GROUP_NO'
      ELSE
         CALL GETVTX(MOTFAC,'NOEUD_1',IOCC,IARG,0,K8BID,N2)
         IF (N2.NE.0) THEN
           MOTCLE = 'NOEUD_1'
           TYMOCL = 'NOEUD'
         ELSE
           CALL GETVTX(MOTFAC,'GROUP_MA_1',IOCC,IARG,0,K8BID,N3)
           IF (N3.NE.0) THEN
             MOTCLE = 'GROUP_MA_1'
             TYMOCL = 'GROUP_MA'
           ELSE
             CALL GETVTX(MOTFAC,'MAILLE_1',IOCC,IARG,0,K8BID,N4)
             IF (N4.NE.0) THEN
               MOTCLE = 'MAILLE_1'
               TYMOCL = 'MAILLE'
             ELSE
               CALL U2MESK('F','MODELISA2_92',1,MOTFAC)
             ENDIF
           ENDIF
         ENDIF
      ENDIF
C
C --- CONSTITUTION DE LA PREMIERE LISTE DE NOEUDS :
C     -------------------------------------------
      TYPEM  = 'NO_NOEUD'
      CALL RELIEM(' ', NOMA, TYPEM, MOTFAC, IOCC, 1, MOTCLE,
     &                    TYMOCL, LISIN1, LONLI1 )
C
C --- DETERMINATION DU MOT-CLE A TRAITER POUR LA SECONDE LISTE
C --- DE NOEUDS (I.E. 'GROUP_NO_2' OU 'NOEUD_2' OU 'GROUP_MA_2'
C --- OU 'MAILLE_2') :
C     --------------
      CALL GETVTX(MOTFAC,'GROUP_NO_2',IOCC,IARG,0,K8BID,N5)
      IF (N5.NE.0) THEN
         MOTCLE = 'GROUP_NO_2'
         TYMOCL = 'GROUP_NO'
      ELSE
         CALL GETVTX(MOTFAC,'NOEUD_2',IOCC,IARG,0,K8BID,N6)
         IF (N6.NE.0) THEN
           MOTCLE = 'NOEUD_2'
           TYMOCL = 'NOEUD'
         ELSE
           CALL GETVTX(MOTFAC,'GROUP_MA_2',IOCC,IARG,0,K8BID,N7)
           IF (N7.NE.0) THEN
             MOTCLE = 'GROUP_MA_2'
             TYMOCL = 'GROUP_MA'
           ELSE
             CALL GETVTX(MOTFAC,'MAILLE_2',IOCC,IARG,0,K8BID,N8)
             IF (N8.NE.0) THEN
               MOTCLE = 'MAILLE_2'
               TYMOCL = 'MAILLE'
             ELSE
               CALL U2MESK('F','MODELISA2_93',1,MOTFAC)
             ENDIF
           ENDIF
         ENDIF
      ENDIF
C
C --- CONSTITUTION DE LA SECONDE LISTE DE NOEUDS :
C     ------------------------------------------
      TYPEM  = 'NO_NOEUD'
      CALL RELIEM(' ', NOMA, TYPEM, MOTFAC, IOCC, 1, MOTCLE,
     &                    TYMOCL, LISIN2, LONLI2 )
C
 9999 CONTINUE
      CALL JEDEMA()
      END
