      SUBROUTINE ATA000 (AZ,NUMEDZ,RTBLOC,ATAZ,BASEZ,RESOCO,NESCL)
      IMPLICIT   NONE
      CHARACTER*(*)       AZ, NUMEDZ,         ATAZ, BASEZ
      CHARACTER*24        RESOCO
      REAL*8              RTBLOC
      INTEGER             NESCL
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 18/09/2002   AUTEUR CIBHHLV L.VIVAN 
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
C
C     ATA     --  LE BUT DE CETTE ROUTINE EST DE CREER LA MATR_ASSE
C                 DE NOM ATA.
C                 LE .VALE DE CETTE MATR_ASSE VA CONTENIR LES TERMES
C                 DU PRODUIT DE MATRICES AT*A.
C                 A EST 'CONCEPTUELLEMENT' UNE MATRICE RECTANGLE
C                 DE 'HAUTEUR' NBLIG ET DE LARGEUR NEQ.
C                 A EST 'INFORMATIQUEMENT' UNE COLLECTION NUMEROTEE
C                 COMPORTANT NBLIG OBJETS QUI SONT DES VECTEURS DE REELS
C                 DE LONGUEUR NEQ.
C                 CHACUN DE CES VECTEURS EST UNE LIGNE DE LA MATRICE A.
C                 LA MATR_ASSE ATA VA DONC ETRE SYMETRIQUE ET A
C                 VALEURS REELLES. D'AUTRE PART ON VA LUI AFFECTER
C                 LE PROFIL CORRESPONDANT AU NUMEDDL NUMEDZ
C                 QUI EST EN LIGNE DE CIEL OU MORSE .
C                 C'EST UNE ROUTINE CHAPEAU QUI APPELLE
C                 .ATASMO DANS LE CAS D'UN STOCKAGE MORSE
C                 .ATASLC DANS LE CAS D'UN STOCKAGE EN LIGNE DE 
C                    CIEL ET D'UN CONTACT AVEC FROTTEMENT 
C                 .ATALC2 DANS LE CAS D'UN STOCKAGE EN LIGNE DE 
C                    CIEL ET D'UN CONTACT SANS FROTTEMENT 
C                       
C
C   ARGUMENT        E/S  TYPE         ROLE
C    AZ              IN    K*     NOM DE LA COLLECTION DES VECTEURS
C                                 LIGNES (I.E. AZ EST LA MATRICE
C                                 RECTANGULAIRE POUR LAQUELLE ON VA
C                                 CALCULER LE PRODUIT AZ_T*AZ).
C    NUMEDZ         IN    K*      NOM DU NUME_DDL DECRIVANT LES
C                                 LIGNES DE LA MATRICE AZ
C    RTBLOC          IN    R8     TAILLE DES BLOCS DE LA MATR_ASSE.
C    ATAZ           OUT    K*     NOM DE LA MATR_ASSE SYMETRIQUE
C                                 A VALEURS REELLES DONT LE .VALE
C                                 CONTIENT LE PRODUIT AT*A.
C                                 LE PROFIL DE CETTE MATRICE EST
C                                 EN LIGNE DE CIEL.
C                                 CE PROFIL EST DETERMINE DANS LA
C                                 ROUTINE.
C    BASEZ           IN    K*     NOM DE LA BASE SUR LAQUELLE ON
C                                 CREE LA MATR_ASSE.
C    RESOCO          IN    K24    SD DE TRAITEMENT NUMERIQUE DU CONTACT
C.========================= DEBUT DES DECLARATIONS ====================
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------------------
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
      CHARACTER*32       JEXNUM , JEXNOM
C ---------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
      INTEGER   IRET, ICONTA
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEEXIN(NUMEDZ(1:14)//'.SLCS.REFE',IRET)
      IF (IRET.EQ.0) THEN
        CALL JEEXIN(NUMEDZ(1:14)//'.SMOS.REFE',IRET)
        IF (IRET.EQ.0) THEN
          CALL UTMESS('F','ATA000','IMPOSSIBILITE, LE NUME_DDL '//
     +                NUMEDZ(1:14)//' NE CORRESPOND NI A UN STOCKAGE '//
     +               'MORSE, NI A UN STOCKAGE EN LIGNE DE CIEL .')
        ELSE
          CALL ATASMO ( AZ, NUMEDZ, RTBLOC, ATAZ, BASEZ, NESCL )
        ENDIF
      ELSE
C
C --- TRAITEMENT DU CONTACT :
C
        CALL JEEXIN ( RESOCO(1:14)//'.APREAC', ICONTA )
        IF ( ICONTA .NE. 0 ) THEN
            CALL ATALC2 ( AZ, NUMEDZ, RTBLOC, ATAZ, BASEZ, NESCL )
        ENDIF       
      ENDIF
C
      END
