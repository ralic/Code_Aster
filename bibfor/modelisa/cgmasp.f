      SUBROUTINE CGMASP (MOFAZ, IOCC, NOMAZ, LISMAZ, NBMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/08/2003   AUTEUR CIBHHLV L.VIVAN 
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
C       CGMASP -- TRAITEMENT DE L'OPTION SPHERE
C                 DU MOT FACTEUR CREA_GROUP_MA DE
C                 LA COMMANDE DEFI_GROUP
C
C      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_MA CONSTITUE
C      DE TOUTES LES MAILLES DONT UN NOEUD AU MOINS APPARTIENT
C      A UNE SPHERE DE RAYON R ET DE CENTRE P0 (X0,Y0,Z0).
C      LE RAYON ET LE POINT P0 SONT DES DONNEES UTILISATEUR.
C
C -------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES DONT UN
C                                   NOEUD AU MOINS APPARTIENT A LA
C                                   SPHERE DEFINIE PAR L'UTILISATEUR
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
      CHARACTER*8    NOMA, K8BID, NOMAIL, NOMCEN, NOMNOE
      CHARACTER*16   MOTFAC, MOCLE(3)
      CHARACTER*24   LISMAI
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
      LISMAI = LISMAZ
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
      NBMA  = 0
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
          CALL UTMESS('F','CGMASP','ON DOIT UTILISER '//
     +                'OBLIGATOIREMENT LE MOT-CLE RAYON '//
     +                'POUR DEFINIR LE RAYON DE LA SPHERE. ')
      ELSE
         CALL GETVR8(MOTFAC,'RAYON',IOCC,1,1,RAYON,NB)
         IF (RAYON.LE.ZERO) THEN
             CALL UTMESS('F','CGMASP','ON DOIT DONNER '//
     +                   'UN RAYON STRICTEMENT POSITIF POUR '//
     +                   'DEFINIR LA SPHERE. ')
         ENDIF
      ENDIF
C
C --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
C     ---------------------------------------------
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMAI,K8BID,IER)
C
C --- ALLOCATION DU VECTEUR DES NOMS DES MAILLES  APPARTENANT
C --- A LA SPHERE :
C     -----------
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
     +                  K1BID)
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
                X(1) =  ZR(IDCOOR-1+3*(NUMNOE-1)+1)
                X(2) =  ZR(IDCOOR-1+3*(NUMNOE-1)+2)
                IF (NDIM.EQ.3) THEN
                   X(3) =  ZR(IDCOOR-1+3*(NUMNOE-1)+3)
                ENDIF
C
C ---        DISTANCE DU NOEUD COURANT AU CENTRE DE LA SPHERE :
C            ------------------------------------------------
                D2 =       (X(1)-X0(1))*(X(1)-X0(1))
     +                   + (X(2)-X0(2))*(X(2)-X0(2))
     +                   + (X(3)-X0(3))*(X(3)-X0(3))
C
C ---       SI LE NOEUD COURANT EST DANS LA SPHERE, ON AFFECTE
C ---       LA MAILLE COURANTE A LA LISTE DE MAILLES QUI SERA
C ---       AFFECTEE AU GROUP_MA :
C           --------------------
                IF (D2.LE.RAYON*RAYON) THEN
                    NBMA = NBMA + 1
                    ZI(IDLIMA+NBMA-1) = IMA
                    GOTO 10
                ENDIF
C
 20        CONTINUE
C
 10   CONTINUE
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
