      SUBROUTINE CGNOPL (MOFAZ, IOCC, NOMAZ, LISNOZ, NBNO)
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
C       CGNOPL -- TRAITEMENT DE L'OPTION PLAN
C                 DU MOT FACTEUR CREA_GROUP_NO DE
C                 LA COMMANDE DEFI_GROUP
C
C      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_NO CONSTITUE
C      DE TOUS LES NOEUDS APPARTENANT A UNE DROITE EN 2D
C      OU UN PLAN EN 3D DEFINIS PAR L'UTILISATEUR.
C
C -------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
C                                   APPARTENANT A LA DROITE (EN 2D)
C                                   OU AU PLAN (EN 3D) DONNES PAR
C                                   L'UTILISATEUR
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
      CHARACTER*8    NOMA, K8BID, NOMAIL, NOMPOI
      CHARACTER*16   MOTFAC, MOCLE(3)
      CHARACTER*24   LISNOE
C
      REAL*8         X0(3), VECNOR(3), ANGLE(2)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      MOTFAC    = MOFAZ
      NOMA      = NOMAZ
      LISNOE    = LISNOZ
C
      ZERO      = 0.0D0
C
      X0(1)     = ZERO
      X0(2)     = ZERO
      X0(3)     = ZERO
C
      VECNOR(1) = ZERO
      VECNOR(2) = ZERO
      VECNOR(3) = ZERO
C
C --- RECUPERATION DE LA DIMENSION DU MAILLAGE :
C     ----------------------------------------
      NDIM=3
      CALL DISMOI('F','Z_CST',NOMA,'MAILLAGE',IBID,K8BID,IER)
      IF (K8BID.EQ.'OUI') NDIM=2
C
C --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
C     --------------------------------------------------
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',IDCOOR)
C
C --- RECUPERATION DU POINT SITUE SUR LE PLAN OU LA DROITE :
C     ----------------------------------------------------
      MOCLE(1) = 'POINT'
      MOCLE(2) = 'NOEUD_CENTRE'
      MOCLE(3) = 'GROUP_NO_CENTRE'
      CALL UTCONO ( MOTFAC, MOCLE, IOCC, NOMA, NDIM, X0, IRET )
C
C --- RECUPERATION DE LA DIRECTION PERPENDICULAIRE AU PLAN MILIEU
C --- DE LA BANDE :
C     -----------
      CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,1,0,R8BID,NANGLE)
      IF (NANGLE.EQ.0) THEN
          CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,1,0,R8BID,NVECT)
          IF (NVECT.EQ.0) THEN
              CALL UTMESS('F','CGNOPL','ON DOIT UTILISER '//
     +                    'OBLIGATOIREMENT LE MOT-CLE ANGL_NAUT '//
     +                    'OU LE MOT-CLE VECT_NORMALE POUR L''OPTION'
     +                  //' PLAN DE CREA_GROUP_NO. CE MOT-CLE'
     +                  //' PERMET DE DEFINIR LA DIRECTION  '//
     +                    'PERPENDICULAIRE AU PLAN OU A LA DROITE. ')
          ELSE
              NVECT = -NVECT
              IF (NDIM.EQ.3.AND.NVECT.NE.3) THEN
                  CALL UTMESS('F','CGNOPL','POUR L''OPTION '//
     +                        'PLAN DE CREA_GROUP_NO, IL FAUT '//
     +                        ' DEFINIR LES 3 COMPOSANTES DU VECTEUR'//
     +                        ' PERPENDICULAIRE AU PLAN.')
              ELSEIF (NDIM.EQ.2.AND.NVECT.NE.2) THEN
                  CALL UTMESS('F','CGNOPL','POUR L''OPTION '//
     +                        'PLAN DE CREA_GROUP_NO, IL FAUT '//
     +                        ' DEFINIR LES 2 COMPOSANTES DU VECTEUR'//
     +                        ' PERPENDICULAIRE A LA DROITE.')
              ELSE
                  CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,1,NVECT,
     +                        VECNOR,NV)
              ENDIF
          ENDIF
      ELSE
          NANGLE = -NANGLE
          NDIM1  =  NDIM - 1
          NANGLE =  MIN (NANGLE,NDIM1)
          CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,1,NANGLE,ANGLE,NV)
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
     +         VECNOR(3)*VECNOR(3)
C
      IF (XNORM2.EQ.ZERO) THEN
          CALL UTMESS('F','CGNOPL','ERREUR DANS LA DONNEE DU '//
     +                   'VECTEUR NORMAL AU PLAN OU A LA DROITE : '//
     +                   'CE VECTEUR EST NUL.')
      ENDIF
C
      XNORM = SQRT(XNORM2)
C
      VECNOR(1) = VECNOR(1)/XNORM
      VECNOR(2) = VECNOR(2)/XNORM
      VECNOR(3) = VECNOR(3)/XNORM
C
C --- RECUPERATION DE LA TOLERANCE :
C     ----------------------------
      CALL GETVR8(MOTFAC,'PRECISION',IOCC,1,0,PREC,NPREC)
      IF (NPREC.EQ.0) THEN
             CALL UTMESS('F','CGNOPL','LE MOT-CLE PRECISION '//
     +                   'EST OBLIGATOIRE APRES LE MOT-CLE PLAN '//
     +                   ' POUR DEFINIR LA TOLERANCE '//
     +                   '(I.E. LA DISTANCE DU POINT AU PLAN OU A '//
     +                   'LA DROITE) ACCEPTEE POUR DECLARER '//
     +                   'L''APPARTENANCE DU POINT A CE PLAN OU A '//
     +                   'CETTE DROITE.')
      ELSE
         CALL GETVR8(MOTFAC,'PRECISION',IOCC,1,1,PREC,NB)
         IF (PREC.LE.ZERO) THEN
             CALL UTMESS('F','CGNOPL','ON DOIT DONNER '//
     +                   'UNE TOLERANCE STRICTEMENT POSITIVE '//
     +                   'POUR VERIFIER L''APPARTENANCE D''UN '//
     +                   'NOEUD AU PLAN OU A LA DROITE. ')
         ENDIF
      ENDIF
C
C --- RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE :
C     ---------------------------------------------
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOE,K8BID,IER)
C
C --- ALLOCATION DU VECTEUR DES NOMS DES NOEUDS  APPARTENANT
C --- AU PLAN OU A LA DROITE :
C     ----------------------
      CALL WKVECT(LISNOE,'V V I',NBNOE,IDLINO)
C
      CALL CGNOP0 (NBNOE,ZR(IDCOOR), X0, VECNOR, PREC, NBNO,ZI(IDLINO))
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
