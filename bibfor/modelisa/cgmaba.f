      SUBROUTINE CGMABA (MOFAZ, IOCC, NOMAZ, LISMAZ, NBMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/10/2001   AUTEUR CIBHHLV L.VIVAN 
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
      CHARACTER*8    NOMA, K8BID, NOMAIL, NOMPOI, NOMNOE
      CHARACTER*16   MOTFAC
      CHARACTER*24   LISMAI, NOEUMA
C
      REAL*8         X0(3), X(3), XX0(3), VECNOR(3), ANGLE(2)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      MOTFAC    = MOFAZ
      NOMA      = NOMAZ
      LISMAI    = LISMAZ
C
      NOEUMA    = NOMA//'.NOMNOE'
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
      CALL GETVR8(MOTFAC,'POINT',IOCC,1,0,R8BID,NPOINT)
      IF (NPOINT.EQ.0) THEN
          CALL GETVEM(NOMA,'NOEUD',MOTFAC,'NOEUD_CENTRE',
     +        IOCC,1,0,K8BID,NBNO)
          IF (NBNO.EQ.0) THEN
              CALL UTMESS('F','CGMABA','ON DOIT UTILISER '//
     +                    'OBLIGATOIREMENT LE MOT-CLE POINT '//
     +                    'OU LE MOT-CLE NOEUD_CENTRE POUR '//
     +                    'L''OPTION BANDE DE CREA_GROUP_MA POUR '//
     +                    'DEFINIR UN POINT SITUE SUR LE PLAN MILIEU '//
     +                    'DE LA BANDE. ')
          ELSE
              NBNO = -NBNO
              IF (NBNO.NE.1) THEN
                 CALL UTMESS('F','CGMABA','ON NE DOIT DONNER '//
     +                       'QU''UN SEUL NOEUD POUR DEFINIR '//
     +                       'LE POINT SITUE SUR LE PLAN MILIEU'//
     +                       ' DE LA BANDE.')
              ELSE
                 CALL GETVEM(NOMA,'NOEUD',MOTFAC,'NOEUD_CENTRE',
     +               IOCC,1,1,NOMPOI,NNO)
C
C ---       NUMERO DU NOEUD SITUE SUR LE PLAN MILIEU :
C           ----------------------------------------
                  CALL JENONU(JEXNOM(NOEUMA,NOMPOI),NUMPOI)
C
C ---       COORDONNEES  :
C           -----------
                  X0(1) =  ZR(IDCOOR-1+3*(NUMPOI-1)+1)
                  X0(2) =  ZR(IDCOOR-1+3*(NUMPOI-1)+2)
                  X0(3) =  ZR(IDCOOR-1+3*(NUMPOI-1)+3)
              ENDIF
          ENDIF
      ELSE
         CALL GETVR8(MOTFAC,'POINT',IOCC,1,NDIM,X0,NB)
         IF ( ABS(NB) .NE. NDIM ) THEN
           CALL UTDEBM('F','CGMABA','ERREUR DANS LES DONNEES')
           CALL UTIMPK('S',' MOT CLE FACTEUR ',1,MOTFAC)
           CALL UTIMPI('S',' OCCURENCE ',1,IOCC)
           IF ( NDIM .EQ. 2 ) THEN
             CALL UTIMPI('L','LE MAILLAGE EST "PLAN" OU "Z_CST"',0,IBID)
           ELSE
             CALL UTIMPI('L','LE MAILLAGE EST "3D"',0,IBID)
           ENDIF
           CALL UTIMPI('L','IL Y A ',1,ABS(NB))
           CALL UTIMPK('S',' VALEURS POUR LE MOT CLE ',1,'POINT')
           CALL UTIMPI('L','IL EN FAUT ',1,NDIM)
           CALL UTFINM()
         ENDIF
C
      ENDIF
C
C --- RECUPERATION DE LA DEMI-LARGEUR DE LA BANDE :
C     -------------------------------------------
      CALL GETVR8(MOTFAC,'DIST',IOCC,1,0,DIST,NDIST)
      IF (NDIST.EQ.0) THEN
          CALL UTMESS('F','CGMABA','ON DOIT UTILISER '//
     +                'OBLIGATOIREMENT LE MOT-CLE DIST '//
     +                'POUR DEFINIR LA DEMI-LARGEUR DE BANDE. ')
      ELSE
         CALL GETVR8(MOTFAC,'DIST',IOCC,1,1,DIST,NB)
         IF (DIST.LE.ZERO) THEN
             CALL UTMESS('F','CGMABA','ON DOIT DONNER '//
     +                   'UNE DISTANCE STRICTEMENT POSITIVE POUR '//
     +                   'DEFINIR LA BANDE. ')
         ENDIF
      ENDIF
C
C --- RECUPERATION DE LA DIRECTION PERPENDICULAIRE AU PLAN MILIEU
C --- DE LA BANDE :
C     -----------
      CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,1,0,R8BID,NANGLE)
      IF (NANGLE.EQ.0) THEN
          CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,1,0,R8BID,NVECT)
          IF (NVECT.EQ.0) THEN
              CALL UTMESS('F','CGMABA','ON DOIT UTILISER '//
     +                    'OBLIGATOIREMENT LE MOT-CLE ANGL_NAUT '//
     +                   'OU LE MOT-CLE VECT_NORMALE POUR L''OPTION '//
     +                   'BANDE DE CREA_GROUP_MA. CE MOT-CLE PERMET '//
     +                   'DE DEFINIR LA DIRECTION PERPENDICULAIRE '//
     +                   'AU PLAN MILIEU DE LA BANDE. ')
          ELSE
              NVECT = -NVECT
              IF (NDIM.EQ.3.AND.NVECT.NE.3) THEN
                  CALL UTMESS('F','CGMABA','POUR L''OPTION '//
     +                        'BANDE DE CREA_GROUP_MA, IL FAUT '//
     +                        ' DEFINIR LES 3 COMPOSANTES DU VECTEUR'//
     +                        ' PERPENDICULAIRE AU PLAN MILIEU DE LA '//
     +                        ' BANDE QUAND ON EST EN 3D.')
              ELSEIF (NDIM.EQ.2.AND.NVECT.NE.2) THEN
                  CALL UTMESS('F','CGMABA','POUR L''OPTION '//
     +                        'BANDE DE CREA_GROUP_MA, IL FAUT '//
     +                        ' DEFINIR LES 2 COMPOSANTES DU VECTEUR'//
     +                        ' PERPENDICULAIRE AU PLAN MILIEU DE LA '//
     +                        ' BANDE QUAND ON EST EN 2D.')
              ELSE
                  CALL GETVR8(MOTFAC,'VECT_NORMALE',IOCC,1,NVECT,VECNOR,
     +                        NV)
              ENDIF
          ENDIF
      ELSE
          NANGLE = -NANGLE
          NDIM1  =  NDIM - 1
          NANGLE =  MIN (NANGLE,NDIM1)
          CALL GETVR8(MOTFAC,'ANGL_NAUT',IOCC,1,NANGLE,ANGLE,NV)
         IF ( ABS(NV) .NE. NDIM1 ) THEN
           CALL UTDEBM('F','CGMABA','ERREUR DANS LES DONNEES')
           CALL UTIMPK('S',' MOT CLE FACTEUR ',1,MOTFAC)
           CALL UTIMPI('S',' OCCURENCE ',1,IOCC)
           IF ( NDIM .EQ. 2 ) THEN
             CALL UTIMPI('L','LE MAILLAGE EST "PLAN" OU "Z_CST"',0,IBID)
           ELSE
             CALL UTIMPI('L','LE MAILLAGE EST "3D"',0,IBID)
           ENDIF
           CALL UTIMPI('L','IL Y A ',1,ABS(NV))
           CALL UTIMPK('S',' VALEURS POUR LE MOT CLE ',1,'ANGL_NAUT')
           CALL UTIMPI('L','IL EN FAUT ',1,NDIM1)
           CALL UTFINM()
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
     +         VECNOR(3)*VECNOR(3)
C
      IF (XNORM2.EQ.ZERO) THEN
          CALL UTMESS('F','CGMABA','ERREUR DANS LA DONNEE DU '//
     +                   'VECTEUR NORMAL AU PLAN MILIEU DE LA  '//
     +                   'BANDE : CE VECTEUR EST NUL.')
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
     +                 XX0(3)*VECNOR(3)
C
C ---        SI LE NOEUD COURANT EST DANS LA BANDE, ON AFFECTE
C ---        LA MAILLE COURANTE A LA LISTE DE MAILLES QUI SERA
C ---        AFFECTEE AU GROUP_MA :
C            --------------------
                IF (ABS(D).LE.DIST) THEN
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
