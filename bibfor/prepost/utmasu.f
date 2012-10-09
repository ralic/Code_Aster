      SUBROUTINE UTMASU ( MAIL, KDIM, NLIMA, LIMA, NOMOB1,
     &                    COOR, NBMAVO, MAILVO,COINCE )
      IMPLICIT NONE
C RESPONSABLE PELLET J.PELLET
      INCLUDE 'jeveux.h'
      INTEGER             LIMA(*), NLIMA, NBMAVO, MAILVO(*)
      REAL*8              COOR(*)
      CHARACTER*2         KDIM
      CHARACTER*8         MAIL
      CHARACTER*(*)       NOMOB1
      LOGICAL COINCE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/10/2012   AUTEUR PELLET J.PELLET 
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
C BUT : RECUPERER LA COUCHE DES MAILLES QUI "BORDENT"
C       UNE LISTE DE MAILLES DE "PEAU"
C
C     PEAU 2D (TRIA,QUAD) => MAILLES 3D
C     PEAU 1D (SEG)       => MAILLES 2D
C
C   ARGUMENTS :
C   -----------
C     MAIL (IN)  : NOM DU MAILLAGE
C     KDIM (IN)  : / '3D' RECHERCHE LES MAILLES 3D VOISINES
C                  / '2D' RECHERCHE LES MAILLES 2D VOISINES
C     NLIMA (IN)  : NOMBRE DE MAILLES DE LIMA
C     LIMA  (IN)  : LISTE DES NUMEROS DES MAILLES DE PEAU
C
C     NOMOB1 (IN/JXOUT) : NOM DE L' OJB A CREER (VECTEUR D'ENTIERS)
C       CE VECTEUR EST DE LONGUEUR NLIMA :
C       POUR CHAQUE MAILLE DE PEAU, IL CONTIENT UNE MAILLE QUI
C       "BORDE" CETTE MAILLE DE PEAU.
C
C     NBMAVO (IN) : / NB DE MAILLES DE MAILVO
C                   / 0
C     MAILVO (IN) : / LISTE DE MAILLE "CANDIDATES"
C                   / 0 (SI NBMAVO=0)
C     MAILVO EST UN SOUS-ENSEMBLE DES MAILLES DU MAILLAGE
C     QUI PERMET D'"ORIENTER" LE CHOIX DES MAILLES DE NOMOB1
C     MAILVO EST UTILISE EN PARTICULIER QUAND ON VEUT REORIENTER
C     DES FACETTES QUI SONT INSERREES ENTRE DES MAILLES VOLUMIQUES:
C        ORIE_PEAU_3D ("GROUP_MA_VOLU")
C        ORIE_PEAU_2D ("GROUP_MA_SURF")
C
C     COINCE (IN) : /.TRUE.  /.FALSE.
C       SI .TRUE. ON ACCEPTE QU'UNE MAILLE DE PEAU SOIT "COINCEE"
C       ENTRE 2 MAILLES DE PART ET D'AUTRE DE LA PEAU.
C       SINON, ON EMET UNE ERREUR FATALE.
C       SI .TRUE., ON CHOISIT LA MAILLE TELLE QUE SA NORMALE SORTANTE
C       SOIT LA MEME QUE CELLE DE LA MAILLE DE PEAU.
C
C-----------------------------------------------------------------------
C
      INTEGER       P1,P2,P3,P4, JM3D, INDIIS, NBMAT, IRET, IM1, IM2
      INTEGER       IMA, NUMA, NNOE, INO, NBM, I, K, INDI, NNOEM, NNOE1
      INTEGER       IFM , NIV, IPOS, ITYPMA, NUTYMA
      INTEGER       LISNOE(27),INDMAI
      LOGICAL       FIRST
      CHARACTER*8   K8B, NOMAIL, TYPE, VNOR
      CHARACTER*16  OPER,K16B
      CHARACTER*24  NOMAVO,VALK(4)
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFNIV ( IFM , NIV )

      CALL ASSERT(KDIM.EQ.'3D'.OR.KDIM.EQ.'2D')
      FIRST = .FALSE.
      CALL GETRES(K8B,K16B,OPER)

      CALL JEVEUO ( JEXATR(MAIL//'.CONNEX','LONCUM'), 'L', P2 )
      CALL JEVEUO ( MAIL//'.CONNEX', 'L', P1 )
      CALL JEVEUO ( MAIL//'.TYPMAIL','L',ITYPMA)

C --- CREATION DE NOMOB1:
C     -------------------
      CALL WKVECT ( NOMOB1, 'V V I' , NLIMA, JM3D )

C --- RECUPERATION DES MAILLES VOISINES DE LIMA :
C     ---------------------------------------------
      NOMAVO = '&&UTMASU.MAILLE_VOISINE '
      CALL UTMAVO ( MAIL, KDIM, LIMA, NLIMA, 'V', NOMAVO,NBMAVO,MAILVO)
      CALL JEVEUO ( JEXATR(NOMAVO,'LONCUM'), 'L', P4 )
      CALL JEVEUO ( NOMAVO, 'L', P3 )


C --- ON REMPLIT NOMOB1 :
C     ------------------
      DO 100 IMA = 1, NLIMA
         NUMA = LIMA(IMA)
         NUTYMA=ZI(ITYPMA+NUMA-1)
         NNOE = ZI(P2+NUMA)-ZI(P2-1+NUMA)
         CALL ASSERT( NNOE .LE. 27 )
         DO 80 INO = 1,NNOE
            LISNOE(INO) = ZI(P1-1+ZI(P2+NUMA-1)+INO-1)
  80     CONTINUE
         NBMAT = ZI(P4+IMA+1-1) - ZI(P4+IMA-1)
         NBM = 0
         DO 10 I = 1, NBMAT
            IM2 = ZI(P3+ZI(P4+IMA-1)-1+I-1)
            IF ( IM2 .EQ. 0 ) GOTO 10
            IF ( ZI(P1+ZI(P2+IM2-1)-1) .EQ. 0 ) GOTO 10
            NNOEM = ZI(P2+IM2) - ZI(P2-1+IM2)

            DO 12 K =  1 , NNOE
               INDI = INDIIS(ZI(P1+ZI(P2+IM2-1)-1),LISNOE(K),1,NNOEM)
               IF ( INDI .EQ. 0 )   GOTO 10
 12         CONTINUE
            NBM = NBM + 1
            IF ( NBM .EQ. 1 ) THEN
               ZI(JM3D+IMA-1) = IM2
            ELSE
C              -- CAS OU LA MAILLE DE PEAU EST BORDEE PAR PLUS
C                 D'UNE MAILLE
               IM1 = ZI(JM3D+IMA-1)
               NNOE1 = ZI(P2+IM1) - ZI(P2-1+IM1)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPE)
               CALL ORIEM0 ( TYPE, MAIL, COOR, ZI(P1+ZI(P2+IM1-1)-1),
     &              NNOE1, ZI(P1+ZI(P2+IM2-1)-1), NNOEM, LISNOE, NNOE,
     &              IPOS, INDMAI )
               IF ( IPOS .EQ. 0)THEN
C                -- SI IPOS=0, LES 2 MAILLES SONT DU MEME COTE, ON PEUT
C                   CONSERVER LA 1ERE.
               ELSE
C                -- SINON, IM2 ET IM1 SONT DE PART ET D'AUTRE DE NUMA
                 IF(.NOT.COINCE) THEN
                   CALL JENUNO(JEXNUM(MAIL//'.NOMMAI',NUMA),VALK(1))
                   CALL JENUNO(JEXNUM(MAIL//'.NOMMAI',IM1),VALK(2))
                   CALL JENUNO(JEXNUM(MAIL//'.NOMMAI',IM2),VALK(3))
                   CALL U2MESK('F','PREPOST4_97',3,VALK)
                 ELSE
                   ZI(JM3D+IMA-1) = IM2
                 ENDIF
               ENDIF
            ENDIF
 10      CONTINUE


         IF ( NBM .EQ. 0 .AND. NIV.GT.1 ) THEN
            CALL JENUNO(JEXNUM(MAIL//'.NOMMAI',NUMA),NOMAIL)
            IF ( FIRST ) THEN
               VALK(1)=NOMAIL
               CALL U2MESK('A+','PREPOST6_29',1,VALK)
            ELSE
               VALK (1)= NOMAIL
               CALL U2MESG('A+','PREPOST6_30',1,VALK,0,0,0,0.D0)
            ENDIF
            FIRST = .TRUE.
         ENDIF

 100  CONTINUE

      CALL JEDETR ( NOMAVO )
      CALL JEDEMA()
C
      END
