      SUBROUTINE ALLIGR ( CHAR, OPER, NOMA, FONREE, LIGRCZ )
      IMPLICIT NONE
      CHARACTER*4                           FONREE
      CHARACTER*8         CHAR,       NOMA
      CHARACTER*16              OPER
      CHARACTER*(*)                                 LIGRCZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 27/06/2001   AUTEUR CIBHHLV L.VIVAN 
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
C     ALLOUER LE LIGREL DE CHARGE  CORRESPONDANT AUX MOTS-CLE:
C       FORCE_NODALE                                      EN MECANIQUE
C       ECHANGE_PAROI                                     EN THERMIQUE
C
C IN  : CHAR   : NOM UTILISATEUR DE LA CHARGE
C IN  : OPER   : NOM DE LA COMMANDE (AFFE_CHAR_XXXX)
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : FONREE : FONC OU REEL SUIVANT L'OPERATEUR
C OUT : LIGRCZ : NOM DU LIGREL DE CHARGE
C OUT : NBTOUT : TABLEAU CONTENANT LES NOMBRE TOTAUX DE NOEUDS, GROUPES.
C                10 ENTIERS PAR MOT-CLE: DDL_IMPO, FORCE_NODALE,...
C ----------------------------------------------------------------------
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
C     ------- FIN COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       NFONO, NECHP, NTOT, LONLIG, LONEMA, NBGREL,
     +              NBMATA, IDNBNO, IDLGNS, NBET, JNOMA
      INTEGER       NBT1(10), LLIG, LNEMA, NGREL
      CHARACTER*8   TYPMCL(2)
      CHARACTER*16  MOCLEF, MOCLES(2)
      CHARACTER*19  LIGRCH
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NFONO  = 0
      NECHP  = 0
      LONLIG = 0
      LONEMA = 0
      NBGREL = 0
      NBMATA = 0
C
      MOCLES(1) = 'GROUP_NO'
      MOCLES(2) = 'NOEUD'
      TYPMCL(1) = 'GROUP_NO'
      TYPMCL(2) = 'NOEUD'
C
      IF (OPER(1:14).EQ.'AFFE_CHAR_MECA') THEN
C
         IF ( FONREE .NE. 'COMP' ) THEN
C
            MOCLEF = 'FORCE_NODALE'
            CALL GETFAC ( MOCLEF , NFONO )
            IF ( NFONO .NE. 0 ) THEN
               CALL ALCAR1 ( NOMA, MOCLEF, 2, MOCLES, TYPMCL, NBET )
               LONLIG = LONLIG + 2*NBET
               LONEMA = LONEMA + 2*NBET
               NBGREL = NBGREL + NBET
               NBMATA = NBMATA + NBET
            ENDIF
C
         ENDIF
C
         LIGRCH = CHAR//'.CHME.LIGRE'
C
      ELSEIF (OPER(1:14).EQ.'AFFE_CHAR_THER') THEN
C
         CALL GETFAC ( 'ECHANGE_PAROI', NECHP )
         IF ( NECHP .NE. 0 ) THEN
            CALL LIGECP ( CHAR, NOMA, NBT1, LLIG, LNEMA, NGREL )
            LONLIG = LONLIG + LLIG
            LONEMA = LONEMA + LNEMA
            NBGREL = NBGREL + NGREL
            NBMATA = NBMATA + NBT1(3)
         ENDIF
C
         LIGRCH = CHAR//'.CHTH.LIGRE'
C
      ELSEIF (OPER(1:14).EQ.'AFFE_CHAR_ACOU') THEN
C
         LIGRCH = CHAR//'.CHAC.LIGRE'
C
      ENDIF
C
C     --- CREATION DU LIGREL DE CHARGE SI NECESSAIRE ---
C
      NTOT = NFONO + NECHP
      IF (NTOT .NE. 0) THEN
         NBGREL = MAX(NBGREL,1)
         CALL JECREC (LIGRCH//'.LIEL','G V I','NU','CONTIG',
     &                'VARIABLE',NBGREL)
         LONLIG = MAX(LONLIG,1)
         CALL JEECRA (LIGRCH//'.LIEL', 'LONT', LONLIG, ' ')
C
         NBMATA = MAX(NBMATA,1)
         CALL JECREC (LIGRCH//'.NEMA', 'G V I', 'NU', 'CONTIG',
     +                                              'VARIABLE', NBMATA)
         LONEMA = MAX(LONEMA,1)
         CALL JEECRA (LIGRCH//'.NEMA', 'LONT', LONEMA, ' ')
C
         CALL JECREO (LIGRCH//'.NOMA', 'G E K8')
         CALL JEVEUO (LIGRCH//'.NOMA', 'E', JNOMA)
         ZK8(JNOMA) = NOMA
C
         CALL JECREO (LIGRCH//'.NBNO', 'G E I')
         CALL JEVEUO (LIGRCH//'.NBNO', 'E',IDNBNO)
         ZI(IDNBNO) = 0
C
         CALL WKVECT(LIGRCH//'.LGNS','G V I',2*LONEMA,IDLGNS)
C
      ENDIF
      LIGRCZ = LIGRCH
C
      CALL JEDEMA()
      END
