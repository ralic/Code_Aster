      SUBROUTINE ORVLMA ( NOMA, LISTMA, NBMAIL, NORIEN,
     .                    VECT, NOEUD, PREC )
      IMPLICIT NONE
      INTEGER             LISTMA(*), NBMAIL, NOEUD, NORIEN
      CHARACTER*8         NOMA
      REAL*8              VECT(*), PREC
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 28/03/2006   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   ORVLMA  --  LE BUT EST QUE TOUTES LES MAILLES DE LA LISTE SOIENT
C               ORIENTEES SUIVANT LE VECTEUR DIRECTEUR.
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMA           IN    K8      NOM DU MAILLAGE
C    LISTMA         IN    I       LISTE DES MAILLES A REORIENTER
C    NBMAIL         IN    I       NB DE MAILLES DE LA LISTE
C    NORIEN        VAR            NOMBRE DE MAILLES REORIENTEES
C    VECT           IN    R       VECTEUR DIRECTEUR
C    NOEUD          IN    I       NOEUD D'ORIENTATION
C    PREC           IN    R       PRECISION
C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR
C -----  VARIABLES LOCALES
      INTEGER       IFM, NIV, IER, IMA, NUMA, NUTYMA, NBNMAI, NORIEG
      INTEGER       JTYMA, JCOOR,  P1, P2, JNBN, JDNO, JNOM, JDESM
      INTEGER       ICO, IORIV1, IORIV2, P3, P4, MAOK, INDI, IDEB
      INTEGER       NBNMA1, NBNMA2, JDESM1, JDESM2, JNOR, INDIIS
      INTEGER       NBMAT, I, IM2, IORIM1, IORIM2
      LOGICAL       DIME1, DIME2, REORIE
      CHARACTER*8   K8B, TPMAIL, NOMAIL
      CHARACTER*24  MAILMA, NOMAVO
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
C
C --- INITIALISATIONS :
C     ---------------
      CALL INFNIV ( IFM , NIV )
      MAILMA = NOMA//'.NOMMAI'
      REORIE = .TRUE.
C
C --- VECTEUR DU TYPE DES MAILLES DU MAILLAGE :
C     ---------------------------------------
      CALL JEVEUO ( NOMA//'.TYPMAIL        ', 'L', JTYMA )
C
C --- COORDONNEES DES NOEUDS DU MAILLAGE :
C     ----------------------------------
      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', JCOOR )
C
C --- RECUPERATION DE LA CONNECTIVITE DES MAILLES :
C     -------------------------------------------
      CALL JEVEUO ( JEXATR(NOMA//'.CONNEX','LONCUM'), 'L', P2 )
      CALL JEVEUO ( NOMA//'.CONNEX', 'E', P1 )
C
C --- RECUPERATION DES MAILLES VOISINES DU GROUP_MA :
C     ---------------------------------------------
      NOMAVO = '&&ORVLMA.MAILLE_VOISINE '
      CALL UTMAVO ( NOMA, '  ', LISTMA, NBMAIL, 'V', NOMAVO )
      CALL JEVEUO ( JEXATR(NOMAVO,'LONCUM'), 'L', P4 )
      CALL JEVEUO ( NOMAVO, 'L', P3 )
C
C     ALLOCATIONS :
C     -----------
      CALL WKVECT ( '&&ORVLMA.ORI1', 'V V I' , NBMAIL, JNBN )
      CALL WKVECT ( '&&ORVLMA.ORI2', 'V V I' , NBMAIL, JDNO )
      CALL WKVECT ( '&&ORVLMA.ORI3', 'V V K8', NBMAIL, JNOM )
      CALL WKVECT ( '&&ORVLMA.ORI4', 'V V L' , NBMAIL, JNOR )
C
C --- VERIFICATION DU TYPE DES MAILLES
C --- (ON DOIT AVOIR DES MAILLES DE PEAU) :
C     -----------------------------------
      DIME1 = .FALSE.
      DIME2 = .FALSE.
      DO 10 IMA = 1, NBMAIL
         NUMA = LISTMA(IMA)
         CALL JENUNO ( JEXNUM(MAILMA,NUMA), NOMAIL )
         ZK8(JNOM-1+IMA) = NOMAIL
         ZI(JNBN-1+IMA) = ZI(P2+NUMA+1-1) - ZI(P2+NUMA-1)
         ZI(JDNO-1+IMA) = ZI(P2+NUMA-1)
         ZL(JNOR-1+IMA) = .FALSE.
C
C ---   TYPE DE LA MAILLE COURANTE :
C       --------------------------
        NUTYMA = ZI(JTYMA+NUMA-1)
        CALL JENUNO ( JEXNUM('&CATA.TM.NOMTM',NUTYMA), TPMAIL )
C
        IF (TPMAIL(1:4).EQ.'QUAD') THEN
           DIME2 = .TRUE.
        ELSEIF (TPMAIL(1:4).EQ.'TRIA') THEN
           DIME2 = .TRUE.
        ELSEIF (TPMAIL(1:3).EQ.'SEG') THEN
           DIME1 = .TRUE.
        ELSE
           CALL UTMESS('F','ORVLMA','IMPOSSIBILITE, LA MAILLE '//
     +                NOMAIL//' DOIT ETRE UNE MAILLE DE PEAU, I.E. '//
     +                'DE TYPE "QUAD" OU "TRIA" EN 3D OU DE TYPE "SEG" '
     +              //'EN 2D, ET ELLE EST DE TYPE : '//TPMAIL)
        ENDIF
        IF (DIME1.AND.DIME2) CALL UTMESS('F','ORVLMA',
     +  'IMPOSSIBILITE DE MELANGER DES "SEG" ET DES "TRIA" OU "QUAD" !')
C
 10   CONTINUE
C 
C 
C --- PREMIER PASSAGE: METTRE LES MAILLES AYANT LE NOEUD DANS
C     LA BONNE ORIENTATION
C 
      NORIEG = 0
      MAOK = 0
C     
      DO 100 IMA = 1 , NBMAIL
         NOMAIL = ZK8(JNOM-1+IMA)
         NBNMAI =  ZI(JNBN-1+IMA)
         JDESM  =  ZI(JDNO-1+IMA)
C
C ------ VERIFICATION QUE LE NOEUD EST DANS LA MAILLE
         IF (DIME1)
     +      ICO = IORIV1( ZI(P1+JDESM-1), NOEUD, VECT, ZR(JCOOR) )
         IF (DIME2)
     +      ICO = IORIV2( ZI(P1+JDESM-1),NBNMAI, NOEUD,VECT, ZR(JCOOR))
C
C ------ LA MAILLE NE CONTIENT PAS LE NOEUD
         IF ( ICO .EQ. 0 ) THEN
C
C ------ LA MAILLE A ETE REORIENTEE
         ELSEIF ( ICO .LT. 0 ) THEN
            IDEB = IMA
            MAOK = MAOK + 1
            ZL(JNOR-1+IMA) = .TRUE.
            IF ( NIV .EQ. 2 ) THEN
               WRITE(IFM,*) 'LA MAILLE '//NOMAIL//
     +                       ' A ETE ORIENTEE PAR RAPPORT AU VECTEUR'
            ENDIF
            NORIEG = NORIEG + 1
C
C ------ LA MAILLE A LA BONNE ORIENTATION
         ELSE
            IDEB = IMA
            MAOK = MAOK + 1
            ZL(JNOR-1+IMA) = .TRUE.
            IF ( NIV .EQ. 2 ) THEN
               WRITE(IFM,*) 'LA MAILLE '//NOMAIL//
     +                       ' EST ORIENTEE PAR RAPPORT AU VECTEUR'
            ENDIF
         ENDIF

 100  CONTINUE
C 
C --- DEUXIEME PASSAGE: TOUTES LES NORMALES DANS LE MEME SENS
C 
 220  CONTINUE
      DO 200 IMA = IDEB , NBMAIL
         NOMAIL = ZK8(JNOM-1+IMA)
         IF ( .NOT. ZL(JNOR-1+IMA) ) GOTO 200
         NBNMA1 =  ZI(JNBN-1+IMA)
         JDESM1 =  ZI(JDNO-1+IMA)
C
         NBMAT = ZI(P4+IMA+1-1) - ZI(P4+IMA-1)

         DO 210 I = 1, NBMAT
            IM2 = ZI(P3+ZI(P4+IMA-1)-1+I-1)
            INDI = INDIIS ( LISTMA, IM2, 1, NBMAIL )
            IF ( INDI .EQ. 0 ) GOTO 210
            IF ( ZL(JNOR-1+INDI) ) GOTO 210
            NBNMA2 =  ZI(JNBN-1+INDI)
            JDESM2 =  ZI(JDNO-1+INDI)
C
            IF (DIME1)
     +         ICO = IORIM1 ( ZI(P1+JDESM1-1), 
     +                        ZI(P1+JDESM2-1), REORIE )
            IF (DIME2)
     +         ICO = IORIM2 ( ZI(P1+JDESM1-1), NBNMA1,
     +                        ZI(P1+JDESM2-1), NBNMA2, REORIE )
C
            IF ( ICO .EQ. 0 )  GOTO 210
            IF ( ICO .LT. 0 )  NORIEG = NORIEG + 1
            ZL(JNOR-1+INDI) = .TRUE.
            MAOK = MAOK + 1
C
 210     CONTINUE
C
 200  CONTINUE
C
      IDEB = 1
      IF ( MAOK .LT. NBMAIL ) GOTO 220
C
      NORIEN = NORIEN + NORIEG
C
      CALL JEDETR ( '&&ORVLMA.ORI1' )
      CALL JEDETR ( '&&ORVLMA.ORI2' )
      CALL JEDETR ( '&&ORVLMA.ORI3' )
      CALL JEDETR ( '&&ORVLMA.ORI4' )
      CALL JEDETR ( NOMAVO )
C
      CALL JEDEMA()
      END
