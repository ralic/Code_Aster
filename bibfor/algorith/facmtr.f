      SUBROUTINE FACMTR (MATIN,MATOUT,IER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/05/2000   AUTEUR DURAND C.DURAND 
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
C***********************************************************************
C    P. RICHARD     DATE 23/11/90
C-----------------------------------------------------------------------
C  BUT: DELIVRER UN MATRICE FACTORISEE LDLT ET GENERATION DE
      IMPLICIT REAL*8 (A-H,O-Z)
C            SON NOM
C
C       CODE RETOUR:    0  TOUT S'EST BIEN PASSE
C                      -1  PRESENCE DE MODES DE CORPS SOLIDES
C-----------------------------------------------------------------------
C
C MATIN    /I/: NOM UTILISATEUR MATRICE BLOC EN ENTREE
C MATOUT   /I/: NOM UTILISATEUR MATRICE FACTORISEE EN SORTIE
C IER      /O/: CODE RETOUR
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*19 MATIN,MATOUT
      LOGICAL HPLOG
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      IF(MATIN.EQ.'                     ') GOTO 9999
      HPLOG=.FALSE.
      IF(MATIN(1:19).NE.MATOUT(1:19)) HPLOG=.TRUE.
C
C---------CONTROLE D'EXISTENCE DE LA MATRICE----------------------------
C
      CALL MTEXIS(MATIN,IER)
      IF(IER.EQ.0) THEN
        CALL UTDEBM('F','FACMTR','ARRET SUR MATRICE INEXISTANTE')
        CALL UTIMPK('L','MATRICE',1,MATIN)
        CALL UTFINM
      ENDIF
C
C
C    SI LA FACTORISATION EST HORS PLACE
C
      IF(HPLOG) THEN
C
C---------------------CREATION DE LA MATRICE DUPLIQUEE------------------
C
        CALL MTDEFS(MATOUT,MATIN,'V',' ')
C
C--------------DUPLICATION DE LA MATRICE--------------------------------
C
C
        CALL MTCOPY(MATIN,MATOUT,IER)
C
        IF(IER.GT.0) THEN
          CALL UTDEBM('F','FACMTR','PROBLEME DE DUPLICATION DE MATRICE')
          CALL UTIMPK('L','MATRICE: ',1,MATIN)
          CALL UTFINM
        ENDIF
C
C--------------ALLOCATION DESCRIPTEUR DE LA MATRICE---------------------
C
        CALL MTDSCR(MATOUT)
      ENDIF
C
C
C------------FACTORISATION EN PLACE DE LA MATRICE DUPLIQUEE-------------
C
C
      CALL JEVEUO(MATOUT(1:19)//'.&INT','E',LMAT)
C
      CALL TLDLGG(1,LMAT,1,0,0,NDECI,ISINGU,NPVNEG,IRE)
C
C------------------------ETUDE DU CODE RETOUR---------------------------
C
      IF(IRE.GT.1) THEN
        CALL UTDEBM('F','FACMTR',' ARRET PROBLEME DE FACTORISATION:
     &PRESENCE DE MODES DE CORPS RIGIDE')
        CALL UTFINM
        IER=-1
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
