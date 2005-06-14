      SUBROUTINE EXNOCO (CHAR,MOTFAC,NOMA,MOTCLE,IREAD,JTRAV,
     &                   NBMA,NBNO,NBNOQU,
     &                   IPMA,IPNO,IPNOQU, 
     &                   LISTMA,LISTNO,LISTQU) 
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 07/10/2004   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT      NONE
      CHARACTER*8   CHAR
      CHARACTER*16  MOTFAC
      CHARACTER*8   NOMA
      CHARACTER*(*) MOTCLE      
      INTEGER       IREAD
      INTEGER       JTRAV
      INTEGER       NBMA
      INTEGER       NBNO
      INTEGER       NBNOQU  
      INTEGER       IPMA
      INTEGER       IPNO
      INTEGER       IPNOQU          
      INTEGER       LISTMA(NBMA)
      INTEGER       LISTNO(NBNO)
      INTEGER       LISTQU(3*NBNOQU)
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : LIEXCO
C ----------------------------------------------------------------------
C
C REMPLISSAGE DE LA LISTE DES MAILLES, DES NOEUDS ET DES NOEUDS 
C    QUADRATIQUES
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
C IN  NOMA   : NOM DU MAILLAGE
C IN  MOTCLE : MOT-CLE (GROUP_MA ou MAILLE)
C IN  IREAD  : INDICE POUR LIRE LES DONNEES DANS AFFE_CHAR_MECA
C IN  JTRAV  : POINTEUR VERS VECTEUR DE TRAVAIL 'BIDON'
C IN  NBMA   : NOMBRE DE MAILLES DANS LA ZONE IREAD
C IN  NBNO   : NOMBRE DE NOEUDS DANS LA ZONE IREAD
C IN  NBNOQU : NOMBRE DE NOEUDS QUADRATIQUES DANS LA ZONE IREAD
C I/O IPMA   : INDICE POUR LA LISTE DES NUMEROS DES MAILLES DE CONTACT
C I/O IPNO   : INDICE POUR LA LISTE DES NUMEROS DES NOEUDS DE CONTACT
C I/O IPQU   : INDICE POUR LA LISTE DES NUMEROS DES NOEUDS QUADRATIQUES
C              DE CONTACT
C I/O LISTMA : LISTE DES NUMEROS DES MAILLES DE CONTACT
C I/O LISTNO : LISTE DES NUMEROS DES NOEUDS DE CONTACT
C I/O LISTQU : LISTE DES NUMEROS DES NOEUDS QUADRATIQUES DE CONTACT
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*8    TYPENT,K8BID
      INTEGER        NG,NGR
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      
      IF (MOTCLE(1:6).EQ.'MAILLE') THEN
         TYPENT = 'MAILLE'    
      ELSE IF (MOTCLE(1:8).EQ.'GROUP_MA') THEN
         TYPENT = 'GROUP_MA'    
      ELSE
         CALL UTMESS ('F','EXNOCO',
     +                'MOT CLE INCONNU (NI MAILLE, NI GROUP_MA')
      ENDIF

      CALL GETVEM(NOMA,TYPENT,MOTFAC,MOTCLE,
     &             IREAD,1,0,K8BID,NG)
      IF (NG.NE.0) THEN
         NG = - NG
         CALL GETVEM(NOMA,TYPENT,MOTFAC,MOTCLE,
     &               IREAD,1,NG,ZK8(JTRAV),NGR)
     
         IF (TYPENT.EQ.'MAILLE') THEN
            NGR = 0
         ENDIF
         CALL EXNOEL(CHAR,NOMA,MOTCLE(1:8),NGR,ZK8(JTRAV),
     &                   NBMA,NBNO,NBNOQU,
     &                   LISTMA,LISTNO,LISTQU,
     &                   IPMA,IPNO,IPNOQU)
      END IF

C
C ----------------------------------------------------------------------
C
      CALL JEDEMA()
      END
