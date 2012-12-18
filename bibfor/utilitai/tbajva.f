      SUBROUTINE TBAJVA(TABLE,NBPARA,NOMPAR,
     &                  VI,LIVI,VR,LIVR,VC,LIVC,VK,LIVK)
      IMPLICIT NONE
      INTEGER NBPARA,VI,LIVI(*)
      REAL*8 VR,LIVR(*)
      COMPLEX*16 VC,LIVC(*)
      CHARACTER*(*) TABLE,NOMPAR,VK,LIVK(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/12/2012   AUTEUR DELMAS J.DELMAS 
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
C      AJOUTER UNE LIGNE A UNE TABLE.
C ----------------------------------------------------------------------
C IN  : TABLE  : NOM DE LA STRUCTURE "TABLE".
C IN  : NBPARA : NOMBRE DE PARAMETRES DE NOMPAR
C IN  : NOMPAR : PARAMETRE POUR LEQUEL ON VEUT ECRIRE
C IN  : VI     : VALEUR POUR LE PARAMETRE "I"
C I/O : LIVI   : LISTE DES VALEURS POUR LES PARAMETRES "I"
C IN  : VR     : VALEUR POUR LE PARAMETRE "R"
C I/O : LIVR   : LISTE DES VALEURS POUR LES PARAMETRES "R"
C IN  : VC     : VALEUR POUR LE PARAMETRE "C"
C I/O : LIVC   : LISTE DES VALEURS POUR LES PARAMETRES "C"
C IN  : VK     : VALEUR POUR LE PARAMETRE "K"
C I/O : LIVK   : LISTE DES VALEURS POUR LES PARAMETRES "K"
C ----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
C
C ----------------------------------------------------------------------
C
      INTEGER      IRET,NBCOL,JTBNP
      INTEGER      JTBLP,I,KI,KR,KC,KK
      CHARACTER*19 NOMTAB
      CHARACTER*24 TYPE,NOMCOL
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = ' '
      NOMTAB = TABLE
      CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_64')
      ENDIF
      IF ( NOMTAB(18:19) .NE. '  ' ) THEN
         CALL U2MESS('F','UTILITAI4_68')
      ENDIF
C
      CALL JEVEUO (NOMTAB//'.TBLP','L',JTBLP)
      CALL JEVEUO (NOMTAB//'.TBNP','L',JTBNP)
      NBCOL  = ZI(JTBNP  )
      CALL ASSERT (NBCOL.NE.0)
      CALL ASSERT (NBCOL.EQ.NBPARA)
C
       KI = 0
       KR = 0
       KC = 0
       KK = 0
       DO 10 I = 1 , NBCOL
         NOMCOL = ZK24(JTBLP-1+4*(I-1)+1)
         TYPE   = ZK24(JTBLP-1+4*(I-1)+2)
         IF (TYPE(1:1).EQ.'I') THEN
           KI = KI + 1
           IF (NOMPAR.EQ.NOMCOL) THEN
             LIVI(KI) = VI
             GOTO 20
           ENDIF
         ELSEIF (TYPE(1:1).EQ.'R') THEN
           KR = KR + 1
           IF (NOMPAR.EQ.NOMCOL) THEN
             LIVR(KR) = VR
             GOTO 20
           ENDIF
         ELSEIF (TYPE(1:1).EQ.'C') THEN
           KC = KC + 1
           IF (NOMPAR.EQ.NOMCOL) THEN
             LIVC(KC) = VC
             GOTO 20
           ENDIF
         ELSEIF (TYPE(1:1).EQ.'K') THEN
           KK = KK + 1
           IF (NOMPAR.EQ.NOMCOL) THEN
             LIVK(KK) = VK
             GOTO 20
           ENDIF
         ENDIF
 10    CONTINUE
       CALL U2MESK('F','TABLE0_1',1,NOMPAR)
 20    CONTINUE
C
      CALL JEDEMA()
C
      END
