      SUBROUTINE IMPVEM ( IFI )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 16/12/97   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGERIFI
C ----------------------------------------------------------------------
C
C     ECRITURE DES RESULTATS DES FONCTIONS ENVIMA
C        ASSOCIES A LA MACHINE COURANTE
C
C ----------------------------------------------------------------------
C
      CHARACTER*24   CFT1IS , CFT2IS , CFTIS
      CHARACTER*34   CFT1R8 , CFT2R8 , CFT3R8 , CFTR8
C
      LOGICAL        LCRAEM
      INTEGER        LBISEM , LUISEM
      INTEGER        LOLSEM , LOISEM , LOR8EM , LOC8EM
      INTEGER        NCISEM , NCR8EM
      REAL*8         R8BAEM , R8PREM , R8MAEM , R8MIEM , R8GAEM , R8NNEM
      REAL*8         RMIREM , RMAREM , RMINEM , RMAXEM
      INTEGER        ISMAEM , ISNNEM , LOUAEM , LOFIEM , ISPBEM
      REAL*8         R8VIDE , R8PI   , R8DEPI , R8DGRD , R8RDDG
C
      PARAMETER    ( NTEST = 32 )
      CHARACTER*38   LABEL(NTEST)
C
      DATA ( LABEL(I), I=1,7) /
     1   'LCRAEM  MACHINE  CRAY   ',
     2   'LBISEM  ENTIER   INTEGER    ',
     3   'LUISEM  ENTIER   INTEGER    ',
     4   'LOLSEM  LOGIQUE  LOGICAL    ',
     5   'LOISEM  ENTIER   INTEGER    ',
     6   'LOR8EM  REEL     REAL*8     ',
     7   'LOC8EM  COMPLEXE COMPLEX*16 ' /
C
      DATA ( LABEL(I), I=8,12) /
     1   'NCISEM  ENTIER   INTEGER    ',
     2   'NCR8EM  REEL     REAL*8     ',
     3   'LOUAEM  EN OCTETS      ',
     4   'LOFIEM  EN OCTETS      ',
     5   'MOFIEM  EN OCTETS      '   /
C
      DATA ( LABEL (I), I=13,14 ) /
     5   'ISMAEM  ENTIER   INTEGER    ',
     6   'ISNNEM  ENTIER   INTEGER    '  /
C
      DATA ( LABEL(I), I=15,25 )     /
     7   'R8BAEM  BASE NUMERATION    REAL*8    ',
     8   'R8PREM  PRECISION RELATIVE REAL*8    ',
     9   'R8MAEM  MAXIMAL            REAL*8    ',
     +   'R8MIEM  MINIMAL            REAL*8    ',
     1   'R8GAEM  GAMME              REAL*8    ',
     2   'R8NNEM  NOT A NUMBER       REAL*8    ',
     3   'R8VIDE  VIDE               REAL*8    ',
     4   'RMIREM  B**-T'                        ,
     5   'RMAREM  B**(1-T)'                     ,
     6   'RMINEM  B**(EMIN-1)'                  ,
     7   'RMAXEM  B**EMAX(1-B**(-T))'           /
C
      DATA ( LABEL(I), I=26,29 )     /
     1   'R8PI    REEL  REAL*8     ',
     2   'R8DEPI  REEL  REAL*8     ',
     3   'R8DGRD  REEL  REAL*8     ',
     4   'R8RDDG  REEL  REAL*8     '  /
C    ------------------------------------------------------
C       FORMATS AUTOMATIQUES
C  123456789012345678901234
      CFT1IS = '(1X,A,IX,2X,Z16)'
      CFT2IS = '(1X,A,IXX,2X,Z16)'
      IF ( NCISEM() .LT. 9 ) THEN
         WRITE ( CFT1IS( 8: 8) , '(I1)' ) NCISEM() + 1
         CFTIS = CFT1IS
      ELSE
         WRITE ( CFT2IS( 8: 9) , '(I2)' ) NCISEM() + 1
         CFTIS = CFT2IS
      ENDIF
C  12345678901234567890123456789012345
      CFT1R8 = '(1X,A,1PDX.X,  2X,Z16)'
      CFT2R8 = '(1X,A,1PDXX.X, 2X,Z16)'
      CFT3R8 = '(1X,A,1PDXX.XX,2X,Z16)'
C  12345678901234567890123456789012345
      IF ( NCR8EM()+8 .LT. 10 ) THEN
         WRITE ( CFT1R8(10:10) , '(I1)' ) NCR8EM()+8
         WRITE ( CFT1R8(12:12) , '(I1)' ) NCR8EM()-1
         CFTR8 = CFT1R8
      ELSE
         WRITE ( CFT2R8(10:11) , '(I2)' ) NCR8EM()+8
         WRITE ( CFT3R8(10:11) , '(I2)' ) NCR8EM()+8
         IF ( NCR8EM()-1 .LT. 10 ) THEN
            WRITE ( CFT2R8(13:13) , '(I1)' ) NCR8EM()-1
            CFTR8 = CFT2R8
         ELSE
            WRITE ( CFT3R8(13:14) , '(I2)' ) NCR8EM()-1
            CFTR8 = CFT3R8
         ENDIF
      ENDIF

C    ------------------------------------------------------
C
      WRITE ( IFI , '(/,(A))' )
     &   '-------------------------------------------------' ,
     &   '---- ENVIMA VERSION 97 MULTI MACHINES    --------' ,
     &   '-------------------------------------------------'
C
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- MACHINE UTILISEE'
      WRITE ( IFI , '((1X,A,L4))' )
     +   LABEL( 1) , LCRAEM() 
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- LONGUEUR EN BITS'
      WRITE ( IFI , '((1X,A,I3,2X))' ) LABEL( 2) , LBISEM()
C
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- LONGUEUR EN UNITES D''ADRESSAGE DE LA MACHINE'
      WRITE ( IFI , '((1X,A,I3))' )    LABEL( 3) , LUISEM()
C
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- LONGUEUR EN OCTETS'
      WRITE ( IFI , '((1X,A,I3))' )
     +   LABEL( 4) , LOLSEM() ,
     +   LABEL( 5) , LOISEM() ,
     +   LABEL( 6) , LOR8EM() ,
     +   LABEL( 7) , LOC8EM()
C
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- NOMBRE DE CHIFFRES SIGNIFICATIFS'
      WRITE ( IFI , '((1X,A,I3))' )
     +   LABEL( 8) , NCISEM() ,
     +   LABEL( 9) , NCR8EM()
C
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- LONGUEUR UNITE D''ADRESSAGE'
      WRITE ( IFI , '((1X,A,I3))' )
     +   LABEL(10) , LOUAEM()
C
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- LONGUEUR ET TAILLE DE FICHIER'
      WRITE ( IFI , '((1X,A,I10))' )  LABEL(11) , LOFIEM()
      WRITE ( IFI , '((1X,A,I10,2X,I10,1X,A))' )
     &   LABEL(12) , MOFIEM()
C
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- ENTIER STANDARD'
      WRITE ( IFI , CFTIS )
     +   LABEL(13) , ISMAEM() , ISMAEM() ,
     +   LABEL(14) , ISNNEM() , ISNNEM()
C
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- REAL*8'
C
      WRITE ( IFI , '(/,A,'' '',A/)' )
     &   ' FORMAT D''IMPRESSION DES FLOTTANTS',CFTR8
C
      WRITE ( IFI , CFTR8 )
     +   LABEL(15) , R8BAEM() , R8BAEM() ,
     +   LABEL(16) , R8PREM() , R8PREM() ,
     +   LABEL(17) , R8MAEM() , R8MAEM() ,
     +   LABEL(18) , R8MIEM() , R8MIEM() ,
     +   LABEL(19) , R8GAEM() , R8GAEM() ,
     +   LABEL(20) , R8NNEM() , R8NNEM() ,
     +   LABEL(21) , R8VIDE() , R8VIDE() ,
     +   LABEL(22) , RMIREM() , RMIREM() ,
     +   LABEL(23) , RMAREM() , RMAREM() ,
     +   LABEL(24) , RMINEM() , RMINEM() ,
     +   LABEL(25) , RMAXEM() , RMAXEM()
C
      WRITE ( IFI , '(/,A,/,A,/)' )
     &   ' ----- POIDS DES BITS 1 A LBIS' ,
     &   ' ISPBEM  BITS NUMEROTES DE DROITE A GAUCHE'
      DO 10 I = 1 , LBISEM()-1
         WRITE ( IFI , '(1X,I3,2X,I20)' ) I , ISPBEM(I)
   10 CONTINUE
C ----------------------------------------------------------------------
      WRITE ( IFI , '(/,A,/)' )
     &   ' ----- VALEURS PARTICULIERES  PI, DEPI, ...    '
      WRITE ( IFI , CFTR8 )
     +   LABEL(26) , R8PI()   , R8PI()   ,
     +   LABEL(27) , R8DEPI() , R8DEPI() ,
     +   LABEL(28) , R8DGRD() , R8DGRD() ,
     +   LABEL(29) , R8RDDG() , R8RDDG()
C
      WRITE ( IFI , '(/,A)' )
     &   '-------------------------------------------------' ,
     &   '---- FIN TEST ENVIMA MULTI MACHINES -------------' ,
     &   '   OK     ' ,
     &   '-------------------------------------------------' ,
     &   ' '
      END
