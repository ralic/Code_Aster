      SUBROUTINE FIIMPR (IFM, NTERM, EXPRES, MESS )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             EXPRES(*), NTERM
      CHARACTER*(*)                      MESS
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 12/09/97   AUTEUR GIBHHCM C.MASSERET 
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
C     ------------------------------------------------------------------
C     IMPRESSION STRUCTURES FONCTIONS INTERPRETEES (DEBUG)
C     ------------------------------------------------------------------
C     AUTEUR : D.SELIGMANN
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
      CHARACTER*24
     +   KNOMOP, KARITE, KPRIOR, KCLASS, KVALIS, KVALR8, KVALC8
      INTEGER
     +   LNOMOP, LARITE, LPRIOR, LCLASS, LVALIS, LVALR8, LVALC8
C     ------------------------------------------------------------------
      COMMON /FICK01/
     +   KNOMOP, KARITE, KPRIOR, KCLASS, KVALIS, KVALR8, KVALC8
      COMMON /FICI01/
     +   LNOMOP, LARITE, LPRIOR, LCLASS, LVALIS, LVALR8, LVALC8
C     ------------------------------------------------------------------
C     ICSTE  : NOMBRE D'INFORMATION ARCHIVES
C     IOPE1  : NOMBRE D'OPERATEURS UNAIRES
C     IOPE2  : NOMBRE D'OPERATEURS BINAIRES
C     LOPE1  : PREMIER NUMERO D'ORDRE DES OPERATEURS UNAIRES
C     LOPE2  : PREMIER NUMERO D'ORDRE DES OPERATEURS BINAIRES
      COMMON /FICL01/ ICSTE , IOPE1, IOPE2, LOPE1, LOPE2
      COMMON /FISY01/ IPARG,IPARD,IVIRG,IEGAL,IPTVI,ILOAD,IPLUS,IFONC
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
      CHARACTER*2   BL2*2
      CHARACTER*8   BL8*8
      CHARACTER*13  BL13*13
      DATA        BL2/'..'/, BL8/'........'/, BL13/'.............'/
C     ------------------------------------------------------------------
C
      WRITE(IFM,*) '<FIIMPR>:  <',MESS,'>'
C
      DO 10 ITERM = 1, NTERM
         I = EXPRES(ITERM)
         IF (ZI(LCLASS-1+I) .EQ. 0 .OR. ZI(LCLASS-1+I) .GT. 30 ) THEN
C            --- OPERATEURS ---
             WRITE(IFM,'(10X,A,I4,A,3A,I2,A,I2,A,I2,A,A ,A,  A    ,A)' )
     +              '<', I,'>',
     +              '<', ZK8(LNOMOP-1+I),'><',ZI(LPRIOR-1+I),'><',
     +                   ZI(LARITE-1+I),'><',
     +                   ZI(LCLASS-1+I),'><', BL8 ,'><', BL13   ,'>'
         ELSEIF (MOD(ZI(LCLASS-1+I),10) .EQ. 1 ) THEN
C            --- CONSTANTE OU VARIABLE ENTIERE ---
             WRITE(IFM,'(10X,A,I4,A,3A, A,A,A ,A,I2,A,I8,A,   A   ,A)' )
     +              '<', I,'>',
     +              '<', ZK8(LNOMOP-1+I),'><', BL2 ,'><', BL2   ,'><',
     +             ZI(LCLASS-1+I),'><', ZI(LVALIS-1+I) ,'><', BL13 ,'>'
         ELSEIF (MOD(ZI(LCLASS-1+I),10) .EQ. 2 ) THEN
C            --- CONSTANTE OU VARIABLE REELLE  ---
             WRITE(IFM,'(10X,A,I4,A,3A, A,A, A,A,I2,A,A ,A,1PE13.6,A)' )
     +              '<', I,'>',
     +              '<', ZK8(LNOMOP-1+I),'><', BL2 ,'><', BL2   ,'><',
     +             ZI(LCLASS-1+I),'><', BL8 ,'><', ZR(LVALR8-1+I) ,'>'
         ELSEIF (MOD(ZI(LCLASS-1+I),10) .EQ. 3 ) THEN
C            --- CONSTANTE OU VARIABLE IDENT   ---
             WRITE(IFM,'(10X,A,I4,A,3A,I2,A,I2,A,I2,A,A ,A,   A   ,A)' )
     +              '<', I,'>',
     +   '<', ZK8(LNOMOP-1+I),'><',ZI(LPRIOR-1+I),'><',
     +        ZI(LARITE-1+I),'><',
     +   ZI(LCLASS-1+I),'><', BL8 ,'><', BL13   ,'>'
         ELSEIF (MOD(ZI(LCLASS-1+I),10) .EQ. 4 ) THEN
C            --- CONSTANTE OU VARIABLE TEXTE    ---
            WRITE(IFM,'(10X,A,I4,A,3A,I2,A,I2,A,I2,A,A ,A,   A   ,A)' )
     +              '<', I,'>',
     +              '<', ZK8(LNOMOP-1+I),'><',ZI(LPRIOR-1+I),'><',
     +                   ZI(LARITE-1+I),'><',
     +                   ZI(LCLASS-1+I),'><', BL8 ,'><', BL13   ,'>'
         ELSEIF (MOD(ZI(LCLASS-1+I),10) .EQ. 5 ) THEN
C            --- CONSTANTE OU VARIABLE COMPLEXE ---
             WRITE(IFM,'(10X,A,I4,A,3A,A,A,A,A,I2,A,A ,A,'//
     +                                        '1PE13.6,1H,,1PE13.6,A)')
     +             '<', I,'>',
     +             '<', ZK8(LNOMOP-1+I),'><', BL2 ,'><', BL2   ,'><',
     +              ZI(LCLASS-1+I),'><', BL8 ,'><', ZC(LVALC8-1+I) ,'>'
         ELSEIF (MOD(ZI(LCLASS-1+I),10) .EQ. 6 ) THEN
C            --- CONSTANTE OU VARIABLE LOGIQUE  ---
             WRITE(IFM,'(10X,A,I4,A,3A, A,A,A ,A,I2,A,I8,A,   A   ,A)' )
     +              '<', I,'>',
     +              '<', ZK8(LNOMOP-1+I),'><', BL2 ,'><', BL2   ,'><',
     +             ZI(LCLASS-1+I),'><', ZI(LVALIS-1+I) ,'><', BL13 ,'>'
         ENDIF
   10 CONTINUE
C
      WRITE(IFM,'(10X,25A)' ) ' --------------------------------------',
     +               ' --------------------------------------------- '
C
      END
