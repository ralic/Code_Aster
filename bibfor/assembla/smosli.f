      SUBROUTINE SMOSLI(STOMOZ,STOLCZ,BASZ,RTBLOC)
      IMPLICIT NONE
      CHARACTER*(*) STOMOZ,STOLCZ,BASZ
      REAL*8 RTBLOC
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
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
C     CALCUL D'UN STOC_LCIEL A PARTIR D'UN STOC_MORSE (POUR CONTENIR
C     LA MEME MATRICE)
C     ------------------------------------------------------------------
C IN  JXIN  K19 STOMOZ     : NOM D'UNE S.D. STOC_MORSE
C IN  JXOUT K19 STOLCZ     : NOM D'UNE S.D. STOC_LCIEL
C IN        K1  BASZ       : BASE DE CREATION POUR STOLCZ
C IN        R   RTBLOC     : TAILLE DES BLOCS DE STOLCI
C     ------------------------------------------------------------------

C     ------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C-----------------------------------------------------------------------
C     VARIABLES LOCALES
      CHARACTER*1  BASE
      CHARACTER*19 STOMOR,STOLCI
      INTEGER  JSMDE,JSCDE,NEQ,NBLOC
      INTEGER JSMHC, JSMDI, JSCDI, JSCHC, JSCBL, JSCIB, HCC
      INTEGER HC,HCMAX,ITBLOC,IEQ,IBLOC,TCUMU,IDIAG,IDIAG1,IMIN,IMAX
C     ------------------------------------------------------------------



      CALL JEMARQ()
      STOMOR=STOMOZ
      STOLCI=STOLCZ
      BASE=BASZ

C     -- ON DETRUIT STOLCI S'IL EXISTE DEJA :
      CALL DETRSD('STOC_LCIEL',STOLCI)


C     -- OBJET .SCDE : C'EST FACILE MAIS INCOMPLET:
      CALL WKVECT(STOLCI//'.SCDE',BASE//' V I',6,JSCDE)
      CALL JEVEUO(STOMOR//'.SMDE','L',JSMDE)
      NEQ=ZI(JSMDE-1+1)
      ZI(JSCDE-1+1)=NEQ

C     -- CALCUL DE ITBLOC :
      ITBLOC = NINT(RTBLOC*1024)


C     -- ALLOCATION DE  .SCHC .SCDI ET .SCIB :
      CALL WKVECT(STOLCI//'.SCHC',BASE//' V I',NEQ,JSCHC)
      CALL WKVECT(STOLCI//'.SCDI',BASE//' V I',NEQ,JSCDI)
      CALL WKVECT(STOLCI//'.SCIB',BASE//' V I',NEQ,JSCIB)


C     1. REMPLISSAGE DE .SCHC .SCDI ET .SCIB
C        CALCUL DE HCMAX, NBLOC:
C     -------------------------------------------------------------
      CALL JEVEUO(STOMOR//'.SMDI','L',JSMDI)
      CALL JEVEUO(STOMOR//'.SMHC','L',JSMHC)

C     1.1  INITIALISATIONS :
      HCMAX=0
      TCUMU=0
      HCC=0

C     1.2  EQUATION 1 :
      NBLOC=1
      HC=1
      HCC=HCC+HC
      ZI(JSCHC-1+1)=HC
      ZI(JSCIB-1+1)=NBLOC
      ZI(JSCDI-1+1)=TCUMU +HC
      HCMAX=MAX(HCMAX,HC)
      TCUMU=TCUMU+HC

C     1.3  EQUATIONS 2, ..., NEQ :
      DO 1, IEQ=2,NEQ
C        -- CALCUL DE HC : HAUTEUR DE LA COLONNE IEQ :
         IDIAG=ZI(JSMDI-1+IEQ)
         IDIAG1=ZI(JSMDI-1+IEQ-1)
         IMIN=ZI(JSMHC-1+ IDIAG1+1)
         IMAX=ZI(JSMHC-1+ IDIAG)
         HC=IMAX-IMIN+1

C        -- PEUT-ON ENCORE STOCKER CETTE COLONNE DANS LE BLOC COURANT ?
         CALL ASSERT(HC.LE.ITBLOC)
         IF (TCUMU+HC.GT.ITBLOC) THEN
           NBLOC=NBLOC+1
           TCUMU=0
         ENDIF

         ZI(JSCHC-1+IEQ)=HC
         ZI(JSCIB-1+IEQ)=NBLOC
         ZI(JSCDI-1+IEQ)=TCUMU +HC
         HCMAX=MAX(HCMAX,HC)
         TCUMU=TCUMU+HC
         HCC=HCC+HC
 1    CONTINUE
      ZI(JSCDE-1+3)=NBLOC
      ZI(JSCDE-1+4)=HCMAX


C     2. ALLOCATION ET REMPLISSAGE DE .SCBL :
C     ----------------------------------------
      CALL WKVECT(STOLCI//'.SCBL',BASE//' V I',NBLOC+1,JSCBL)
      ZI (JSCBL-1+1)=0
      IBLOC=1
      DO 2, IEQ=1,NEQ
         IF (ZI(JSCIB-1+IEQ).GT.IBLOC) THEN
            IBLOC=IBLOC+1
            ZI (JSCBL-1+IBLOC)=IEQ-1
         ENDIF
 2    CONTINUE
      CALL ASSERT(IBLOC.EQ.NBLOC)
      ZI (JSCBL-1+NBLOC+1)=NEQ


C     3. SI TOUTE LA MATRICE TIENT DANS UN SEUL BLOC, ON
C        ADAPTE LA TAILLE DE CE BLOC
C     ------------------------------------------------------
      IF (ITBLOC.GT.HCC) THEN
         CALL ASSERT(NBLOC.EQ.1)
         ITBLOC=HCC
      ENDIF
      ZI(JSCDE-1+2)=ITBLOC


      CALL JEDEMA()
      END
