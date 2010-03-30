      SUBROUTINE SLISMO(STOLCZ,STOMOZ,BASZ)
      IMPLICIT NONE
      CHARACTER*(*) STOMOZ,STOLCZ,BASZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 29/03/2010   AUTEUR BOITEAU O.BOITEAU 
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
C TOLE CRP_4
C     CALCUL D'UN STOC_MORSE A PARTIR D'UN STOC_LCIEL (POUR CONTENIR
C     LA MEME MATRICE)
C
C     REMARQUE : CETTE ROUTINE DEVRAIT ETRE TRES RAREMENT UTILISEE CAR
C     LE STOCKAGE MORSE PRODUIT EST EN GENERAL TROP VOLUMINEUX PUISQU'
C     IL CONTIENT TOUS LES TERMES SOUS LA LIGNE DE CIEL
C     ------------------------------------------------------------------
C IN  JXIN  K19 STOLCZ     : NOM D'UNE S.D. STOC_LCIEL
C IN  JXOUT K19 STOMOZ     : NOM D'UNE S.D. STOC_MORSE
C IN        K1  BASZ       : BASE DE CREATION POUR STOLCZ
C     ------------------------------------------------------------------

C     ------------------------------------------------------------------
      INTEGER*4 ZI4
      COMMON  /I4VAJE/ZI4(1)
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
      INTEGER  JSMDE,JSCDE,NEQ,IEQ,ILIG
      INTEGER JSMHC, JSMDI, JSCHC, HCOL,NTERM,KTERM
C     ------------------------------------------------------------------



      CALL JEMARQ()
      STOLCI=STOLCZ
      STOMOR=STOMOZ
      BASE=BASZ

C     -- ON DETRUIT STOMOR S'IL EXISTE DEJA :
      CALL DETRSD('STOC_MORSE',STOMOR)


      CALL JEVEUO(STOLCI//'.SCDE','L',JSCDE)
      CALL JEVEUO(STOLCI//'.SCHC','L',JSCHC)

      NEQ=ZI(JSCDE-1+1)

C     -- CALCUL DE NTERM :
      NTERM=0
      DO 1, IEQ=1,NEQ
         HCOL=ZI(JSCHC-1+IEQ)
         NTERM=NTERM+HCOL
 1    CONTINUE


C     -- OBJET .SMDE :
      CALL WKVECT(STOMOR//'.SMDE',BASE//' V I',6,JSMDE)
      ZI(JSMDE-1+1)=NEQ
      ZI(JSMDE-1+2)=NTERM
      ZI(JSMDE-1+3)=1


C     -- OBJET .SMDI :
      CALL WKVECT(STOMOR//'.SMDI',BASE//' V I',NEQ,JSMDI)
      NTERM=0
      DO 2, IEQ=1,NEQ
         HCOL=ZI(JSCHC-1+IEQ)
         NTERM=NTERM+HCOL
         ZI(JSMDI-1+IEQ)=NTERM
 2    CONTINUE


C     -- OBJET .SMHC :
      CALL WKVECT(STOMOR//'.SMHC',BASE//' V S',NTERM,JSMHC)
      KTERM=0
      DO 3, IEQ=1,NEQ
         HCOL=ZI(JSCHC-1+IEQ)
         CALL ASSERT(HCOL.LE.IEQ)
         DO 4, ILIG=IEQ-HCOL+1,IEQ
            KTERM=KTERM+1
            ZI4(JSMHC-1+KTERM)=ILIG
 4       CONTINUE
 3    CONTINUE



      CALL JEDEMA()
      END
