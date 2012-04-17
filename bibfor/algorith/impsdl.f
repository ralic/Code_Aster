      SUBROUTINE IMPSDL(SDIMPR,COLONN,UIMPR )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/07/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT      NONE
      CHARACTER*24  SDIMPR
      CHARACTER*(*) COLONN
      INTEGER       UIMPR
C 
C ----------------------------------------------------------------------
C
C ROUTINE AFFICHAGE
C
C IMPRESSION D'UNE LIGNE DU TABLEAU
C      
C ----------------------------------------------------------------------
C      
C
C IN  SDIMPR : SD AFFICHAGE
C IN  COLONN : LIGNE DU TABLEAU AVEC SEPARATION COLONNES
C IN  UIMPR  : UNITE D'IMPRESSION
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER          ICOL,IBID,VALI,UNIBID
      INTEGER          POS,POSFIN,POSMAR
      CHARACTER*16     K16BID,NOVALU,VALK
      REAL*8           R8BID,VALR,R8VIDE
      CHARACTER*1      MARQ
      INTEGER          FORCOL
      CHARACTER*255    TAMPON 
      CHARACTER*24     IMPINF
      INTEGER          JIMPIN      
      INTEGER          NCOL,LARGE
      INTEGER          LONGR,PRECR,LONGI
      CHARACTER*16     TITCOL(3)      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- RECUPERATION ET VERIFICATION DES PARAMETRES
C
      IMPINF = SDIMPR(1:14)//'.INFO'
      CALL JEVEUO(IMPINF,'L',JIMPIN)
      NCOL   = ZI(JIMPIN-1+1)
      LARGE  = ZI(JIMPIN-1+2)
C 
C --- INITIALISATIONS
C     
      TAMPON = COLONN(1:LARGE)
      NOVALU = ' - SANS OBJET - '
      UNIBID = 0
      POS    = 2
      LONGR  = 12
      PRECR  = 5
      LONGI  = 6      
C
C --- CREATION DE L'AFFICHAGE (-> TAMPON)
C      
      DO 100 ICOL = 1, NCOL
        CALL IMPSDA(SDIMPR,'LIRE',ICOL  ,IBID  ,TITCOL,
     &              FORCOL)
        POSFIN = 16+POS-1
        IF (FORCOL.EQ.1) THEN
          CALL IMPSDV(SDIMPR,ICOL  ,K16BID,R8BID ,VALI  ,
     &                MARQ  )
          IF (VALI.EQ.-1) THEN
            TAMPON(POS:POSFIN) = NOVALU(1:16)
          ELSE
            CALL IMPFOI(UNIBID,LONGI ,VALI  ,
     &                  TAMPON(POS:POSFIN))
            IF (MARQ(1:1).NE.' ') THEN
              POSMAR = POS + 16 - 2
              TAMPON(POSMAR:POSMAR) = MARQ(1:1)
            ENDIF
          ENDIF
        ELSE IF (FORCOL.EQ.2) THEN
          POSFIN = POS+16-1
          CALL IMPSDV(SDIMPR,ICOL  ,K16BID,VALR  ,IBID  ,
     &                MARQ  )
     
          IF (VALR.EQ.R8VIDE()) THEN 
            TAMPON(POS:POSFIN) = NOVALU(1:16)
          ELSE
            CALL IMPFOR(UNIBID,LONGR ,PRECR ,VALR  ,
     &                  TAMPON(POS:POSFIN))
            IF (MARQ(1:1).NE.' ') THEN
              POSMAR = POS + 16 - 2
              TAMPON(POSMAR:POSMAR) = MARQ(1:1)
            ENDIF
          ENDIF  
        ELSE IF (FORCOL.EQ.3) THEN
          CALL IMPSDV(SDIMPR,ICOL  ,VALK  ,R8BID ,IBID  ,
     &                MARQ  )
          TAMPON(POS:POSFIN) = VALK(1:16)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        POS = POS + 16 + 1
 100  CONTINUE
C
C --- IMPRESSION
C
      CALL IMPFOK(TAMPON,LARGE ,UIMPR )
C
      CALL JEDEMA()
      END
