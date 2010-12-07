      SUBROUTINE LISLFC(EXCIT ,ICHAR ,INDIC ,IEXCIT,NEXCI ,
     &                  LFCPLX,LACCE ,FCTCSR,NOMFCT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/01/2010   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      LOGICAL      LFCPLX,LACCE
      INTEGER      ICHAR,INDIC
      INTEGER      IEXCIT,NEXCI
      CHARACTER*19 EXCIT
      CHARACTER*8  FCTCSR
      CHARACTER*8  NOMFCT
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (LISTE_CHARGES)
C
C NOM DE LA FONCTION MULTIPLICATRICE
C
C ----------------------------------------------------------------------
C
C
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      JFCHA2,JINFC2,JLCHA2
      CHARACTER*24 K24BID
      INTEGER      NFCPLX,NFREEL 
      INTEGER      NCCPLX,NCREEL  
      INTEGER      NFACCE   
      CHARACTER*4  KNUM
      COMPLEX*16   CCOEF
      REAL*8       RCOEF,ICOEF
      CHARACTER*19 NOMF19
      INTEGER      JVAL,IRET
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES SD
C
      IF (IEXCIT.EQ.0) THEN
        CALL JEVEUO(EXCIT(1:19)//'.INFC','L',JINFC2)
        CALL JEVEUO(EXCIT(1:19)//'.LCHA','L',JLCHA2)
        CALL JEVEUO(EXCIT(1:19)//'.FCHA','L',JFCHA2)  
      ENDIF      
C
C -------- FONCTIONS MULTIPLICATIVES DES CHARGES
C
      IF (LFCPLX ) THEN

        CALL GETVID('EXCIT','FONC_MULT_C',ICHAR,1,1,NOMFCT,NFCPLX)
        CALL GETVID('EXCIT','FONC_MULT'  ,ICHAR,1,1,NOMFCT,NFREEL)

        IF ((NFCPLX.EQ.0).AND.(NFREEL.EQ.0)) THEN
          CALL CODENT(ICHAR   ,'D0',KNUM  )
          NOMFCT = '&&NC'//KNUM
          
          CALL GETVC8('EXCIT','COEF_MULT_C',ICHAR,1,1,CCOEF,NCCPLX)
          IF (NCCPLX.EQ. 0 ) THEN
            CALL GETVR8('EXCIT','COEF_MULT',ICHAR,1,1,RCOEF,NCREEL)
            CALL ASSERT(NCREEL.EQ.0)
            CALL FOCSTE(NOMFCT,'TOUTRESU',RCOEF  ,'V')
          ELSE
            RCOEF   = DBLE ( CCOEF )
            ICOEF   = DIMAG( CCOEF )
            CALL FOCSTC(NOMFCT,'TOUTRESU',RCOEF,ICOEF,'V')
          ENDIF
        ENDIF
        
      ELSE
        IF (IEXCIT.EQ.0) THEN 
          IF (ZK24(JFCHA2+ICHAR-1)(1:1).EQ.'&') THEN
            NFREEL = 0
          ELSE
            NFREEL = 1
          ENDIF
        ELSEIF (IEXCIT.EQ.1) THEN
          CALL GETVID('EXCIT','FONC_MULT',INDIC,1,1,K24BID,NFREEL)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

        IF (LACCE) THEN
          CALL GETVID('EXCIT','ACCE',INDIC,1,1,K24BID,NFACCE)
        ELSE
          NFACCE = 0
        END IF
C
C -------- PAS DE FONCTIONS MULTIPLICATRICES -> CREATION FCT CSTE = 1
C
        IF (NFREEL.EQ.0 .AND. NFACCE.EQ.0) THEN               
          NOMF19 = FCTCSR
          CALL JEEXIN(NOMF19//'.PROL',IRET)
          IF (IRET.EQ.0) THEN
            RCOEF  = 1.D0
            CALL FOCSTE(FCTCSR,'TOUTRESU',RCOEF  ,'V')
          END IF
          NOMFCT = FCTCSR

        ELSE
          IF (NFREEL.NE.0) THEN
            IF (IEXCIT.EQ.0) THEN
              NOMFCT = ZK24(JFCHA2+ICHAR-1)(1:8)
            ELSEIF (NEXCI.NE.0) THEN
              CALL GETVID('EXCIT','FONC_MULT',INDIC,1,1,
     &                    NOMFCT,NFREEL)
            ENDIF
          ENDIF

          IF (NFACCE.NE.0) THEN
            CALL GETVID('EXCIT','ACCE',INDIC,1,1,
     &                   NOMFCT,NFACCE)
          ENDIF
                      
        ENDIF      
      ENDIF  
C  
      CALL JEDEMA()
      END
