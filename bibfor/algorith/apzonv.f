      SUBROUTINE APZONV(SDAPPA,IZONE ,QUESTZ,VALR  )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/01/2011   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      CHARACTER*19  SDAPPA
      INTEGER       IZONE 
      REAL*8        VALR(3)
      CHARACTER*(*) QUESTZ
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C INFO. DE TYPE VECEUR DE REELS (3) SUR LA ZONE COURANTE
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  IZONE  : NUMERO DE LA ZONE
C IN  QUESTI : QUESTION
C OUT VALR   : REPONSE A LA QUESTION 
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
      INTEGER      IFM,NIV
      CHARACTER*24 APINZR
      INTEGER      JPINZR
      CHARACTER*24 QUESTI
      INTEGER      APMMVD,ZINZR
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('APPARIEMENT',IFM,NIV)     
C
C --- ACCES SDAPPA
C
      APINZR = SDAPPA(1:19)//'.INZR'
      CALL JEVEUO(APINZR,'L',JPINZR)
C
C --- INITIALISATIONS
C
      QUESTI  = QUESTZ   
      VALR(1) = 0.D0
      VALR(2) = 0.D0
      VALR(3) = 0.D0
      ZINZR   = APMMVD('ZINZR')
C      
C --- REPONSE
C
      IF (QUESTI.EQ.'DIRE_APPA_VECT') THEN
        VALR(1) = ZR(JPINZR+ZINZR*(IZONE-1)+1 -1)
        VALR(2) = ZR(JPINZR+ZINZR*(IZONE-1)+2 -1)
        VALR(3) = ZR(JPINZR+ZINZR*(IZONE-1)+3 -1)
      ELSEIF (QUESTI.EQ.'VECT_MAIT') THEN
        VALR(1) = ZR(JPINZR+ZINZR*(IZONE-1)+6 -1)
        VALR(2) = ZR(JPINZR+ZINZR*(IZONE-1)+7 -1)
        VALR(3) = ZR(JPINZR+ZINZR*(IZONE-1)+8 -1)
      ELSEIF (QUESTI.EQ.'VECT_ESCL') THEN
        VALR(1) = ZR(JPINZR+ZINZR*(IZONE-1)+9 -1)
        VALR(2) = ZR(JPINZR+ZINZR*(IZONE-1)+10-1)
        VALR(3) = ZR(JPINZR+ZINZR*(IZONE-1)+11-1)        
      ELSE
        CALL ASSERT(.FALSE.)  
      ENDIF
C
      CALL JEDEMA()
C 
      END
