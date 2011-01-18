      SUBROUTINE NMCROI(SDOBSE,MOTFAC,NBOCC )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2011   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*19 SDOBSE
      INTEGER      NBOCC
      CHARACTER*16 MOTFAC
C
C --------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (STRUCTURES DE DONNES - OBSERVATION)
C
C LECTURE LISTE DES INSTANTS
C
C ----------------------------------------------------------------------
C
C
C IN  SDOBSE : NOM DE LA SD POUR OBSERVATION
C IN  MOTFAC : MOT-FACTEUR POUR LIRE 
C IN  MOTPAS : MOT-FACTEUR POUR LIRE PAS
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      IOCC
      CHARACTER*1  BASE
      CHARACTER*19 LISTLI
      CHARACTER*2  CHAINE
      CHARACTER*16 MOTPAS
C      
C ----------------------------------------------------------------------
C      
      CALL JEMARQ()                       
C
C --- INITIALISATIONS
C
      BASE   = 'V'
      MOTPAS = 'PAS_OBSE'
C
C --- LECTURE LISTE INSTANTS
C 
      DO 10 IOCC = 1 , NBOCC        
        CALL IMPFOI(0,2,IOCC,CHAINE)        
        LISTLI = SDOBSE(1:14)//CHAINE(1:2)//'.LI'
        CALL NMCRPX(MOTFAC,MOTPAS,IOCC  ,LISTLI,BASE  )
  10  CONTINUE
C
      CALL JEDEMA()
      END
