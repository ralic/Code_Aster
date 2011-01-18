      SUBROUTINE NMCRPO(NOMSD ,NUME  ,INST  ,LSELEC)
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
      IMPLICIT      NONE
      CHARACTER*19  NOMSD
      REAL*8        INST
      INTEGER       NUME
      LOGICAL       LSELEC
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (UTILITAIRE - SELEC. INST.)
C
C L'INSTANT CHOISI DE VALEUR <INST> ET D'ORDRE <NUME> EST-IL 
C SELECTIONNE DANS LA LISTE D'INSTANTS OU PAR UN PAS 
C DE FREQUENCE DONNE ?
C
C ----------------------------------------------------------------------
C
C
C IN  NOMSD  : NOM DE LA SD 
C IN  INST   : INSTANT COURANT
C IN  NUME   : ORDRE DE L'INSTANT COURANT
C OUT LSELEC : .TRUE. SI INSANT SELECTIONNE
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
      REAL*8       TOLE,TOLR
      INTEGER      NBINST,FREQ
      CHARACTER*24 SDINFL   
      INTEGER      JINFL
      CHARACTER*4  TYPSEL
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      LSELEC = .FALSE.
      TYPSEL = 'NONE'
C
C --- ACCES A LA SD
C
      SDINFL = NOMSD(1:19)//'.INFL'
      CALL JEVEUO(SDINFL,'L',JINFL)
C
C --- INFORMATIONS        
C
      FREQ   = NINT(ZR(JINFL-1+1))
      TOLE   =      ZR(JINFL-1+2)
      NBINST = NINT(ZR(JINFL-1+3))
      IF (TOLE.GT.0.D0) THEN
        TOLR = INST*TOLE
      ELSE
        TOLR = ABS(TOLE)
      ENDIF
C
C --- TYPE DE SELECTION (INSTANT OU FREQUENCE)
C
      IF (FREQ.EQ.0) THEN
        TYPSEL = 'INST'
      ELSE
        TYPSEL = 'FREQ'
      ENDIF
C
C --- RECHERCHE        
C
      CALL NMCRIT(NOMSD ,NBINST,TYPSEL,NUME  ,INST  ,
     &            FREQ  ,TOLR  ,LSELEC)
      
C
      CALL JEDEMA()

      END
