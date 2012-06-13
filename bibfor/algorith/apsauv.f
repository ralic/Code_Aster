      SUBROUTINE APSAUV(PHASEZ,SDAPPA,IZONE ,IP    ,VALI  ,
     &                  VALR  )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) PHASEZ
      CHARACTER*19  SDAPPA
      INTEGER       IZONE,IP
      INTEGER       VALI(*)
      REAL*8        VALR(*)
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C SAUVEGARDE APPARIEMENT
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE DE STOCKAGE
C                'ND_PROCHE' - NOEUD LE PLUS PROCHE DU POINT
C                'MA_PROCHE' - MAILLE LA PLUS PROCHE DU POINT
C             
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  IZONE  : NUMERO DE LA ZONE
C IN  IP     : NUMERO DU POINT APPARIE
C IN  VALI   : INFO. DE TYPE INTEGER A STOCKER
C                'ND_PROCHE'
C                   VALI(1) =  1  SI APPARIEMENT
C                             -1  SI POINT DANS SANS_NOEUD 
C                             -2  SI AUCUN NOEUD DANS TOLE_APPA
C                   VALI(2) = POSNO POSITION NOEUD (DANS SD) APPARIE
C                              0  SI PAS DE NOEUD APPARIE
C                'MA_PROCHE'
C                   VALI(1) =  2  SI APPARIEMENT
C                             -3  SI POINT HORS MAILLE
C                   VALI(2) = POSMA POSITION MAILLE (DANS SD) APPARIEE
C                               0 SI PAS DE MAILLE APPARIEE
C
C IN  VALR   : INFO. DE TYPE REAL*8 A STOCKER
C                'ND_PROCHE'
C                   VALR(1) = DIST DISTANCE DU NOEUD APPARIE
C                   VALR(2) = VECTEUR PT -> PROJETE
C                   VALR(3) = VECTEUR PT -> PROJETE
C                   VALR(4) = VECTEUR PT -> PROJETE
C                'MA_PROCHE'
C                   VALR(1) = DIST DISTANCE DE LA MAILLE
C                   VALR(2) = KSI1 COORD. PARAM. DE LA PROJ. SUR MAILLE
C                   VALR(3) = KSI3 COORD. PARAM. DE LA PROJ. SUR MAILLE
C                   VALR(4) = TAU1 TANGENTE PROJ. SUR MAILLE
C                   VALR(5) = TAU1 TANGENTE PROJ. SUR MAILLE
C                   VALR(6) = TAU1 TANGENTE PROJ. SUR MAILLE
C                   VALR(7) = TAU2 TANGENTE PROJ. SUR MAILLE
C                   VALR(8) = TAU2 TANGENTE PROJ. SUR MAILLE
C                   VALR(9) = TAU2 TANGENTE PROJ. SUR MAILLE
C                   VALR(10) = VECTEUR PT -> PROJETE
C                   VALR(11) = VECTEUR PT -> PROJETE
C                   VALR(12) = VECTEUR PT -> PROJETE
C
C
C
C
      INTEGER      IFM,NIV
      CHARACTER*24 PHASE
      CHARACTER*24 APPAR
      INTEGER      JAPPA
      CHARACTER*24 APDIST,APTAU1,APTAU2,APPROJ
      INTEGER      JDIST,JTAU1,JTAU2,JPROJ
      INTEGER      NTPT
      INTEGER      POSNO,POSMA
      INTEGER      TYPAPP
      REAL*8       DIST
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('APPARIEMENT',IFM,NIV)     
C
C --- ACCES SDAPPA
C
      APPAR  = SDAPPA(1:19)//'.APPA' 
      CALL JEVEUO(APPAR ,'E',JAPPA ) 
      APDIST = SDAPPA(1:19)//'.DIST'      
      APTAU1 = SDAPPA(1:19)//'.TAU1'
      APTAU2 = SDAPPA(1:19)//'.TAU2'
      CALL JEVEUO(APDIST,'E',JDIST )
      CALL JEVEUO(APTAU1,'E',JTAU1 )
      CALL JEVEUO(APTAU2,'E',JTAU2 )
      APPROJ = SDAPPA(1:19)//'.PROJ'            
      CALL JEVEUO(APPROJ,'E',JPROJ )
C
C --- INFOS SDAPPA
C
      CALL APPARI(SDAPPA,'APPARI_NTPT'   ,NTPT  ) 
C
C --- INITIALISATIONS
C
      PHASE  = PHASEZ
      TYPAPP = 0
      POSNO  = 0
      POSMA  = 0
      CALL ASSERT(IP.LE.NTPT) 
C
C --- SAUVEGARDE INFOS NOEUD LE PLUS PROCHE
C           
      IF (PHASE.EQ.'ND_PROCHE') THEN  
C
C ----- TYPE D'APPARIEMENT
C
        TYPAPP = VALI(1)
        CALL ASSERT((TYPAPP.EQ.-2).OR.(TYPAPP.EQ.-1).OR.
     &              (TYPAPP.EQ.1))
C
C ----- INDICE ET DISTANCE DU NOEUD LE PLUS PROCHE
C 
        POSNO  = VALI(2)
        DIST   = VALR(1)
C
C ----- SAUVEGARDE
C
        ZI(JAPPA+4*(IP-1)+1-1) = TYPAPP
        ZI(JAPPA+4*(IP-1)+2-1) = POSNO
        ZI(JAPPA+4*(IP-1)+3-1) = IZONE
        ZR(JDIST+4*(IP-1)+1-1) = DIST 
        ZR(JDIST+4*(IP-1)+2-1) = VALR(2)
        ZR(JDIST+4*(IP-1)+3-1) = VALR(3)
        ZR(JDIST+4*(IP-1)+4-1) = VALR(4)
C
C --- SAUVEGARDE INFOS MAILLE LA PLUS PROCHE
C           
      ELSEIF (PHASE.EQ.'MA_PROCHE') THEN
C
C --- TYPE D'APPARIEMENT
C
        TYPAPP = VALI(1)
C
C --- INDICE ET DISTANCE DE LA MAILLE LA PLUS PROCHE
C 
        POSMA  = VALI(2)
        DIST   = VALR(1)
C
C --- SAUVEGARDE
C
        ZI(JAPPA+4*(IP-1)+1-1) = TYPAPP
        ZI(JAPPA+4*(IP-1)+2-1) = POSMA
        ZI(JAPPA+4*(IP-1)+3-1) = IZONE 
        ZR(JDIST+4*(IP-1)+1-1) = DIST 
        ZR(JDIST+4*(IP-1)+2-1) = VALR(10)
        ZR(JDIST+4*(IP-1)+3-1) = VALR(11)
        ZR(JDIST+4*(IP-1)+4-1) = VALR(12)
        ZR(JPROJ+2*(IP-1)+1-1) = VALR(2)
        ZR(JPROJ+2*(IP-1)+2-1) = VALR(3)        
        ZR(JTAU1+3*(IP-1)+1-1) = VALR(4)
        ZR(JTAU1+3*(IP-1)+2-1) = VALR(5)
        ZR(JTAU1+3*(IP-1)+3-1) = VALR(6)
        ZR(JTAU2+3*(IP-1)+1-1) = VALR(7)
        ZR(JTAU2+3*(IP-1)+2-1) = VALR(8)
        ZR(JTAU2+3*(IP-1)+3-1) = VALR(9)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
C 
      END
