      SUBROUTINE APVECT(SDAPPA,QUESTZ,IP    ,VALR  )
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
      INTEGER       IP 
      REAL*8        VALR(3)
      CHARACTER*(*) QUESTZ
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C INTERROGATION DE LA SDAPPA - VECTEUR (LONGEUER 3)
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C IN  QUESTI : QUESTION
C              APPARI_TAU1          : TANGENTE 1 AU PT PROJETE
C              APPARI_TAU2          : TANGENTE 2 AU PT PROJETE
C              APPARI_VECTPM        : VECTEUR PT -> PROJECTION
C              APPARI_NOEUD_TAU1    : TANGENTE 1 AU NOEUD 
C              APPARI_NOEUD_TAU2    : TANGENTE 2 AU NOEUD
C IN  IP     : INDICE DU POINT OU POSITION DU NOEUD
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
      CHARACTER*24 APDIST,APTAU1,APTAU2,APTGNO
      INTEGER      JDIST,JTAU1,JTAU2,JPTGNO
      CHARACTER*24 QUESTI
      INTEGER      POSNO
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()    
C
C --- ACCES SDAPPA
C
      APTAU1 = SDAPPA(1:19)//'.TAU1'
      APTAU2 = SDAPPA(1:19)//'.TAU2'
      APDIST = SDAPPA(1:19)//'.DIST'
      APTGNO = SDAPPA(1:19)//'.TGNO'
C
C --- INITIALISATIONS
C
      VALR(1) = 0.D0
      VALR(2) = 0.D0
      VALR(3) = 0.D0            
      QUESTI = QUESTZ
C
C --- QUESTION
C
      IF (QUESTI.EQ.'APPARI_TAU1') THEN
        CALL JEVEUO(APTAU1,'L',JTAU1 )
        VALR(1)  = ZR(JTAU1+3*(IP-1)+1-1)
        VALR(2)  = ZR(JTAU1+3*(IP-1)+2-1)
        VALR(3)  = ZR(JTAU1+3*(IP-1)+3-1)
C
      ELSEIF (QUESTI.EQ.'APPARI_TAU2') THEN
        CALL JEVEUO(APTAU2,'L',JTAU2 )
        VALR(1)  = ZR(JTAU2+3*(IP-1)+1-1)
        VALR(2)  = ZR(JTAU2+3*(IP-1)+2-1)
        VALR(3)  = ZR(JTAU2+3*(IP-1)+3-1)
C
      ELSEIF (QUESTI.EQ.'APPARI_VECTPM') THEN
        CALL JEVEUO(APDIST,'L',JDIST )
        VALR(1)  = ZR(JDIST+4*(IP-1)+2-1)
        VALR(2)  = ZR(JDIST+4*(IP-1)+3-1)
        VALR(3)  = ZR(JDIST+4*(IP-1)+4-1)
C
      ELSEIF (QUESTI.EQ.'APPARI_NOEUD_TAU1') THEN
        CALL JEVEUO(APTGNO,'L',JPTGNO)
        POSNO    = IP
        VALR(1)  = ZR(JPTGNO+6*(POSNO-1)+1-1)
        VALR(2)  = ZR(JPTGNO+6*(POSNO-1)+2-1)
        VALR(3)  = ZR(JPTGNO+6*(POSNO-1)+3-1)
C
      ELSEIF (QUESTI.EQ.'APPARI_NOEUD_TAU2') THEN
        CALL JEVEUO(APTGNO,'L',JPTGNO)
        POSNO    = IP
        VALR(1)  = ZR(JPTGNO+6*(POSNO-1)+4-1)
        VALR(2)  = ZR(JPTGNO+6*(POSNO-1)+5-1)
        VALR(3)  = ZR(JPTGNO+6*(POSNO-1)+6-1)
C
      ELSE    
        CALL ASSERT(.FALSE.)
      ENDIF             
C
      CALL JEDEMA()
C 
      END
