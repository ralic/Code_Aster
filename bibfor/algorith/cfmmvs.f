      SUBROUTINE CFMMVS(RESOCO,NPT   ,JEUX  ,LOCA  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/06/2011   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT NONE
      CHARACTER*24  RESOCO
      CHARACTER*24  JEUX,LOCA
      INTEGER       NPT
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE VERIF)
C
C REMPLISSAGE SD VALE_CONT
C      
C ----------------------------------------------------------------------
C
C      
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  JEUX   : NOM DE LA SD STOCKANT LE JEU
C IN  LOCA   : NUMERO DU NOEUD POUR LE POINT DE CONTACT (-1 SI LE POINT
C              N'EST PAS UN NOEUD ! )
C IN  NPT    : NOMBRE DE POINTS EN MODE VERIF
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C          
      CHARACTER*24 NOCHCO
      INTEGER      JNOCHC
      CHARACTER*19 CNSINR
      INTEGER      JCNSVR,JCNSLR
      INTEGER      JJEUX,JLOCA
      INTEGER      IPT 
      REAL*8       JEU,VARC
      INTEGER      NUMNOE
      INTEGER      CFMMVD,ZRESU
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      ZRESU  = CFMMVD('ZRESU')
C
C --- NOM DU CHAM_NO VALE_CONT
C
      NOCHCO = RESOCO(1:14)//'.NOCHCO'
      CALL JEVEUO(NOCHCO,'L',JNOCHC)
      CNSINR = ZK24(JNOCHC+1-1)(1:19)
C
C --- ACCES AU CHAM_NO_S POUR LE CONTACT
C
      CALL JEVEUO(CNSINR(1:19)//'.CNSV','E',JCNSVR)
      CALL JEVEUO(CNSINR(1:19)//'.CNSL','E',JCNSLR)           
C
C --- ACCES SD PROVISOIRES
C
      CALL JEVEUO(JEUX  ,'L',JJEUX )
      CALL JEVEUO(LOCA  ,'L',JLOCA )
C
C --- REMPLISSAGE
C      
      DO 10 IPT = 1,NPT
C
C ----- INFORMATIONS SUR LE POINT
C
        JEU    = ZR(JJEUX+IPT-1) 
        NUMNOE = ZI(JLOCA+IPT-1)
C
C ----- ETAT DU CONTACT
C         
        IF (JEU.LT.0.D0) THEN
          VARC = 1.D0
        ELSE
          VARC = 0.D0          
        ENDIF
C
C ----- REMPLISSAGE EFFECTIF
C     
        IF (NUMNOE.NE.-1) THEN
          ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+1 ) = VARC
          ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+2 ) = JEU
          ZL(JCNSLR-1+ZRESU*(NUMNOE-1)+1 ) = .TRUE.
          ZL(JCNSLR-1+ZRESU*(NUMNOE-1)+2 ) = .TRUE.
        ENDIF
C
 10   CONTINUE      
C
      CALL JEDEMA()
      END
