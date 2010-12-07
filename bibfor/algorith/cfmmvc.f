      SUBROUTINE CFMMVC(DEFICO,JEUX  ,LOCA  ,ENTI  ,ZONE  ,
     &                  NPT   )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*24  DEFICO
      CHARACTER*24  JEUX,LOCA,ENTI,ZONE
      INTEGER       NPT
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE VERIF)
C
C CREATION SD PROVISOIRE POUR MODE VERIF EN POST-TRAITEMENT
C      
C ----------------------------------------------------------------------
C
C      
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  JEUX   : NOM DE LA SD STOCKANT LE JEU
C IN  ENTI   : NOM DE LA SD STOCKANT LES NOMS DES ENTITES APPARIEES
C IN  ZONE   : NOM DE LA SD STOCKANT LA ZONE A LAQUELLE APPARTIENT LE
C              POINT
C IN  LOCA   : NUMERO DU NOEUD POUR LE POINT DE CONTACT (-1 SI LE POINT
C              N'EST PAS UN NOEUD ! )
C OUT NPT    : NOMBRE DE POINTS EN MODE VERIF
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
      INTEGER      CFDISI,NTPT,NTPC
      INTEGER      JJEUX,JLOCA,JENTI,JZONE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- QUELQUES DIMENSIONS
C
      NTPT   = CFDISI(DEFICO,'NTPT'  )
      NTPC   = CFDISI(DEFICO,'NTPC'  )
      NPT    = NTPT-NTPC
      CALL ASSERT(NPT.GE.1)
C
C --- CREATION SD PROVISOIRES
C
      JEUX  = '&&CFMMVC.JEUX'
      LOCA  = '&&CFMMVC.LOCA'
      ENTI  = '&&CFMMVC.ENTI'
      ZONE  = '&&CFMMVC.ZONE'
      CALL WKVECT(JEUX  ,'V V R'  ,NPT  ,JJEUX )
      CALL WKVECT(LOCA  ,'V V I'  ,NPT  ,JLOCA )
      CALL WKVECT(ENTI  ,'V V K16',NPT*2,JENTI )
      CALL WKVECT(ZONE  ,'V V I'  ,NPT  ,JZONE ) 
C
      CALL JEDEMA()
      END
