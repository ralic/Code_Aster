      SUBROUTINE NMPCIN(MATASS,JCCID )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/04/2010   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*19  MATASS
      INTEGER       JCCID

C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C RETOUR DU POINTEUR SUR LES DDLS ELIMINES PAR AFFE_CHAR_CINE
C      
C ----------------------------------------------------------------------
C
C
C IN  MATASS : MATRICE DU PREMIER MEMBRE ASSEMBLEE
C OUT JCCID  : POINTEUR SUR LES DDLS ELIMINES PAR AFFE_CHAR_CINE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER JREFA
      LOGICAL LVCINE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- LA MATRICE DOIT EXISTER. SINON ON NE NE PEUT PAS DEVINER
C --- LES DDLS IMPOSES PAR AFFE_CHAR_CINE :
C
      CALL JEEXIN(MATASS(1:19)//'.REFA',JREFA)
      IF (JREFA.EQ.0) THEN
        CALL U2MESK('F','ALGELINE2_88',1,MATASS)
      ENDIF 
C
C --- ACCES POINTEUR
C
      CALL JEEXIN(MATASS(1:19)//'.CCID',JCCID)
      LVCINE = (JCCID.GT.0)
      IF (LVCINE) THEN
        CALL JEVEUO(MATASS(1:19)//'.CCID','L',JCCID)
      ELSE 
        CALL ASSERT(.FALSE.)
      ENDIF  
C
      CALL JEDEMA()
      END
