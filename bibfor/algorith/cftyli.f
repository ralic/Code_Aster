      SUBROUTINE CFTYLI(RESOCO, POSIT, TYPE0)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/05/2003   AUTEUR PABHHHH N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
      IMPLICIT      NONE
      INTEGER       POSIT, TYPE0
      CHARACTER*24  RESOCO
C ======================================================================
C ----------------------------------------------------------------------
C --- BUT : TYPE DE LA LIAISON -----------------------------------------
C ----------------------------------------------------------------------
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
C ======================================================================
      CHARACTER*32       JEXNUM , JEXNOM
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
C ======================================================================
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C ======================================================================
      INTEGER      JVECC
      CHARACTER*2  TYPEC0, TYPEF0, TYPEF1, TYPEF2
      CHARACTER*19 CONVEC
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
      CONVEC = RESOCO(1:14)//'.CONVEC'
      CALL JEVEUO (CONVEC,'L',JVECC )
C ======================================================================
C --- TYPE D'INITIALISATIONS POUR LE CONTACT-FROTTEMENT ----------------
C ======================================================================
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
C ======================================================================
C --- IDENTIFICATION DE LA LIAISON -------------------------------------
C ======================================================================
      IF ( ZK8(JVECC-1+POSIT).EQ.TYPEC0 ) THEN
         TYPE0 = 1
      ELSE IF ( ZK8(JVECC-1+POSIT).EQ.TYPEF0 ) THEN
         TYPE0 = 2
      ELSE IF ( ZK8(JVECC-1+POSIT).EQ.TYPEF1 ) THEN
         TYPE0 = 3
      ELSE IF ( ZK8(JVECC-1+POSIT).EQ.TYPEF2 ) THEN
         TYPE0 = 4
      ENDIF
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
