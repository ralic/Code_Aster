      SUBROUTINE LOBS(NBOBSE, NUINS0, LOBSER, INSTAP,
     &                 NUOBSE, LISINS, LISOBS)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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

      IMPLICIT NONE

      CHARACTER*19 LISOBS
      CHARACTER*24 LISINS
      INTEGER NBOBSE, NUINS0, NUOBSE
      REAL*8 INSTAP
      LOGICAL LOBSER
C ----------------------------------------------------------------------
C
C      DOIT-ON FAIRE UNE OBSERVATION 
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------

      INTEGER JINST, JOBSE
      REAL*8 R8PREM
      
      CALL JEMARQ()
      
      LOBSER = .FALSE.
      IF (NBOBSE.NE.0) THEN
         CALL JEVEUO(LISINS(1:19)//'.VALE','L',JINST)
         CALL JEVEUO ( LISOBS, 'L', JOBSE )
         IF ((INSTAP+R8PREM( )).GE.ZR(JINST+NUINS0)) THEN
            NUINS0 = NUINS0 + 1
            IF (ZI(JOBSE-1+NUINS0).EQ.1) THEN
               LOBSER = .TRUE.
               NUOBSE = NUOBSE + 1            
            ENDIF
         ENDIF
      ENDIF
      
      CALL JEDEMA()
      
      END
