      SUBROUTINE INIOBS(NBOBSE, NUINS0, LOBSER, INSTAM, RESULT,
     &                   NUOBSE, NOMTAB, KCHAM,  KCOMP,  KNUCM, 
     &                   KNOEU,  KMAIL,  KPOIN,  MAILL2, NBOBAR,
     &                   LISINS, LISOBS)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/02/2003   AUTEUR PABHHHH N.TARDIEU 
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

      CHARACTER*8  RESULT,MAILL2
      CHARACTER*19  NOMTAB, LISOBS
      CHARACTER*24 LISINS
      INTEGER NBOBSE, NUINS0,NUOBSE, KCHAM,  KCOMP, KNUCM
      INTEGER KNOEU, KMAIL,KPOIN, NBOBAR
      REAL*8 INSTAM
      LOGICAL LOBSER
C ----------------------------------------------------------------------
C
C      INITIALISATION DES OBSERVATIONS
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

      INTEGER NLISIN,KKKMA,NOINS2,JINST,JOBSE
      REAL*8 EPSI
      CHARACTER*8  K8BID
      
      CALL JEMARQ()
      
      NBOBSE = 0
      NUINS0 = 1      
      LOBSER = .FALSE.

      CALL GETVID('INCREMENT','LIST_INST',1,1,1,LISINS,NLISIN)
      IF (NLISIN.NE.0) THEN
         CALL JEVEUO(LISINS(1:19)//'.VALE','L',JINST)
         CALL JELIRA(LISINS(1:19)//'.VALE','LONUTI',NOINS2,K8BID)
         EPSI = 1.D-4
         CALL RNLIR8(LISINS,INSTAM,EPSI,NUINS0)
         LISOBS = '&&OP0070.OBSERVATIO'
         CALL DYOBSE (NOINS2,LISINS,LISOBS,NBOBSE,RESULT)
         CALL JEVEUO ( LISOBS, 'L', JOBSE )
      ENDIF

      IF ( NBOBSE .NE. 0 ) THEN
         NUOBSE = 0
         NOMTAB = ' '
         CALL LTNOTB ( RESULT, 'OBSERVATION', NOMTAB )
         CALL JEVEUO ( '&&DYOBSE.MAILLA'   , 'L' , KKKMA )
         CALL JEVEUO ( '&&DYOBSE.NOM_CHAM' , 'L' , KCHAM )
         CALL JEVEUO ( '&&DYOBSE.NOM_CMP ' , 'L' , KCOMP )
         CALL JEVEUO ( '&&DYOBSE.NUME_CMP' , 'L' , KNUCM )
         CALL JEVEUO ( '&&DYOBSE.NOEUD'    , 'L' , KNOEU )
         CALL JEVEUO ( '&&DYOBSE.MAILLE'   , 'L' , KMAIL )
         CALL JEVEUO ( '&&DYOBSE.POINT'    , 'L' , KPOIN )
         MAILL2 = ZK8(KKKMA)
         CALL JELIRA('&&DYOBSE.NOM_CHAM','LONUTI',NBOBAR,K8BID)
      ENDIF
      
      CALL JEDEMA()
      
      END
