      SUBROUTINE CFPOSN(NOMA  ,DEFICO,POSMAM,POSNSM,NBNOM )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/04/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO     
      INTEGER      POSMAM
      INTEGER      POSNSM(9)
      INTEGER      NBNOM
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
C
C INDICES DANS CONTNO DES NOEUDS POUR UNE MAILLE DONNEE
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEFICO : SD DE CONTACT (DEFINITION)
C IN  POSMAM : INDICE DE LA MAILLE MAITRE (DANS SD CONTACT)
C OUT POSNSM : INDICES DANS CONTNO DES NOEUDS MAITRES
C OUT NBNOM  : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
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
      INTEGER      JDECNO,INO,I
      CHARACTER*24 CONTMA,PNOMA,NOMACO
      INTEGER      JMACO,JPONO,JNOMA
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C      
C --- RECUPERATION DE QUELQUES DONNEES      
C
      CONTMA = DEFICO(1:16)//'.MAILCO'
      PNOMA  = DEFICO(1:16)//'.PNOMACO'
      NOMACO = DEFICO(1:16)//'.NOMACO'
C
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(PNOMA, 'L',JPONO)
      CALL JEVEUO(NOMACO,'L',JNOMA)
C
C --- INITIALISATIONS
C
      DO 6 I = 1,9
        POSNSM(I) = 0
 6    CONTINUE            
C
C --- NOMBRE DE NOEUDS
C
      NBNOM  = ZI(JPONO+POSMAM) - ZI(JPONO+POSMAM-1)
      IF (NBNOM.GT.9) THEN
        CALL ASSERT(.FALSE.)
      ENDIF  
C
C --- INDICE DANS CONTMA DES NOEUDS DE LA MAILLE MAITRE
C
      JDECNO = ZI(JPONO+POSMAM-1)
      DO 60 INO = 1,NBNOM
        POSNSM(INO) = ZI(JNOMA+JDECNO+INO-1)
 60   CONTINUE
C
      CALL JEDEMA()
C ----------------------------------------------------------------------
      END
