      INTEGER FUNCTION DININS(SDDISC,NUMINS)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/07/2009   AUTEUR GENIAUT S.GENIAUT 
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
C RESPONSABLE MABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER      NUMINS
      CHARACTER*19 SDDISC
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE - DISCRETISATION)
C
C ACCES AU NIVEAU DE SUBDIVISION D'UN INSTANT
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION
C IN  NUMINS : NUMERO DE L'INSTANT
C OUT DININS : NIVEAU DE SUBDIVISION DE L'INSTANT (1=PAS REDECOUPE)
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
      INTEGER      JNIVTP,IBID
      CHARACTER*24 TPSDIN
      CHARACTER*16 METLIS
      REAL*8       R8B
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

C     LA NOTION DE SOUS-NIVEAU N'EXISTE PAS EN GESTION AUTO
      CALL UTDIDT('L',SDDISC,'LIST',IBID,'METHODE',R8B,IBID,METLIS)
      IF (METLIS.EQ.'AUTO') THEN
        DININS = 1
        GOTO 999
      ENDIF
C
C --- ACCES SD LISTE D'INSTANTS
C    
      TPSDIN = SDDISC(1:19)//'.DINI' 
      CALL JEVEUO(TPSDIN,'L',JNIVTP) 
C
      DININS = ZI(JNIVTP+NUMINS)

 999  CONTINUE
      CALL JEDEMA()
      END
