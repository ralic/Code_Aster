      LOGICAL FUNCTION DICREC(SDDISC,QUEST,IOCC,ITEMAX,ERROR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/07/2009   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT
C
      IMPLICIT NONE
      INTEGER      IOCC
      CHARACTER*19 SDDISC
      CHARACTER*(*)QUEST
      LOGICAL      ITEMAX,ERROR
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C ACCES AU CRITERE D'ECHEC (DECOUPAGE PAS DE TEMPS)
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION LOCALE OU 
C IN  NUMINS : NUMERO D'INSTANTS
C OUT DIADAP : DECLENCHEUR VERIFIE ?
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
      REAL*8       R8B,VALE
      INTEGER      IB,NB,I,JITER,VALI
      CHARACTER*8  K8B,CRICOM
      CHARACTER*16 NOPARA 
      CHARACTER*19 EVEN
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

      DICREC = .FALSE.

      CALL UTDIDT('L',SDDISC,'ECHE',IOCC,'NOM_EVEN',R8B,IB,EVEN)

      IF (EVEN.EQ.'DIVERGENCE_ITER'.AND.ITEMAX) THEN
      
        DICREC = .TRUE.

      ELSEIF (EVEN.EQ.'DIVERGENCE_ERRE'.AND.ERROR) THEN
      
        DICREC = .TRUE.

      ELSEIF (EVEN.EQ.'COLLISION') THEN

         CALL ASSERT(0.EQ.1)

      ENDIF

      CALL JEDEMA()
      END
