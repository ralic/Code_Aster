      LOGICAL FUNCTION NMRCYC(SDDISC,ITERAT,PREC)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER ITERAT
      REAL*8 PREC
      CHARACTER*19 SDDISC

C ----------------------------------------------------------------------
C       REPERAGE DE CYCLES DANS UNE SEQUENCE DE RESIDUS
C ----------------------------------------------------------------------
C IN  SDDISC SD DISCRETISATION
C IN  ITERAT ITERATION COURANTE (DONT LE RESIDU N'EST PAS CALCULE)
C IN  PREC   TOLERANCE POUR LA RECHERCHE DES CYCLES (QQ POURCENTS)

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

      INTEGER ITEMAX,MAXSEQ,LENSEQ,FINSEQ,OFFSET,JRES
      REAL*8 RES1,RES2
      CHARACTER*24 RESIDU
C ----------------------------------------------------------------------
      DATA RESIDU /'&&NMRCYC.RESIDU'/      
C ----------------------------------------------------------------------

C    INITIALISATION
      CALL JEMARQ()
      NMRCYC = .FALSE.
      ITEMAX = ITERAT-1
      MAXSEQ = (ITEMAX+1)/2
      IF (MAXSEQ.LE.1) GOTO 9999
   
C    LECTURE DES RESIDUS DE L'ITERATION 0 A ITERAT   
      CALL WKVECT(RESIDU,'V V R',ITEMAX,JRES)
      CALL NMLERE(SDDISC,'L','VMAXI_TOUS',ITEMAX,ZR(JRES))

C    RECHERCHE DE CYCLES DE LONGUEUR LENSEQ
      DO 10 LENSEQ = 2,MAXSEQ
        DO 20 FINSEQ = LENSEQ,ITEMAX-LENSEQ
          DO 30 OFFSET = 0,LENSEQ-1
            RES1 = ZR(JRES+ITEMAX-OFFSET)
            RES2 = ZR(JRES+FINSEQ-OFFSET)
            IF (ABS(RES1-RES2)/MAX(RES1,RES2) .GT. PREC) GOTO 1000
 30       CONTINUE
          NMRCYC = .TRUE.
          GOTO 2000
 1000     CONTINUE
 20     CONTINUE
 10   CONTINUE
 2000 CONTINUE
 
      CALL JEDETR(RESIDU)     
 9999 CONTINUE
      CALL JEDEMA()
      END
