      LOGICAL FUNCTION DIADAP(SDDISC,IOCC,NUMINS)
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
      INTEGER      NUMINS,IOCC
      CHARACTER*19 SDDISC
C      LOGICAL      DIADAP
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C ACCES AU DECLANCHEUR DE L'ADAPTATION DU PAS DE TEMPS
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
      CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'NOM_EVEN',R8B,IB,EVEN)

      IF (EVEN.EQ.'AUCUN') THEN

        DIADAP = .FALSE.

      ELSEIF (EVEN.EQ.'TOUT_INST') THEN
      
        DIADAP = .TRUE.

      ELSEIF (EVEN.EQ.'SEUIL_SANS_FORMULE') THEN

        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'NB_INCR_SEUIL',R8B,NB,K8B)
        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'NOM_PARA',R8B,IB,NOPARA)
        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'CRIT_COMP',R8B,IB,CRICOM)
        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'VALE',VALE,VALI,K8B)

        CALL ASSERT(NOPARA.EQ.'NB_ITER_NEWT')
        CALL JEVEUO(SDDISC//'.ITER','L',JITER)
        
        DIADAP = .TRUE.
C       DES QU'UN CRITERE EST FAUX, ON SORT
        DO 10 I=0,NB-1

          IF  (NUMINS-I .LE. 0) THEN  
C           INSTANT NON CALCULE, ON NE VALIDE PAS
            DIADAP = .FALSE.
            GOTO 999
          ENDIF

          IF (CRICOM.EQ.'LT'.AND.ZI(JITER-1+NUMINS-I).GE.VALI.OR.
     &        CRICOM.EQ.'GT'.AND.ZI(JITER-1+NUMINS-I).LE.VALI.OR.
     &        CRICOM.EQ.'LE'.AND.ZI(JITER-1+NUMINS-I).GT.VALI.OR.
     &        CRICOM.EQ.'GE'.AND.ZI(JITER-1+NUMINS-I).LT.VALI) THEN
            DIADAP = .FALSE.
            GOTO 999
          ENDIF
             
 10     CONTINUE

      ELSEIF (EVEN.EQ.'SEUIL_AVEC_FORMULE') THEN

        CALL ASSERT(0.EQ.1)

      ENDIF

999   CONTINUE      
      CALL JEDEMA()
      END
