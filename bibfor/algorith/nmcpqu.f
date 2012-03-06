      SUBROUTINE NMCPQU(COMPOR,NOMCMZ,NOMPAZ,EXIST  )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/03/2012   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*19  COMPOR
      CHARACTER*(*) NOMCMZ
      CHARACTER*(*) NOMPAZ
      LOGICAL       EXIST
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C INTERROGATION DE LA CARTE COMPOR
C      
C ----------------------------------------------------------------------
C
C IN  COMPOR : CARTE COMPORTEMENT
C IN  NOMCMP : NOM DE LA CMP DE LA GRANDEUR COMPOR QUE L'ON VEUT TESTER
C               'RELCOM': EST-CE QUE CE COMPORTEMENT EXISTE ?
C               'DEFORM': EST-CE QUE DEFORM = NOMPAR
C               'C_PLAN': EST-CE QUE ALGO_C_PLAN OU ALGO_1D =DEBORST
C                ETC...
C IN  NOMPAZ : NO DU PARAMETRE INTERROGE (PAR EX. NOM DU COMPORTEMENT
C                RECHERCHE'
C OUT EXIST  : REPONSE A LA NOMCMPON
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
      CHARACTER*24 NOMCMP
      CHARACTER*16 NOMPAR,COMP
      INTEGER      IRET,IMA,JDECAL
      INTEGER      JCESD,JCESL,JCESV
      INTEGER      NBMA
      CHARACTER*19 COTO,COPM
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      NOMCMP = NOMCMZ
      NOMPAR = NOMPAZ
      EXIST  = .FALSE.
      COTO   = '&&NMCPQU.COTO'
      COPM   = '&&NMCPQU.COPM'
C
C --- TRANSFO CARTE EN CHAM_ELEM_S
C
      CALL CARCES(COMPOR,'ELEM',' ','V',COTO,IRET)
C
C --- REDUCTION SUR COMPOSANTE
C
      CALL CESRED(COTO,0,0,1,NOMCMP,'V',COPM)
      CALL DETRSD('CHAM_ELEM_S',COTO)
C
C --- ACCES CHAM_ELEM_S
C        
      CALL JEVEUO(COPM(1:19)//'.CESD','L',JCESD)
      CALL JEVEUO(COPM(1:19)//'.CESL','L',JCESL)
      CALL JEVEUO(COPM(1:19)//'.CESV','L',JCESV)
      NBMA   = ZI(JCESD-1+1)   
      
      DO 60 IMA = 1,NBMA
        CALL CESEXI('C',JCESD,JCESL,IMA,1,1,1,JDECAL)
        COMP   = ZK16(JCESV-1+JDECAL)
        IF (COMP.EQ.NOMPAR) THEN
          EXIST = .TRUE.
          GOTO 99
        ENDIF
  60  CONTINUE
C
  99  CONTINUE
C      
      CALL DETRSD('CHAM_ELEM_S',COPM)
C      
      CALL JEDEMA()
C   
      END
