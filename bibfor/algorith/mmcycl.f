      SUBROUTINE MMCYCL(DEFICO,RESOCO,TYPCYC,LIECYC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/02/2012   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*16 LIECYC,TYPCYC
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE)
C
C RECUPERATION DU PREMIER POINT OU IL Y A CYCLAGE
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  RESOCO : SD DE RESOLUTION DU CONTACT
C OUT TYPCYC : TYPE DE CYCLAGE
C              ' '               - AUCUN CYCLAGE
C              'CYCLAGE CONT'    - CYCLE CONTACT/PAS CONTACT
C              'CYCLAGE ADH/GLI' - CYCLE ADHERENT/GLISSANT
C              'CYCLAGE AV/AR'   - CYCLE GLISSANT AVANT/ARRIERE
C              'FLIP FLOP'       - FLIP FLOP HISTORIQUE
C OUT LIECYC : LIEU DE CYCLAGE
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
      INTEGER      CFDISI,NTPC
      INTEGER      IPTC
      CHARACTER*24 CYCTYP,CYCPOI
      INTEGER      JCYTYP,JCYPOI
      INTEGER      TCYCLE
      CHARACTER*16 LCYCLE
      LOGICAL      DETECT,CFDISL,LFROT
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES OBJETS
C
      CYCTYP = RESOCO(1:14)//'.CYCTYP'
      CYCPOI = RESOCO(1:14)//'.CYCPOI'
      CALL JEVEUO(CYCTYP,'L',JCYTYP)
      CALL JEVEUO(CYCPOI,'L',JCYPOI)
C
C --- INITIALISATIONS
C
      NTPC   = CFDISI(DEFICO,'NTPC')
      LFROT  = CFDISL(DEFICO,'FROTTEMENT')
      DETECT = .FALSE.
      TYPCYC = ' '
      LIECYC = ' '
C
C --- DETECTION DU PREMIER POINT OU IL Y A CYCLAGE
C
      DO 50 IPTC = 1,NTPC
        TCYCLE = ZI(JCYTYP-1+4*(IPTC-1)+4)
        LCYCLE = ZK16(JCYPOI-1+4*(IPTC-1)+4)
        DETECT = TCYCLE.EQ.1
        IF (DETECT) THEN
          TYPCYC = 'FLIP FLOP'
          LIECYC = LCYCLE
          GOTO 99
        ENDIF
        TCYCLE = ZI(JCYTYP-1+4*(IPTC-1)+1)
        LCYCLE = ZK16(JCYPOI-1+4*(IPTC-1)+1)
        DETECT = TCYCLE.EQ.1
        IF (DETECT) THEN
          TYPCYC = 'CYCLAGE CONT'
          LIECYC = LCYCLE
          GOTO 99
        ENDIF
        IF (LFROT) THEN
          TCYCLE = ZI(JCYTYP-1+4*(IPTC-1)+2)
          LCYCLE = ZK16(JCYPOI-1+4*(IPTC-1)+2)
          DETECT = TCYCLE.EQ.1
          IF (DETECT) THEN
            TYPCYC = 'CYCLAGE ADH/GLI'
            LIECYC = LCYCLE
            GOTO 99
          ENDIF
          TCYCLE = ZI(JCYTYP-1+4*(IPTC-1)+3)
          LCYCLE = ZK16(JCYPOI-1+4*(IPTC-1)+3)
          DETECT = TCYCLE.EQ.1
          IF (DETECT) THEN
            TYPCYC = 'CYCLAGE AV/AR'
            LIECYC = LCYCLE
            GOTO 99
          ENDIF
        ENDIF
  50  CONTINUE
C
  99  CONTINUE
C
      CALL JEDEMA()
      END
