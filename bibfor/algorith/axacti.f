      SUBROUTINE AXACTI(BASMOD,NUMA,NBDIAM,LISNU,NBLIS,NBACTI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/06/2005   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C***********************************************************************
C    P. RICHARD     DATE 11/03/91
C-----------------------------------------------------------------------
C  BUT:    < AXE ACTIVITE >
C
C   SUBROUTINE SPECIFIQUE AU CALCUL CYCLIQUE
C
C  PERMET DE DETERMINER LES DDL GENERALISE D'INTERFACE AXE A
C  ASSEMBLE CELON LE NOMBRE DE DIAMETRES MODAUX AINSI QUE LA LISTE
C  DES NUMERO DE DDL AXE CORRESPONDANT (NON PAS NUMERO DANS LA LISTE
C  TOTALE DES DDL GENERALISES MAIS DANS LA LISTE DES DDL AXE)
C
C  SI LA LISTE EN ENTREE EST SOUS DIMENSIONNER ON EN TIENT COMPTE
C
C-----------------------------------------------------------------------
C
C BASMOD   /I/: NOM UTLISATEUR DE LA BASE MODALE
C NUMA     /I/: NUMERO DE L'INTERFACE DEFINISSANT LES POINTS DE L'AXE
C NBDIAM   /I/: NOMBRE DE DIAMETRE MODAUX
C LISNU    /O/: LISTE DES NUMERO DES DL A ASSEMBLER
C NBLIS    /I/: DIMENSION DE LA LISTE EN ENTREE
C NBACTI   /O/: NOMBRE DE DDL AXE A ASSENBLER
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32 JEXNUM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C      NTA EST LE NOMBRE DE CMP TRAITEE EN CYCLIQUE
      PARAMETER (NBCPMX=300)
      CHARACTER*1 K1BID
      CHARACTER*8 BASMOD,INTF,K8BID
      LOGICAL OKASS
      INTEGER IDEC(NBCPMX),LISNU(NBLIS)
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
C
C-------------------RECUPERATION DE LA LISTE-INTERFACE------------------
C
      CALL JEMARQ()
      CALL JEVEUO(BASMOD//'           .REFD','L',LLREF)
      INTF=ZK24(LLREF+4)
C
C----------------RECUPERATION DU NOMBRE D'ENTIERS CODES-----------------
C
      CALL DISMOI('F','NB_CMP_MAX',INTF,'INTERF_DYNA',NBCMP,K8BID,IER)
      CALL DISMOI('F','NB_EC',INTF,'INTERF_DYNA',NBEC,K8BID,IER)
      IF (NBEC.GT.10) THEN
        CALL UTMESS('F','AXACTI',
     &                  'LE DESCRIPTEUR_GRANDEUR DES DEPLACEMENTS'//
     &                  ' NE TIENT PAS SUR DIX ENTIERS CODES')
      ENDIF
C
C-------------------REQUETTE DESCRIPTEUR DES DEFORMEES STATIQUES--------
C
      CALL JEVEUO(INTF//'      .INTD.DEFO','L',LLDESC)
      CALL JELIRA(INTF//'      .INTD.DEFO','LONMAX',NBNOT,K1BID)
C**************************************************************
      NBNOT = NBNOT/(2+NBEC)
C      NBNOT=NBNOT/3
C**************************************************************
C
C
C
C---------------REQUETTE SUR DEFINITION INTEFACES AXE-------------------
C
      CALL JEVEUO(JEXNUM(INTF//'      .INTD.LINO',NUMA),'L',LLNOA)
C
       CALL JELIRA(JEXNUM(INTF//'      .INTD.LINO',NUMA),'LONMAX',
     &NBNOA,K1BID)
C
C--------------------------ON DETERMINE LA LISTE------------------------
C
      ICOMP=0
      NBACTI=0
C
      DO 10 I=1,NBNOA
        INU=ZI(LLNOA+I-1)
C*************************************************************
C        ICOD=ZI(LLDESC+2*NBNOT+INU-1)
        CALL ISDECO(ZI(LLDESC+2*NBNOT+(INU-1)*NBEC+1-1),IDEC,NBCMP)
        DO 20 J=1,NBCMP
C*************************************************************
          OKASS=.FALSE.
          IF(IDEC(J).GT.0) THEN
            ICOMP=ICOMP+1
C
            IF(J.EQ.1.AND.NBDIAM.EQ.1) OKASS=.TRUE.
            IF(J.EQ.2.AND.NBDIAM.EQ.1) OKASS=.TRUE.
            IF(J.EQ.3.AND.NBDIAM.EQ.0) OKASS=.TRUE.
            IF(J.EQ.4.AND.NBDIAM.EQ.1) OKASS=.TRUE.
            IF(J.EQ.5.AND.NBDIAM.EQ.1) OKASS=.TRUE.
            IF(J.EQ.6.AND.NBDIAM.EQ.0) OKASS=.TRUE.
            IF(J.EQ.7.AND.NBDIAM.EQ.0) OKASS=.TRUE.
            IF(J.EQ.8.AND.NBDIAM.EQ.0) OKASS=.TRUE.
            IF(J.EQ.9.AND.NBDIAM.EQ.0) OKASS=.TRUE.
            IF(J.EQ.10.AND.NBDIAM.EQ.0) OKASS=.TRUE.
C
          ENDIF
          IF(OKASS) THEN
            NBACTI=NBACTI+1
            IF(NBACTI.LE.NBLIS) LISNU(NBACTI)=ICOMP
          ENDIF
20      CONTINUE
10    CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA()
      END
