      SUBROUTINE REFE80(NOMRES)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C    P. RICHARD     DATE 07/03/91
C-----------------------------------------------------------------------
C
C  BUT:  REMPLIR L'OBJET REFE ASSOCIE AU CALCUL CYCLIQUE
C
C-----------------------------------------------------------------------
C
C NOM----- / /:
C
C NOMRES   /I/: NOM UTILISATEUR DU RESULTAT
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
      CHARACTER*32 JEXNOM, JEXNUM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8 NOMRES,BASMOD,INTF,MAILLA
      CHARACTER*10 TYPBAS(3)
      CHARACTER*24 BLANC
      CHARACTER*24 VALK(3)
C
C-----------------------------------------------------------------------
C
      DATA TYPBAS/'CLASSIQUE','CYCLIQUE','RITZ'/
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      BLANC='   '
      BASMOD=BLANC
C
C------------RECUPERATION DU NOMBRE D'OCCURENCES DES MOT-CLE------------
C
      CALL GETVID(BLANC,'BASE_MODALE',1,1,1,BASMOD,IOC1)
C
C------------------CONTROLE SUR TYPE DE BASE MODALE---------------------
C
      CALL JEVEUO(BASMOD//'           .UTIL','L',LLUTI)
      IDESC=ZI(LLUTI)
C
        IF(IDESC.NE.1) THEN
          VALK (1) = BASMOD
          VALK (2) = TYPBAS(IDESC)
          VALK (3) = TYPBAS(1)
          CALL U2MESG('F', 'ALGORITH14_13',3,VALK,0,0,0,0.D0)
        ENDIF
C
C--------------------RECUPERATION DES CONCEPTS AMONTS-------------------
C
      CALL JEVEUO(BASMOD//'           .REFD','L',LLREF)
      INTF=ZK24(LLREF+4)
      CALL DISMOI('F','NOM_MAILLA',INTF,'INTERF_DYNA',IBID,
     &            MAILLA,IRET)
C
C--------------------ALLOCATION ET REMPLISSAGE DU REFE------------------
C
      CALL WKVECT(NOMRES//'.CYCL_REFE','G V K24',3,LDREF)
C
      ZK24(LDREF)=MAILLA
      ZK24(LDREF+1)=INTF
      ZK24(LDREF+2)=BASMOD
C
C
 9999 CONTINUE
      CALL JEDEMA()
      END
