      SUBROUTINE BMNBMD(BASMDZ,OPTINZ,NBOUT)
C MODIF ALGORITH  DATE 19/12/2001   AUTEUR CIBHHPD D.NUNEZ 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C    P. RICHARD     DATE 09/0491/
C-----------------------------------------------------------------------
C  BUT:   < BASE MODALE NOMBRE MODES ET DEFORMEES >
C
C  SI OPTION = 'MODE'   REND LE NOMBRE DE MODES
C
C  SI OPTION = 'TOUT'   REND LE NOMBRE DE MODES ET DEFORMEES
C
C-----------------------------------------------------------------------
C
C BASMDZ   /I/: NOM UTILISATEUR DE LA BASE MODALE
C OPTINZ   /I/: OPTION
C NBOUT    /O/: NOMBRE TROUVE
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*6 PGC
      CHARACTER*8 BASMOD,INTF
      CHARACTER*8 OPTION
      CHARACTER*(*) BASMDZ, OPTINZ

C
C-----------------------------------------------------------------------
      DATA PGC /'BMNBMD'/
C-----------------------------------------------------------------------
C
C--------------------RECUPERATION DU TYPE DE BASE-----------------------
C
      CALL JEMARQ()
      BASMOD = BASMDZ
      OPTION = OPTINZ
C
      CALL JEVEUO(BASMOD//'           .UTIL','L',LLUTI)
      IDESC=ZI(LLUTI)
      NBMOD=ZI(LLUTI+1)
C
C--------------------RECUPERATION INTERF_DYNA---------------------------
C                          EVENTUELLE
      CALL JEVEUO(BASMOD//'           .REFE','L',LLREF)
      INTF=ZK24(LLREF)
C
C--------------------CAS BASE MODALE CLASSIQUE--------------------------
C
      IF(IDESC.EQ.1) THEN
        IF(OPTION.EQ.'MODE') THEN
           NBOUT=NBMOD
           GOTO 9999
        ELSEIF (OPTION.EQ.'TOUT') THEN
           CALL JEVEUO(INTF//'      .INTD.DESC','L',LLDESC)
           NBOUT=NBMOD+ZI(LLDESC+4)
           GOTO 9999
        ELSEIF(OPTION.EQ.'DEFORMEE') THEN
           CALL JEVEUO(INTF//'      .INTD.DESC','L',LLDESC)
           NBOUT=ZI(LLDESC+4)
           GOTO 9999
        ELSE
           CALL UTDEBM('F',PGC,
     &'ARRET SUR QUESTION ILLICITE POUR LE TYPE DE BASE')
          CALL UTIMPK('L',' TYPE DE BASE --> ',1,'CLASSIQUE')
          CALL UTIMPK('L',' QUESTION --> ',1,OPTION)
          CALL UTFINM
        ENDIF
      ENDIF
C
C--------------------CAS BASE MODALE CYCLIQUE--------------------------
C
      IF(IDESC.EQ.2) THEN
        IF(OPTION.EQ.'MODE'.OR.OPTION.EQ.'TOUT') THEN
           NBOUT=NBMOD
           GOTO 9999
        ELSEIF(OPTION.EQ.'DEFORMEE') THEN
           NBOUT=0
        ELSE
           CALL UTDEBM('F',PGC,
     &'ARRET SUR QUESTION ILLICITE POUR LE TYPE DE BASE')
          CALL UTIMPK('L',' TYPE DE BASE --> ',1,'CYCLIQUE')
          CALL UTIMPK('L',' QUESTION --> ',1,OPTION)
          CALL UTFINM
        ENDIF
      ENDIF
C
C
C--------------------CAS BASE MODALE RIT OU DIAG_MASS------------------
C
      IF((IDESC.EQ.3).OR.(IDESC.EQ.4)) THEN
        IF(OPTION.EQ.'TOUT') THEN
           NBOUT=NBMOD
           GOTO 9999
        ELSEIF(OPTION.EQ.'DEFORMEE') THEN
           NBOUT=ZI(LLUTI+3)
           GOTO 9999
        ELSE
           CALL UTDEBM('F',PGC,
     &'ARRET SUR QUESTION ILLICITE POUR LE TYPE DE BASE')
          CALL UTIMPK('L',' TYPE DE BASE --> ',1,'RITZ')
          CALL UTIMPK('L',' QUESTION --> ',1,OPTION)
          CALL UTFINM
        ENDIF
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
