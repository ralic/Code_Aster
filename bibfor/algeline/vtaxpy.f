      SUBROUTINE VTAXPY(ALPHA,CHAMNA,CHAMNB)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 14/03/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  ENCAPSULATION DAXPY SUR LES .VALE DES CHAM_NO
C                          CHAMN1 ET CHAMN2
C                       CHAMN2.VALE = ALPHA * CHAMN1.VALE + CHAMN2.VALE
C     CETTE ROUTINE EST ADAPTEE A DES CHAM_NOS FETI TRAITES EN PARALLELE
C     ------------------------------------------------------------------
C     IN  ALPHA     :  R8  : COEFF. MULTIPLICATEUR
C     IN  CHAMNA    :  K*  : CHAM_NO MAITRE 1
C     IN/OUT CHAMNB :  K*  : CHAM_NO MAITRE 2
C----------------------------------------------------------------------
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*(*) CHAMNA,CHAMNB
      REAL*8        ALPHA

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      
C DECLARATION VARIABLES LOCALES
      INTEGER      NBSD,ILIMPI,IFETC1,IFETC2,IDD,NEQ,IVAL1,IVAL2,IRET1,
     &             IRET2
      CHARACTER*8  K8BID
      CHARACTER*24 KVAL1,KVAL2,CHAMN1,CHAMN2
      LOGICAL      IDDOK,LFETI

C CORPS DU PROGRAMME
      CALL JEMARQ()
      CHAMN1=CHAMNA
      CHAMN2=CHAMNB

C --- TEST POUR SAVOIR SI LE SOLVEUR EST DE TYPE FETI

      CALL JEEXIN(CHAMN1(1:19)//'.FETC',IRET1)
      IF (IRET1.NE.0) THEN
        CALL JEEXIN(CHAMN2(1:19)//'.FETC',IRET2)
        IF (IRET2.EQ.0)
     &    CALL UTMESS('F','FETAXP','CHAM_NO NON FETI !')
        LFETI=.TRUE.
      ELSE
        LFETI=.FALSE.
      ENDIF
      IF (LFETI) THEN
        CALL JELIRA(CHAMN1(1:19)//'.FETC','LONMAX',NBSD,K8BID)
        CALL JEVEUO('&FETI.LISTE.SD.MPI','L',ILIMPI)
        CALL JEVEUO(CHAMN1(1:19)//'.FETC','L',IFETC1)
        CALL JEVEUO(CHAMN2(1:19)//'.FETC','L',IFETC2)
      ELSE
        NBSD=0
      ENDIF
            
C --- BOUCLE SUR LES SOUS-DOMAINES CF ASSMAM OU VTCMBL PAR EXEMPLE
      DO 20 IDD=0,NBSD
        IDDOK=.FALSE.
        IF (.NOT.LFETI) THEN
          IDDOK=.TRUE.
        ELSE 
          IF (ZI(ILIMPI+IDD).EQ.1) IDDOK=.TRUE.
        ENDIF
        IF (IDDOK) THEN
          IF (IDD.EQ.0) THEN
            KVAL1=CHAMN1(1:19)//'.VALE'
            KVAL2=CHAMN2(1:19)//'.VALE'
          ELSE
            KVAL1=ZK24(IFETC1+IDD-1)(1:19)//'.VALE'
            KVAL2=ZK24(IFETC2+IDD-1)(1:19)//'.VALE'
          ENDIF
          CALL JEVEUO(KVAL1,'L',IVAL1)
          CALL JEVEUO(KVAL2,'E',IVAL2)
          CALL JELIRA(KVAL2,'LONMAX',NEQ,K8BID)
          CALL DAXPY(NEQ,ALPHA,ZR(IVAL1),1,ZR(IVAL2),1)
        ENDIF
   20 CONTINUE
      CALL JEDEMA()
      END
