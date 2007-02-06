      SUBROUTINE VSDSPE(NOMZ,M80,NVU,NOBJ)

      IMPLICIT NONE
      CHARACTER*(*) NOMZ,M80
      INTEGER NVU,NOBJ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/02/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C BUT : VERISD/SPECTRE
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  --------------------------
C -DEB------------------------------------------------------------------

      INTEGER IER,JVAIN,ISPECT,JVAVF,JVATE,VERIOB,IRET,I,LONG
      CHARACTER*8 NOM,KBID

      CALL JEMARQ()
      NOM=NOMZ(1:8)
      
C---- 1. VERIFICATION DE L'OBJET VAIN

C     SA TAILLE DEPEND DU TYPE DE SPECTRE (STOCKER DANS CE VECTEUR)

      IER=VERIOB('O',NOM//'           .VAIN','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .VAIN','TYPE_I',0,' ')

      CALL JEVEUO(NOM//'           .VAIN','L',JVAIN)
      ISPECT=ZI(JVAIN)
      IF (ISPECT.LT.10) THEN
C    LA LONGUEUR DOIT ETRE 1
        IER=VERIOB('O',NOM//'           .VAIN','LONMAX',1,' ')
        CALL ASSERT ((ZI(JVAIN).EQ.1).OR.(ZI(JVAIN).EQ.2).OR.
     &      (ZI(JVAIN).EQ.3).OR.(ZI(JVAIN).EQ.4))
      ELSE
C    LA LONGUEUR DOIT ETRE 3
        IER=VERIOB('O',NOM//'           .VAIN','LONMAX',3,' ')
        CALL ASSERT ((ZI(JVAIN).EQ.11).OR.(ZI(JVAIN).EQ.21))
        CALL ASSERT((ZI(JVAIN+1).EQ.0).OR.(ZI(JVAIN+1).EQ.1))
        CALL ASSERT(ZI(JVAIN+2).GT.0)
      ENDIF

C---- 2. VERIFICATION DE L'OBJET VARE


C     SA TAILLE ET SON EXISTENCE DEPEND DU TYPE DE SPECTRE
C     SI LE SPECTRE EST DE TYPE SPEC_FONC_FORME (ISPEC=11),
C     CET OBJET N'EXISTE PAS
      IF (ISPECT.LT.10) THEN
        IER=VERIOB('O',NOM//'           .VARE','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .VARE','TYPE_R',0,' ')
        IER=VERIOB('O',NOM//'           .VARE','LONMAX',12,' ')
      ELSE IF (ISPECT.EQ.21) THEN
        IER=VERIOB('O',NOM//'           .VARE','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .VARE','TYPE_R',0,' ')
        IER=VERIOB('O',NOM//'           .VARE','LONMAX',1,' ')
      ENDIF


C---- 3. VERIFICATION DE L'OBJET VATE

C     SA TAILLE ET SON CONTENU DEPENDENT DU TYPE D INTERSPECTRE

      IF (ISPECT.EQ.1) THEN
C     SPEC_LONG_COR_1 OU SPEC_CORR_CONV_1     
        IER=VERIOB('O',NOM//'           .VATE','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .VATE','TYPE_K16',0,' ')
        IER=VERIOB('O',NOM//'           .VATE','LONMAX',13,' ')
        CALL JEVEUO(NOM//'           .VATE','L',JVATE)

        IF (ZK16(JVATE).EQ.'SPEC_LONG_COR_1') THEN
          CALL ASSERT(ZK16(JVATE+1).EQ.'LONG_COR')
          CALL JEEXIN(ZK16(JVATE+2)//'   .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
          IF (IRET.EQ.0) THEN
            CALL VERIJA('O','FONCTION',ZK16(JVATE+2),
     &                  M80,NVU,NOBJ)
          ELSE
            CALL VERIJA('O','FORMULE',ZK16(JVATE+2),
     &                    M80,NVU,NOBJ)
          ENDIF
          CALL ASSERT(ZK16(JVATE+3).EQ.'VISC_CINE')

        ELSE IF (ZK16(JVATE).EQ.'SPEC_CORR_CONV_1') THEN
          CALL ASSERT(ZK16(JVATE+1).EQ.'LONG_COR_1')
          CALL ASSERT(ZK16(JVATE+2).EQ.'LONG_COR_2')
          CALL ASSERT(ZK16(JVATE+3).EQ.'VITE_FLUI')
          CALL ASSERT(ZK16(JVATE+4).EQ.'RHO_FLUI')
          CALL ASSERT(ZK16(JVATE+5).EQ.'FREQ_COUP')
          CALL ASSERT(ZK16(JVATE+6).EQ.'K')
          CALL ASSERT(ZK16(JVATE+7).EQ.'D_FLUI')
          CALL ASSERT(ZK16(JVATE+8).EQ.'COEF_VITE_FLUI_A')
          CALL ASSERT(ZK16(JVATE+9).EQ.'COEF_VITE_FLUI_O')
          CALL ASSERT((ZK16(JVATE+10).EQ.'AU_YANG').OR.
     &                (ZK16(JVATE+10).EQ.'GENERALE').OR.
     &                (ZK16(JVATE+10).EQ.'CORCOS'))
        ELSE
          CALL U2MESS('F','SDVERI_33')
        ENDIF

      ELSE IF (ISPECT.EQ.2) THEN
        IER=VERIOB('O',NOM//'           .VATE','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .VATE','TYPE_K16',0,' ')
        IER=VERIOB('O',NOM//'           .VATE','LONMAX',13,' ')
        CALL JEVEUO(NOM//'           .VATE','L',JVATE)
        IF (ZK16(JVATE).EQ.'SPEC_LONG_COR_2') THEN
          CALL ASSERT(ZK16(JVATE+1).EQ.'LONG_COR')
          CALL JEEXIN(ZK16(JVATE+2)//'   .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
          IF (IRET.EQ.0) THEN
            CALL VERIJA('O','FONCTION',ZK16(JVATE+2),
     &                  M80,NVU,NOBJ)
          ELSE
            CALL VERIJA('O','FORMULE',ZK16(JVATE+2),
     &                  M80,NVU,NOBJ)
          ENDIF
          CALL ASSERT(ZK16(JVATE+3).EQ.'FREQ_COUP')
          CALL ASSERT(ZK16(JVATE+4).EQ.'PHI0')
          CALL ASSERT(ZK16(JVATE+5).EQ.'BETA')
        ELSE IF (ZK16(JVATE).EQ.'SPEC_CORR_CONV_2') THEN
          CALL JEEXIN(ZK16(JVATE+1)//'   .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
          IF (IRET.EQ.0) THEN
            CALL VERIJA('O','FONCTION',ZK16(JVATE+1),
     &                  M80,NVU,NOBJ)
          ELSE
            CALL VERIJA('O','FORMULE',ZK16(JVATE+1),
     &                  M80,NVU,NOBJ)
          ENDIF
          CALL ASSERT(ZK16(JVATE+2).EQ.'VITE_FLUI')
          CALL ASSERT(ZK16(JVATE+3).EQ.'FREQ_COUP')
          CALL ASSERT((ZK16(JVATE+4).EQ.'AU_YANG').OR.
     &                (ZK16(JVATE+4).EQ.'GENERALE').OR.
     &                (ZK16(JVATE+4).EQ.'CORCOS'))
          CALL ASSERT(ZK16(JVATE+5).EQ.'COEF_VITE_FLUI_A')
          CALL ASSERT(ZK16(JVATE+6).EQ.'COEF_VITE_FLUI_O')
        ELSE
          CALL U2MESS('F','SDVERI_33')
        ENDIF

      ELSE IF (ISPECT.EQ.3) THEN
        IER=VERIOB('O',NOM//'           .VATE','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .VATE','TYPE_K16',0,' ')
        IER=VERIOB('O',NOM//'           .VATE','LONMAX',13,' ')
        CALL JEVEUO(NOM//'           .VATE','L',JVATE)
        CALL ASSERT(ZK16(JVATE).EQ.'SPEC_LONG_COR_3')
        CALL ASSERT(ZK16(JVATE+1).EQ.'LONG_COR')
        CALL JEEXIN(ZK16(JVATE+2)//'   .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
        IF (IRET.EQ.0) THEN
          CALL VERIJA('O','FONCTION',ZK16(JVATE+2),
     &                  M80,NVU,NOBJ)
        ELSE
          CALL VERIJA('O','FORMULE',ZK16(JVATE+2),
     &                    M80,NVU,NOBJ)
        ENDIF
        CALL ASSERT(ZK16(JVATE+3).EQ.'FREQ_COUP')
        CALL ASSERT(ZK16(JVATE+4).EQ.'PHI0_1')
        CALL ASSERT(ZK16(JVATE+5).EQ.'BETA_1')
        CALL ASSERT(ZK16(JVATE+6).EQ.'PHI0_2')
        CALL ASSERT(ZK16(JVATE+7).EQ.'BETA_2')

      ELSE IF (ISPECT.EQ.4) THEN
        IER=VERIOB('O',NOM//'           .VATE','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .VATE','TYPE_K16',0,' ')
        IER=VERIOB('O',NOM//'           .VATE','LONMAX',13,' ')
        CALL JEVEUO(NOM//'           .VATE','L',JVATE)

        CALL ASSERT(ZK16(JVATE).EQ.'SPEC_LONG_COR_4')
        CALL ASSERT(ZK16(JVATE+1).EQ.'LONG_COR')
        CALL JEEXIN(ZK16(JVATE+2)//'   .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
        IF (IRET.EQ.0) THEN
          CALL VERIJA('O','FONCTION',ZK16(JVATE+2),
     &                  M80,NVU,NOBJ)
        ELSE
          CALL VERIJA('O','FORMULE',ZK16(JVATE+2),
     &                    M80,NVU,NOBJ)
        ENDIF
        CALL ASSERT(ZK16(JVATE+3).EQ.'TAUX_VIDE')
        CALL ASSERT(ZK16(JVATE+4).EQ.'BETA')
        CALL ASSERT(ZK16(JVATE+5).EQ.'GAMMA')

      ELSE IF (ISPECT.EQ.11) THEN
C      FOURNIT-ON L INTERSPECTRE
       
        CALL JEVEUO(NOM//'           .VATE','L',JVATE)
        IF (ZI(JVAIN+1).EQ.0) THEN

C      ON FOURNIT UN INTERSPECTRE

          IER=VERIOB('O',NOM//'           .VATE','EXIS_V',0,' ')
          IER=VERIOB('O',NOM//'           .VATE','TYPE_K16',0,' ')
          CALL JELIRA(NOM//'           .VATE','LONMAX',LONG,KBID)
          CALL ASSERT(ZK16(JVATE).EQ.'SPEC_FONC_FORME')
          CALL VERIJA('O','CARA_ELEM',ZK16(JVATE+1),M80,NVU,NOBJ)
          CALL VERIJA('O','MODELE',ZK16(JVATE+2),M80,NVU,NOBJ)
          CALL VERIJA('O','TABLE',ZK16(JVATE+3),M80,NVU,NOBJ)
          DO 10 I=5,LONG
            CALL JEEXIN(ZK16(JVATE+I-1)//'   .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
            IF (IRET.EQ.0) THEN
              CALL VERIJA('O','FONCTION',ZK16(JVATE+I-1),
     &                    M80,NVU,NOBJ)
            ELSE
              CALL VERIJA('O','FORMULE',ZK16(JVATE+I-1),
     &                    M80,NVU,NOBJ)
            ENDIF
10        CONTINUE
        ELSE

C      ON NE FOURNIT PAS D INTERSPECTRE

          IER=VERIOB('O',NOM//'           .VATE','EXIS_V',0,' ')
          IER=VERIOB('O',NOM//'           .VATE','TYPE_K16',0,' ')
          IER=VERIOB('O',NOM//'           .VATE','LONMAX',5,' ')
          CALL ASSERT(ZK16(JVATE).EQ.'SPEC_FONC_FORME')
          CALL VERIJA('O','CARA_ELEM',ZK16(JVATE+1),M80,NVU,NOBJ)
          CALL VERIJA('O','MODELE',ZK16(JVATE+2),M80,NVU,NOBJ)
          CALL ASSERT(ZK16(JVATE+3).EQ.'GRAPPE_1')
          CALL ASSERT((ZK16(JVATE+4).EQ.'DEBIT_180').OR.
     &                (ZK16(JVATE+4).EQ.'DEBIT_300'))
      
        ENDIF
      ELSE IF (ISPECT.EQ.21) THEN


C      FOURNIT-ON L INTERSPECTRE
       
        CALL JEVEUO(NOM//'           .VATE','L',JVATE)
        IF (ZI(JVAIN+1).EQ.0) THEN

C      ON FOURNIT UN INTERSPECTRE

          IER=VERIOB('O',NOM//'           .VATE','EXIS_V',0,' ')
          IER=VERIOB('O',NOM//'           .VATE','TYPE_K16',0,' ')
          CALL JELIRA(NOM//'           .VATE','LONMAX',LONG,KBID)
          CALL ASSERT(ZK16(JVATE).EQ.'SPEC_EXCI_POINT')
          CALL VERIJA('O','CARA_ELEM',ZK16(JVATE+1),M80,NVU,NOBJ)
          CALL VERIJA('O','MODELE',ZK16(JVATE+2),M80,NVU,NOBJ)
          CALL VERIJA('O','TABLE',ZK16(JVATE+3),M80,NVU,NOBJ)

          DO 20 I=5,LONG
           CALL ASSERT((ZK16(JVATE+I-1).EQ.'FORCE').OR.
     &                 (ZK16(JVATE+I-1).EQ.'MOMENT'))
20        CONTINUE
        ELSE

C      ON NE FOURNIT PAS D INTERSPECTRE

          IER=VERIOB('O',NOM//'           .VATE','EXIS_V',0,' ')
          IER=VERIOB('O',NOM//'           .VATE','TYPE_K16',0,' ')
          IER=VERIOB('O',NOM//'           .VATE','LONMAX',5,' ')
          CALL ASSERT(ZK16(JVATE).EQ.'SPEC_EXCI_POINT')
          CALL VERIJA('O','CARA_ELEM',ZK16(JVATE+1),M80,NVU,NOBJ)
          CALL VERIJA('O','MODELE',ZK16(JVATE+2),M80,NVU,NOBJ)
          CALL ASSERT(ZK16(JVATE+3).EQ.'GRAPPE_2')
          CALL ASSERT((ZK16(JVATE+4).EQ.'ASC_CEN').OR.
     &                (ZK16(JVATE+4).EQ.'ASC_EXC').OR.
     &                (ZK16(JVATE+4).EQ.'DES_CEN').OR.
     &                (ZK16(JVATE+4).EQ.'DES_EXC'))
        ENDIF
      ENDIF  


C---- 4. VERIFICATION DE L'OBJET VAVF

      IER=VERIOB('F',NOM//'           .VAVF','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .VAVF','TYPE_K8',0,' ')
      IER=VERIOB('F',NOM//'           .VAVF','LONMAX',1,' ')

      IF (IER.EQ.1) THEN
        CALL JEVEUO(NOM//'           .VAVF','L',JVAVF)
C ON EST OBLIGE DE FAIRE CETTE VERIFICATION CAR LE .VAVF PEUT TRES
C BIEN ETRE CREER VIDE
        IF (ZK8(JVAVF).NE.' ') THEN
          CALL JEEXIN(ZK8(JVAVF)//'           .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
          IF (IRET.EQ.0) THEN
            CALL VERIJA('O','FONCTION',ZK8(JVAVF),
     &                M80,NVU,NOBJ)
          ELSE
            CALL VERIJA('O','FORMULE',ZK8(JVAVF),
     &                M80,NVU,NOBJ)
          ENDIF
        ENDIF
      ENDIF

C---- 5. VERIFICATION DE L'OBJET NNOE

      IER=VERIOB('F',NOM//'           .NNOE','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .NNOE','TYPE_K8',0,' ')

      CALL JEDEMA()
      END
