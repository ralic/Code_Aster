      SUBROUTINE OP0187 ( IER )
C
      IMPLICIT     NONE
      INTEGER      IER
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 11/03/2003   AUTEUR DURAND C.DURAND 
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
C     ------------------------------------------------------------------
C     COMMANDE POST_GOUJ2E
C     PERMET D'ORGANISER LA TABLE DES RESULTATS AU FORMAT VOULU.
C
C     ------------------------------------------------------------------
C
C  OUT  : IER   = 0 => TOUT C EST BIEN PASSE
C         IER   > 0 => NOMBRE D ERREURS RENCONTREES
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C     -------------------- DEBUT DES DECLARATIONS ----------------------
C
      INTEGER      I, J, N1
      INTEGER      NINCH, NBVAL, NBV, IADFNO, IADFCU, IADNOE
      INTEGER      IADNOR
      INTEGER      VITMP1(100), VI1, VI2(2)
C
      REAL*8       FOAPP, VRTMP1(100), FOINCH, RBID, VR1(2)
C
      COMPLEX*16   CBID
C
      CHARACTER*8  K8B(2), K8B1, VKTMP1(100), KBID
      CHARACTER*8  NOMRES, NOMTAB, NEWTAB
      CHARACTER*8  VK1
      CHARACTER*16 CONCEP, NOMCMD, K16B(1)
      CHARACTER*16 LPARA0(2),LPARA1(4)
      CHARACTER*24 NOMOBJ, NOMOB1, NOMOB2
C
C ----------------------------------------------------------------------
C
      DATA NBV    / 0 /
      DATA FOAPP  / 0.0D+00 /
C
C ----- LISTE DES PARAMETRES DE LA TABLE DES RESULTATS -----------------
C
      DATA LPARA0 / 'NUME_FILET' , 'NOEUDS' /
      DATA LPARA1 / 'NUME_ORDRE' , 'NUME_FILET' ,
     &              'REACTION' , 'REACTION_CUMU' /
C
C     -------------------- FIN DES DECLARATIONS ------------------------
C
      CALL JEMARQ ( )
      CALL INFMAJ()
C
C
C     ------- RECUPERATION DES DONNEES UTILISATEUR ---------------------
C
      CALL GETRES ( NOMRES, CONCEP, NOMCMD )
      NEWTAB = NOMRES
C
      CALL GETVID ( ' ', 'TABLE', 1,1,1, NOMTAB, N1 )
C
C
C     ----- EXTRACTION DES VECTEURS FORCE NODAL, NUMEROS DE NOEUD ----
C     -----                  ET NUMEROS D'ORDRE                   ----
C
      K8B(1) = 'DY'
      NOMOBJ = '&&OP0187.TEMP.VFNO'
      CALL TBEXVE ( NOMTAB, K8B(1), NOMOBJ, 'V', NBVAL, K8B1 )
      CALL JEVEUO ( NOMOBJ, 'E', IADFNO )
C
      K8B(2) = 'NOEUD'
      NOMOB1 = '&&OP0187.TEMP.VNOE'
      CALL TBEXVE ( NOMTAB, K8B(2), NOMOB1, 'V', NBVAL, K8B1 )
      CALL JEVEUO ( NOMOB1, 'E', IADNOE )
C
      K16B(1) = 'NUME_ORDRE'
      NOMOB2 = '&&OP0187.TEMP.VNOR'
      CALL TBEXVE ( NOMTAB, K16B(1), NOMOB2, 'V', NBVAL, K8B1 )
      CALL JEVEUO ( NOMOB2, 'L', IADNOR )
C
      NBV = 0
      DO 10 I = 1, NBVAL
        IF ( ZI(IADNOR+I-1) .EQ. 1 ) THEN
          NBV = NBV + 1
        ENDIF
 10   CONTINUE
      IF ( NBV .GT. 0 ) THEN
        NINCH = NBVAL/NBV
      ELSE
        CALL UTMESS ('F', 'OP0187_F1', 'LE NOMBRE DE FILET '
     &               //'N''EST PAS CORRECT.')
      ENDIF
C
      DO 20 J = 1, NINCH
        DO 30 I = 1, NBV
          VRTMP1(I) = ZR(IADFNO+I-1+((J-1)*NBV))
          VKTMP1(I) = ZK8(IADNOE+I-1+((J-1)*NBV))
 30     CONTINUE
        DO 40, I = 1, NBV
          ZR(IADFNO+I-1+((J-1)*NBV)) = VRTMP1(NBV+1-I)
          ZK8(IADNOE+I-1+((J-1)*NBV)) = VKTMP1(NBV+1-I)
 40     CONTINUE
 20   CONTINUE
C
C     ---------- CONSTRUCTION DU VECTEUR REACTION CUMULEE ------------
C
      CALL WKVECT ( '&&OP0187.VFCU', 'V V R', NBVAL, IADFCU )
C
        FOAPP = 0.0D+00
        N1 = NBVAL - NBV
        DO 50 I = (N1+1), NBVAL
          FOAPP = FOAPP + ZR(IADFNO+I-1)
 50     CONTINUE
C
      FOINCH = FOAPP/NINCH
      DO 80 J = 1, NINCH
        ZR(IADFCU+((J-1)*NBV)) = 100.0D0*ZR(IADFNO+((J-1)*NBV))/
     &                           (J*FOINCH)
        DO 90 I = 1, (NBV-1)
          ZR(IADFCU+I+((J-1)*NBV)) = ZR(IADFCU+I-1+((J-1)*NBV))+
     &                               100.0D0*
     &                               ZR(IADFNO+I+((J-1)*NBV))/
     &                               (J*FOINCH)
 90     CONTINUE
 80   CONTINUE
C     ------------------ LISTE DES NUMEROS DE FILET --------------------
      DO 100 I = 1, NBV
        VITMP1(I) = I
 100  CONTINUE
C
C     ----- CONSTRUCTION DE LA TABLE DES RESULTATS ---------------------
C
      CALL TBCRSD (NEWTAB, 'G')
C
      CALL TBAJPA (NEWTAB, 1, 'NUME_ORDRE', 'I')
      CALL TBAJPA (NEWTAB, 1, 'NUME_FILET', 'I')
      CALL TBAJPA (NEWTAB, 1, 'REACTION', 'R')
      CALL TBAJPA (NEWTAB, 1, 'REACTION_CUMU', 'R')
      CALL TBAJPA (NEWTAB, 1, 'NOEUDS', 'K8')
C
      DO 200 I = 1, NBV
        VI1 = VITMP1(I)
        VK1 = ZK8(IADNOE+I-1)
        CALL TBAJLI ( NEWTAB, 2, LPARA0, VI1, RBID, CBID, VK1, 0 )
 200  CONTINUE
C
      DO 210 J = 1, NINCH
        VI2(1) = J
        DO 220 I = 1, NBV
          VI2(2) = VITMP1(I)
          VR1(1) = ZR(IADFNO+I-1+((J-1)*NBV))
          VR1(2) = ZR(IADFCU+I-1+((J-1)*NBV))
          CALL TBAJLI ( NEWTAB, 4, LPARA1, VI2, VR1, CBID, KBID, 0 )
 220    CONTINUE
 210  CONTINUE
C
C
      CALL JEDETC('V','&&OP0187',1)
      CALL JEDEMA ( )
C
      END
