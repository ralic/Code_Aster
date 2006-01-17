      SUBROUTINE GFACC0 ( Z, DZ, NUMEDD, ACCMOI, CHGRFL ) 

      IMPLICIT NONE
      REAL*8              Z, DZ
      CHARACTER*24        NUMEDD, ACCMOI, CHGRFL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2006   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     FORCE_FLUIDE : CALCUL DE L'ACCELERATION INITIALE
C
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      CHARACTER*32       JEXNUM
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER  JFFL, JIFL, II, I1, I3, I5, I7, I8, I9, I11, I12, I16,
     &         I18, NBNO, NEC, NLILI, NDIM, INO, IVAL, IDIM, I, K, IER,
     &         IAPRNO, JACC0, IT
      REAL*8   ROC, ROD, AC, LTIGE, VARAI, LCRAY, DTM, MA0, MA, PIS4, 
     &         R8PI, ROP, Q, CGG, FFPLAQ, Z0, FTOT, FPTG1, FFTG1, M, 
     +         G, D2Z, VDIR(3)
      CHARACTER*8   K8B
      CHARACTER*24  NOLILI
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JEVEUO ( '&&GFLECT.INDICE', 'E', JIFL ) 
      CALL JEVEUO ( CHGRFL, 'E', JFFL ) 
      II = 5 + ZI(JIFL-1+5)
      I1 = ZI(JIFL-1+II+1)
      I3 = ZI(JIFL-1+II+3)
      I5 = ZI(JIFL-1+II+5)
      I7 = ZI(JIFL-1+II+7)
      I8 = ZI(JIFL-1+II+8)
      I9 = ZI(JIFL-1+II+9)
      I11 = ZI(JIFL-1+II+11)
      I12 = ZI(JIFL-1+II+12)
      I16 = ZI(JIFL-1+II+16)
      I18 = ZI(JIFL-1+II+18)
C
C --- CALCUL DE LA MASSE APPARENTE :
C     ----------------------------
C
      PIS4 = 0.25D0 * R8PI()
      ROC  = ZR(JFFL-1+I3+1)
      ROD  = ZR(JFFL-1+I3+2)
      ROP  = ZR(JFFL-1+I3+3)
      AC   = ZR(JFFL-1+I7+8)
      G    = ZR(JFFL-1+I8+3)
      LTIGE  = ZR(JFFL-1+I9+3)
      VARAI  = ZR(JFFL-1+I9+4)
      LCRAY  = ZR(JFFL-1+I9+7)
      DTM    = ZR(JFFL-1+I9+8)

C --- RECUPERATION DE LA MASSE DE LA GRAPPE :
C     ---------------------------------------
      MA0  = ZR(JFFL-1+I8+1)

      MA =   PIS4*DTM**2*(ROD*(LTIGE-Z) + ROP*Z)
     +     + 24*AC*(ROC*Z + ROP*(LCRAY-Z)) + VARAI*ROP

C
C --- FORCE DE PLAQUAGE DANS LE GUIDAGE CONTINU :
C     -----------------------------------------
C
      ROP = ZR(JFFL-1+I3+3)
      Q   = ZR(JFFL-1+I3+8)
      CGG = ZR(JFFL-1+I5+1)

      FFPLAQ = CGG*ROP*Q**2
C
C --- FORCE REPARTIE DANS LE TUBE GUIDE AVANT DASHPOT :
C     -----------------------------------------------
C
      Z0 = ZR(JFFL-1+I11+4)
      IT = 0
C
      CALL GFGUID ( IT, Z, DZ, FPTG1, FFTG1, ZR(JFFL-1+I3+1),
     +             ZR(JFFL-1+I7+1), ZR(JFFL-1+I1+1), ZI(JIFL-1+1),
     +             ZR(JFFL-1+I12+1), ZR(JFFL-1+I16+1), Z0 ) 

C
      FTOT = -MA*G - 24*(FPTG1+FFTG1) - FFPLAQ
      D2Z = FTOT / MA0
C
      CALL JEVEUO(ACCMOI(1:19)//'.VALE', 'E', JACC0)

      NDIM = 3
      NBNO = ZI(JIFL-1+5)
      VDIR(1) = ZR(JFFL-1+I18+3*(NBNO-1)+1)
      VDIR(2) = ZR(JFFL-1+I18+3*(NBNO-1)+2)
      VDIR(3) = ZR(JFFL-1+I18+3*(NBNO-1)+3)
C
      CALL DISMOI('F','NB_EC','DEPL_R','GRANDEUR',NEC,K8B,IER)
      CALL JELIRA(NUMEDD(1:14)//'.NUME.PRNO','NMAXOC',NLILI,K8B)
C
      K = 0
      DO 10 I = 1, NLILI
         CALL JENUNO(JEXNUM(NUMEDD(1:14)//'.NUME.LILI',I),NOLILI)
         IF (NOLILI(1:8).NE.'&MAILLA ') GOTO 10
         K = I
  10  CONTINUE
      IF (K.EQ.0) THEN
         CALL UTMESS('F','GFACC0','ERREUR DANS LA RECUPERATION DU'//
     +                            ' NUME.PRNO .')
      ENDIF
      CALL JEVEUO(JEXNUM(NUMEDD(1:14)//'.NUME.PRNO',K),'L',IAPRNO)

      DO 20  I = 1, NBNO
         INO  = ZI(JIFL-1+5+I)
         IVAL = ZI(IAPRNO+(INO-1)*(NEC+2)+1-1) - 1
         DO 22 IDIM = 1, NDIM
            ZR(JACC0+IVAL+IDIM-1) = ZR(JACC0-1+IVAL+IDIM)
     +                                   + D2Z*VDIR(IDIM)
 22      CONTINUE
 20   CONTINUE
C
      CALL JEDEMA()
      END
