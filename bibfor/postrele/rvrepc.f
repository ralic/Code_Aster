      SUBROUTINE RVREPC(COURBE,REPERE,SDNEWR)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      CHARACTER*19 SDNEWR
      CHARACTER*8  COURBE, REPERE
C
C***********************************************************************
C
C  OPERATION REALISEE
C  ------------------
C
C     CALCUL  DU REPERE LOCAL OU POLAIRE LA LONG D' UNE COURBE
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     COURBE : NOM DU CONCEPT COURBE
C     REPERE : VAUT 'LOCAL' OU 'POLAIRE'
C
C  ARGUMENTS EN SORTIE
C  -------------------
C
C     SDNEWR : NOM DE LA SD DU REPERE CALCULE
C              (DOC. C.F. RVCHGR)
C
C***********************************************************************
C
C  -----------------------------------------
C
C
C
C  ---------------------------------
C
C  VARIABLES LOCALES
C  -----------------
C
      CHARACTER*14 NABSCR,NABSCE,NCNXOR,NCNXEX
      CHARACTER*24 NVEC1,NVEC2
      INTEGER      AABSCR,AVEC1,AVEC2,AASGT,ABSGT,ACARC,ASARC,ARARC
      INTEGER      NBPART,NBSGT,NBARC,IPART,NBPT,IPT,AABSCE,ACNXE,ACNXO
      INTEGER      NBCX,ICX,PT,DCX,FCX
      REAL*8       X,Y,XA,XB,XC,YA,YB,YC,R,S,T1,T2,N1,N2
      CHARACTER*1 K1BID
C
C====================== CORPS DE LA ROUTINE ===========================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL JEMARQ()
      NVEC1  = SDNEWR//'.VEC1'
      NVEC2  = SDNEWR//'.VEC2'
      NABSCR = COURBE//'.ORSGT'
      NABSCE = COURBE//'.EXSGT'
      NCNXOR = COURBE//'.CNXOR'
      NCNXEX = COURBE//'.CNXEX'
C
      CALL JELIRA(COURBE//'.XYASGT','LONMAX',NBSGT,K1BID)
      CALL JELIRA(COURBE//'.XYCARC','LONMAX',NBARC,K1BID)
      CALL JEVEUO(COURBE//'.XYASGT','L',AASGT)
      CALL JEVEUO(COURBE//'.XYBSGT','L',ABSGT)
      CALL JEVEUO(COURBE//'.XYCARC','L',ACARC)
      CALL JEVEUO(COURBE//'.XRARC' ,'L',ARARC)
      CALL JEVEUO(COURBE//'.XSARC' ,'L',ASARC)
C
      NBSGT  = (NBSGT/2) - 1
      NBARC  = (NBARC/2) - 1
      NBPART =  NBARC + NBSGT
C
      CALL JECREC(NVEC1,'V V R','NU','DISPERSE','VARIABLE',NBPART)
      CALL JECREC(NVEC2,'V V R','NU','DISPERSE','VARIABLE',NBPART)
C
      DO 100, IPART = 1, NBPART, 1
C
         CALL JELIRA(JEXNUM(NCNXOR,IPART),'LONMAX',NBCX,K1BID)
         CALL JEVEUO(JEXNUM(NCNXOR,IPART),'L',ACNXO)
         CALL JEVEUO(JEXNUM(NCNXEX,IPART),'L',ACNXE)
         CALL JELIRA(JEXNUM(NABSCR,IPART),'LONMAX',NBPT,K1BID)
         CALL JEVEUO(JEXNUM(NABSCR,IPART),'L',AABSCR)
         CALL JEVEUO(JEXNUM(NABSCE,IPART),'L',AABSCE)
         CALL JECROC(JEXNUM(NVEC1,IPART))
         CALL JEECRA(JEXNUM(NVEC1,IPART),'LONMAX',2*(NBPT+NBCX),' ')
         CALL JEVEUO(JEXNUM(NVEC1,IPART),'E',AVEC1)
         CALL JECROC(JEXNUM(NVEC2,IPART))
         CALL JEECRA(JEXNUM(NVEC2,IPART),'LONMAX',2*(NBPT+NBCX),' ')
         CALL JEVEUO(JEXNUM(NVEC2,IPART),'E',AVEC2)
C
         XA = ZR(AASGT  + 2*IPART+1 -1)
         YA = ZR(AASGT  + 2*IPART+2 -1)
         XB = ZR(ABSGT  + 2*IPART+1 -1)
         YB = ZR(ABSGT  + 2*IPART+2 -1)
         XC = ZR(ACARC  + 2*IPART+1 -1)
         YC = ZR(ACARC  + 2*IPART+2 -1)
         R  = ZR(ARARC  +   IPART+1 -1)
C
         PT = 1
C
         DO 110, ICX = 1, NBCX, 1
C
            DCX = ZI(ACNXO + ICX-1)
            FCX = ZI(ACNXE + ICX-1)
C
            DO 120, IPT = DCX, FCX + 1, 1
C
               IF ( IPT .LE. FCX ) THEN
C
                  S  = ZR(AABSCR +   IPT -1)
C
               ELSE
C
                  S  = ZR(AABSCE +   IPT-1 -1)
C
               ENDIF
C
               IF ( IPART .LE. NBSGT ) THEN
C
                  X  = XB - XA
                  Y  = YB - YA
C
                  IF ( REPERE .EQ. 'POLAIRE' ) THEN
C
                     X  = XA + S*X
                     Y  = YA + S*Y
C
                     CALL RVRTHE(X,Y,T1,T2,N1,N2)
C
                  ELSE
C
                     IF ( IPT .EQ. 1 ) THEN
C
                        CALL RVRTHE(X,Y,T1,T2,N1,N2)
C
                     ENDIF
C
                  ENDIF
C
               ELSE
C
                  N1 = COS(S)
                  N2 = SIN(S)
C
                  IF ( REPERE .EQ. 'LOCAL' ) THEN
C
                     T1 = -N2
                     T2 =  N1
C
                  ELSE
C
                     X  = XC + R*N1
                     Y  = YC + R*N2
C
                     CALL RVRTHE(X,Y,N1,N2,T1,T2)
C
                  ENDIF
C
               ENDIF
C
               ZR(AVEC1 + 2*PT-1 -1) =  T1
               ZR(AVEC1 + 2*PT   -1) =  T2
               ZR(AVEC2 + 2*PT-1 -1) =  N1
               ZR(AVEC2 + 2*PT   -1) =  N2
C
               PT = PT + 1
C
120         CONTINUE
C
110      CONTINUE
C
100   CONTINUE
C
      CALL JEDEMA()
      END
