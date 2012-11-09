      SUBROUTINE RVAFFS ( MCF, IOCC, SDLIEU, SDEVAL, SDMOY, QUANT,
     +                    OPTION, REP, NOMTAB, NCHEFF, I1, ISD )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER             IOCC, I1, ISD
      CHARACTER*16        NCHEFF
      CHARACTER*19        SDEVAL, NOMTAB
      CHARACTER*24        SDLIEU, SDMOY
      CHARACTER*(*)       MCF, REP, OPTION, QUANT
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     AFFICHAGE SOMME
C     ------------------------------------------------------------------
C IN  SDLIEU : K : SD DU LIEU TRAITEE
C IN  SDEVAL : K : SD DE L' EVALUATION DE LA QUANTITE SUR CE LIEU
C IN  SDMOY  : K : SD DES SOMMES
C IN  QUANT  : K : NOM DE LA QUANTITE TRAITEE
C IN  OPTION : K : NOM DE L' OPTION   TRAITEE
C IN  TEST   : R : TABLE DES VALEURS TEST
C IN  MAXTST : R : DIMENSION DE LA TABLE TEST
C IN  PTEST  : I : POINTEUR SUR LA SOUS TABLE DE TEST A TRAITRER
C            :   : PTEST = 0 <=> AFFICHAGE DE MOYENNE
C IN  PREC   : R : PRECISION DU TEST
C IN  CRIT   : K8: CRITERE   DU TEST
C     ------------------------------------------------------------------
      INTEGER      ANOCP, NBCP, IOC, AABSC, NBPT, NBOC, ASDMO, NIV
      INTEGER      I, IFM, ANOMND, NBCO, NBSP, K
      REAL*8       XA, XB, YA, YB, AX, S1, S2, ZA, ZB
      CHARACTER*1  K1BID
      CHARACTER*4  DOCUL
      CHARACTER*16 OPER
      CHARACTER*24 NABSC, NNOCP
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
      CALL INFNIV ( IFM, NIV )
      OPER = 'SOMME'
      IF (NIV.GT.1) CALL RVINFA(IFM,MCF,IOCC,QUANT,OPTION,OPER,REP(1:1))
      NNOCP = SDMOY(1:19)//'.NOCP'
      NABSC = SDLIEU(1:19)//'.ABSC'
      CALL JELIRA(SDLIEU(1:19)//'.REFE','DOCU',I,DOCUL)
      CALL JEVEUO(SDLIEU(1:19)//'.DESC','L',ANOMND)
      CALL JELIRA(NABSC,'NMAXOC',NBOC,K1BID)
      CALL JELIRA(NNOCP,'LONMAX',NBCP,K1BID)
      CALL JEVEUO(NNOCP,'L',ANOCP)
      CALL JEVEUO(SDEVAL//'.PNCO','L',I)
      NBCO = ZI(I)
      CALL JEVEUO(SDEVAL//'.PNSP','L',I)
      NBSP = ZI(I)
      DO 100, IOC = 1, NBOC, 1
         CALL JELIRA(JEXNUM(NABSC ,IOC),'LONMAX',NBPT,K1BID)
         CALL JEVEUO(JEXNUM(NABSC ,IOC),'L',AABSC)
         CALL JEVEUO(JEXNUM(SDMOY(1:19)//'.VALE',IOC),'L',ASDMO)
         S1 = ZR(AABSC + 1-1)
         S2 = ZR(AABSC + NBPT-1)
         IF ( NIV .GT. 1 ) THEN
            IF ( (DOCUL .EQ. 'LSTN') .OR. (DOCUL .EQ. 'CHMM') )THEN
               WRITE(IFM,*)'CHEMIN RELIANT LES NOEUDS :'
               DO 200, I = 1,NBPT/8, 1
               WRITE(IFM,'(8(1X,A8))')(ZK8(ANOMND+(I-1)*8+K-1),K=1,8,1)
 200           CONTINUE
               WRITE(IFM,*)'   '
               WRITE(IFM,*)(ZK8(ANOMND+K-1)//' ',K=8*(NBPT/8)+1,NBPT,1)
            ELSEIF ( DOCUL .EQ. 'SGTD' ) THEN
               XA = ZR(ANOMND + 1-1)
               YA = ZR(ANOMND + 2-1)
               XB = ZR(ANOMND + 3-1)
               YB = ZR(ANOMND + 4-1)
               AX = SQRT((XA-XB)*(XA-XB) + (YA-YB)*(YA-YB))
               WRITE(IFM,*)'SEGMENT DE DROITE'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' ORIGINE               : (',XA,',',YA,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' EXTREMITE             : (',XB,',',YB,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                 ' ABSCISSES CURVILIGNES : (',AX*S1,',',AX*S2,')'
            ELSEIF ( DOCUL .EQ. 'SGT3' ) THEN
               XA = ZR(ANOMND + 1-1)
               YA = ZR(ANOMND + 2-1)
               ZA = ZR(ANOMND + 3-1)
               XB = ZR(ANOMND + 4-1)
               YB = ZR(ANOMND + 5-1)
               ZB = ZR(ANOMND + 6-1)
             AX = SQRT((XA-XB)*(XA-XB)+(YA-YB)*(YA-YB)+(ZA-ZB)*(ZA-ZB))
               WRITE(IFM,*)'SEGMENT DE DROITE'
               WRITE(IFM,'(A26,3(1PD14.6,A1))')
     +                ' ORIGINE               : (',XA,',',YA,',',ZA,')'
               WRITE(IFM,'(A26,3(1PD14.6,A1))')
     +                ' EXTREMITE             : (',XB,',',YB,',',ZB,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                ' ABSCISSES CURVILIGNES : (',AX*S1,',',AX*S2,')'
            ELSEIF ( DOCUL .EQ. 'ARCC' ) THEN
               XA = ZR(ANOMND + 1-1)
               YA = ZR(ANOMND + 2-1)
               XB = ZR(ANOMND + 3-1)
               AX = 57.29577951D0
               WRITE(IFM,*)'ARC DE CERCLE'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' CENTRE                : (',XA,',',YA,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' RAYON                 :  ',XB
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                 ' ABSCISSES CURVILIGNES : (',AX*S1,',',AX*S2,')'
            ELSE
            ENDIF
            WRITE(IFM,*)' '
         ENDIF
         CALL RVTASO ( ZR(ASDMO), ZK8(ANOCP), NBCP, NBCO, NBSP,
     +                 NOMTAB, IOCC, NCHEFF, I1, IOC, ISD )
 100  CONTINUE
C
      CALL JEDEMA()
      END
