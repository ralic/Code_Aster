      SUBROUTINE COECIS (NAPCIS,FONCIS)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*19        NAPCIS, FONCIS
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     ------------------------------------------------------------------
C       NAPPE DES COEFFICIENTS DE CISAILLEMENT POUR SECTION='RECTANGLE'
C       FONCTION DES COEFFICIENTS DE CISAILLEMENT POUR SECTION='CERCLE'
C     ------------------------------------------------------------------
C
      INTEGER      NBVALE, NBFONC, NPROL, NBVALF
      INTEGER      LPRO, LPAR, JVAL, LNOMF, LADRF, LVAL, I, IFONC, IVAL
      INTEGER      NBVFON, LPRO2, JVAL2, LVAL2, IVAL2, LFON
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C     ##################################################################
C     ######DEFINITION DE LA NAPPE DES COEFFICIENTS DE CISAILLEMENT#####
C     ####################SECTION = 'RECTANGLE'#########################
C     ##################################################################
      NAPCIS = 'NAPPE_'
C
C     NOMBRE DE FONCTIONS
      NBFONC = 12
C
C     NOMBRE DE VALEURS PAR FONCTIONS
      NBVALF = 12
C
      NPROL = 7 + 2*NBFONC
      CALL WKVECT(NAPCIS//'.PROL','V V K24',NPROL,LPRO)
      ZK24(LPRO  ) = 'NAPPE   '
      ZK24(LPRO+1) = 'LIN LIN '
      ZK24(LPRO+2) = 'ALPHA'
      ZK24(LPRO+3) = 'A'
      ZK24(LPRO+4) = 'EE'
      ZK24(LPRO+5) = NAPCIS
      ZK24(LPRO+6) = 'BETA'
      DO 10 I = 0 , NBFONC-1
         ZK24(LPRO+7+I*2) = 'LIN LIN '
         ZK24(LPRO+7+I*2+1) = 'EE'
 10   CONTINUE
C
      CALL WKVECT(NAPCIS//'.PARA','V V R',NBFONC,LPAR)
C     VALEURS DE ALPHA
      ZR(LPAR) = 0.D0
      ZR(LPAR+1) = 0.05D0
      ZR(LPAR+2) = 0.1D0
      ZR(LPAR+3) = 0.2D0
      ZR(LPAR+4) = 0.3D0
      ZR(LPAR+5) = 0.4D0
      ZR(LPAR+6) = 0.5D0
      ZR(LPAR+7) = 0.6D0
      ZR(LPAR+8) = 0.7D0
      ZR(LPAR+9) = 0.8D0
      ZR(LPAR+10) = 0.9D0
      ZR(LPAR+11) = 0.95D0
C
C     NOMBRE DE VALEURS TOTAL PAR FONCTION
      NBVALE = NBFONC*2
      CALL WKVECT('&&COECIS.VALEURS','V V R',NBVALE,JVAL)
C
      CALL WKVECT('&&COECIS.NOM.FONCTIONS','V V K24',NBFONC,LNOMF)
      CALL WKVECT('&&COECIS.POINTEURS.F','V V I',NBFONC,LADRF)
      DO 30 IFONC = 1, NBFONC
         ZK24(LNOMF+IFONC-1) = '&&COECIS.F'
         CALL CODENT(IFONC,'G',ZK24(LNOMF+IFONC-1)(11:19))
         ZK24(LNOMF+IFONC-1)(20:24) = '.VALE'
C        VALEURS DES FONCTIONS A(BETA)
         IF (IFONC.EQ.1) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.2D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.2D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.2D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.2D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.2D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.2D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 1.2D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 1.2D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 1.2D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 1.2D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 1.2D0
         ELSEIF (IFONC.EQ.2) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.2091D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.22903D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.29961D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.41338D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.57689D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.80293D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 2.11482D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 2.56072D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 3.26505D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 4.71451D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 6.68937D0
         ELSEIF (IFONC.EQ.3) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.21234D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.23638D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.31674D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.44242D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.62077D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.86614D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 2.20702D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 2.70395D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 3.51953D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 5.35808D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 8.19368D0
         ELSEIF (IFONC.EQ.4) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.21725D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.24671D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.3388D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.47742D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.67093D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.93642D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 2.30888D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 2.8664D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 3.82993D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 6.21611D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 10.2937D0
         ELSEIF (IFONC.EQ.5) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.22028D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.25225D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.34797D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.48853D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.6827D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.9487D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 2.32387D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 2.89403D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 3.90732D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 6.53601D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 11.2361D0
         ELSEIF (IFONC.EQ.6) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.22125D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.25299D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.34528D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.47872D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.66196D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.91269D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 2.26723D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 2.81021D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 3.79004D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 6.40058D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 11.1893D0
         ELSEIF (IFONC.EQ.7) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.22011D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.24905D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.33181D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.45075D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.61403D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.83775D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 2.1543D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 2.64029D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 3.52423D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 5.91643D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 10.3749D0
         ELSEIF (IFONC.EQ.8) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.21699D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.24091D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.30911D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.40786D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.54462D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.73307D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 2.00002D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 2.40936D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 3.15426D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 5.18602D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 9.01404D0
         ELSEIF (IFONC.EQ.9) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.21234D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.22963D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.27969D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.35418D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.45982D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.60759D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 1.81808D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 2.13953D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 2.71977D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 4.29975D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 7.29574D0
         ELSEIF (IFONC.EQ.10) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.20699D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.21695D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.24713D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.29483D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.36597D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.46914D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 1.61905D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 1.84773D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 2.25238D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 3.33064D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 5.37206D0
         ELSEIF (IFONC.EQ.11) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.20223D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.20561D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.21707D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.23759D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.27159D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.3254D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 1.40907D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 1.54135D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 1.77105D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 2.33808D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 3.36741D0
         ELSEIF (IFONC.EQ.12) THEN
            ZR(JVAL)    = 0.D0
            ZR(JVAL+1)  = 1.2D0
            ZR(JVAL+2)  = 0.05D0
            ZR(JVAL+3)  = 1.20064D0
            ZR(JVAL+4)  = 0.1D0
            ZR(JVAL+5)  = 1.20171D0
            ZR(JVAL+6)  = 0.2D0
            ZR(JVAL+7)  = 1.20582D0
            ZR(JVAL+8)  = 0.3D0
            ZR(JVAL+9)  = 1.2142D0
            ZR(JVAL+10) = 0.4D0
            ZR(JVAL+11) = 1.22956D0
            ZR(JVAL+12) = 0.5D0
            ZR(JVAL+13) = 1.25619D0
            ZR(JVAL+14) = 0.6D0
            ZR(JVAL+15) = 1.30111D0
            ZR(JVAL+16) = 0.7D0
            ZR(JVAL+17) = 1.37761D0
            ZR(JVAL+18) = 0.8D0
            ZR(JVAL+19) = 1.51661D0
            ZR(JVAL+20) = 0.9D0
            ZR(JVAL+21) = 1.84134D0
            ZR(JVAL+22) = 0.95D0
            ZR(JVAL+23) = 2.37116D0
         ENDIF
         CALL WKVECT(ZK24(LNOMF+IFONC-1),'V V R',NBVALF*2,LVAL)
         ZI(LADRF+IFONC-1) = LVAL
         DO 32 IVAL = 1, NBVALF
            ZR(LVAL-1+IVAL)        = ZR(JVAL-1+2*IVAL-1)
            ZR(LVAL-1+NBVALF+IVAL) = ZR(JVAL-1+2*IVAL)
 32      CONTINUE
         ZK24(LPRO+6+2*IFONC)(1:1) = ZK24(LPRO+4)(1:1)
         ZK24(LPRO+6+2*IFONC)(2:2) = ZK24(LPRO+6+2*IFONC)(1:1)
 30   CONTINUE
C
      CALL JECREC(NAPCIS//'.VALE','V V R',
     &                            'NU','CONTIG','VARIABLE',NBFONC)
      CALL FOSTON(NAPCIS//'.VALE',ZK24(LNOMF),NBFONC)
C
C     ##################################################################
C     ####DEFINITION DE LA FONCTION DES COEFFICIENTS DE CISAILLEMENT####
C     ######################SECTION = 'CERCLE'##########################
C     ##################################################################
C
      FONCIS = 'FONCTION'
C
C     NOMBRE DE VALEURS POUR LA FONCTION
      NBVFON = 14
C
      CALL WKVECT(FONCIS//'.PROL','V V K24',6,LPRO2)
      ZK24(LPRO2)   = 'FONCTION'
      ZK24(LPRO2+1) = 'LIN LIN '
      ZK24(LPRO2+2) = 'ALPHA'
      ZK24(LPRO2+3) = 'CCIS'
      ZK24(LPRO2+4) = 'EE'
      ZK24(LPRO2+5) = FONCIS
C
      CALL WKVECT('&&COECIS.VALEURS_FON','V V R',NBVFON*2,JVAL2)
C     VALEURS DE LA FONCTION
      ZR(JVAL2)    = 0.D0
      ZR(JVAL2+1)  = 1.16667D0
      ZR(JVAL2+2)  = 0.05D0
      ZR(JVAL2+3)  = 1.17442D0
      ZR(JVAL2+4)  = 0.1D0
      ZR(JVAL2+5)  = 1.19877D0
      ZR(JVAL2+6)  = 0.2D0
      ZR(JVAL2+7)  = 1.28947D0
      ZR(JVAL2+8)  = 0.3D0
      ZR(JVAL2+9)  = 1.41882D0
      ZR(JVAL2+10) = 0.4D0
      ZR(JVAL2+11) = 1.56277D0
      ZR(JVAL2+12) = 0.5D0
      ZR(JVAL2+13) = 1.69984D0
      ZR(JVAL2+14) = 0.6D0
      ZR(JVAL2+15) = 1.81536D0
      ZR(JVAL2+16) = 0.7D0
      ZR(JVAL2+17) = 1.90233D0
      ZR(JVAL2+18) = 0.8D0
      ZR(JVAL2+19) = 1.95983D0
      ZR(JVAL2+20) = 0.9D0
      ZR(JVAL2+21) = 1.99081D0
      ZR(JVAL2+22) = 0.95D0
      ZR(JVAL2+23) = 1.99781D0
      ZR(JVAL2+24) = 0.99D0
      ZR(JVAL2+25) = 1.99992D0
      ZR(JVAL2+26) = 1.D0
      ZR(JVAL2+27) = 2.D0
      CALL WKVECT(FONCIS//'.VALE','V V R',NBVFON*2,LVAL2)
      LFON = LVAL2 + NBVFON
      DO 20 IVAL2 = 0,NBVFON-1
         ZR(LVAL2+IVAL2) = ZR(JVAL2+2*IVAL2)
         ZR(LFON+IVAL2) = ZR(JVAL2+2*IVAL2+1)
 20   CONTINUE
C
      CALL JEDEMA()
      END
