      SUBROUTINE RVAFFM ( MCF, IOCC, SDLIEU, SDEVAL, SDMOY, OPER, QUANT,
     +                    OPTION, REP, NOMTAB, NCHEFF, I1, ISD )
      IMPLICIT   NONE
      INTEGER             I1, IOCC, ISD
      CHARACTER*16        NCHEFF, OPER
      CHARACTER*19        SDEVAL
      CHARACTER*24        SDLIEU, SDMOY
      CHARACTER*(*)       MCF, REP, OPTION, NOMTAB, QUANT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C     AFFICHAGE MOYENNE
C     ------------------------------------------------------------------
C IN  SDLIEU : K : SD DU LIEU TRAITEE
C IN  SDEVAL : K : SD DE L' EVALUATION DE LA QUANTITE SUR CE LIEU
C IN  SDMOY  : K : SD DES MOYENNES
C IN  OPER   : K : TYPE D'OPERATION: 'MOYENNE'
C IN  QUANT  : K : NOM DE LA QUANTITE TRAITEE
C IN  OPTION : K : NOM DE L' OPTION   TRAITEE
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      ANOCP, NBCP, IOC, AABSC, NBPT, NBOC, ASDMO
      INTEGER      I, IFM, ANOMND, NBCO, NBSP, K, NIV
      REAL*8       XA, XB, YA, YB, AX, S1, S2, ZA, ZB
      CHARACTER*4  DOCUL
      CHARACTER*24 NABSC, NNOCP
      CHARACTER*1  K1BID
C======================================================================
C
      CALL JEMARQ()
      CALL INFNIV ( IFM, NIV )
      IF (NIV.GT.1) CALL RVINFA(IFM,MCF,IOCC,QUANT,OPTION,OPER,REP(1:1))
      NNOCP = SDEVAL//'.NOCP'
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
         CALL JEVEUO(JEXNUM(SDMOY ,IOC),'L',ASDMO)
         S1 = ZR(AABSC + 1-1)
         S2 = ZR(AABSC + NBPT-1)
         IF (NIV.GT.1) THEN
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
     +                   ' ABSCISSES CURVILIGNES : (',S1,',',S2,')'
            ELSEIF ( DOCUL .EQ. 'ARCC' ) THEN
               XA = ZR(ANOMND + 1-1)
               YA = ZR(ANOMND + 2-1)
               XB = ZR(ANOMND + 3-1)
               AX = 57.29577951D0/XB
               WRITE(IFM,*)'ARC DE CERCLE'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' CENTRE                : (',XA,',',YA,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' RAYON                 :  ',XB
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                 ' SECTEUR ANGULAIRE     : (',AX*S1,',',AX*S2,')'
            ELSEIF ( DOCUL .EQ. 'SGT3' ) THEN
               XA = ZR(ANOMND + 1-1)
               YA = ZR(ANOMND + 2-1)
               ZA = ZR(ANOMND + 3-1)
               XB = ZR(ANOMND + 4-1)
               YB = ZR(ANOMND + 5-1)
               ZB = ZR(ANOMND + 6-1)
             AX = SQRT((XA-XB)*(XA-XB)+(YA-YB)*(YA-YB)+(ZA-ZB)*(ZA-ZB))
               WRITE(IFM,*)'SEGMENT DE DROITE'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1,1PD14.6,A1)')
     +                ' ORIGINE               : (',XA,',',YA,',',ZA,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1,1PD14.6,A1)')
     +                ' EXTREMITE             : (',XB,',',YB,',',ZB,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                ' ABSCISSES CURVILIGNES : (',S1,',',S2,')'
            ELSE
            ENDIF
            WRITE(IFM,*)' '
         ENDIF
         IF ( OPER .EQ. 'MOYENNE' ) THEN
            CALL RVTAMO ( ZR(ASDMO), ZK8(ANOCP), NBCP, NBCO, NBSP,
     +                    NOMTAB, IOCC, NCHEFF, I1, IOC, ISD )
         ELSE
            CALL RVRCCM ( ZR(ASDMO), NBCP, NBCO, NBSP,
     +                    NOMTAB, IOCC, NCHEFF, I1, IOC, ISD )
         ENDIF
100   CONTINUE
C
      CALL JEDEMA()
      END
