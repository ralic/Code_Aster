      SUBROUTINE I3IMAS(EPSI,NIL,TETE,QUEUE,SUCC,PREC,DESC,DESCTM,
     +                  SGT,CONEX,VLC,COORDO,
     +                  SDRP1D,SDRPOM,NBSGTE)
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER      NIL,TETE,QUEUE,SUCC(*),PREC(*),DESC(*),DESCTM(*)
      INTEGER      CONEX(*),VLC(*),NBSGTE
      REAL*8       COORDO(*),SGT(*),EPSI
      CHARACTER*24 SDRP1D,SDRPOM
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     INTERSECTION MAILLAGE SEGMENT
C     CALCUL DES SD REPERAGE_1D ET REPERAGE_OMEGA
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  NIL    : I : POINTEUR A  NIL                 -+
C IN  TETE   : I : POINTEUR DE TETE                 !
C IN  QUEUE  : I : POINTEUR DE QUEUE                !--> SD L_MAILLE
C IN  SUCC   : I : POINTEUR SUR LES SUCCESSEURS     !
C IN  PREC   : I : POINTEUR SUR LES PREDECESSEURS   !
C IN  DESC   : I : CHAMP INFO = POINTEUR T_DESCTM  -+
C IN  DESCTM : I : POINTEUR SUR T_TYPE_MAILLE_3D
C IN  SGT    : R : COORDONNES DES EXTREMITEES DU SGT
C IN  CONEX  : I : TABLE DE CONNECTIVITE
C IN  VLC    : I : VECTEUR DE LONGUEURS CUMULEES SUR LA CONNECTIVITE
C IN  COORDO : R : TABLE DES COORDONEES
C OUT SDRP1D : K : PREFIXE DE LA SD REPERAGE_1D INTERMEDIARE
C OUT SDRPOM : K : PREFIXE DE LA SD REPERAGE_OM INTERMEDIARE
C OUT NBSGTE : K : NOMBRE DE SEGMENTS ELEMENTAIRES TROUVES
C     ------------------------------------------------------------------
C     L_MAILLE SUPPOSEE NON VIDE IE : TETE <> NIL
C     ------------------------------------------------------------------
      CHARACTER*8     KBID
C
C
      CHARACTER*24 R1D1,R1D2,R1D3
      CHARACTER*24 ROM1,ROM2,ROM3,ROM4,ROM5,ROM6,ROM7,ROM8,ROM9
      INTEGER      MXSGEL,PTSGEL,MAILLE,ALSTPT,NBSGEL,IRET,NBPT
      INTEGER      A1,A2,F1,F2,TF1,TF2,I1,I2,I,J,LND(4),NBND,LMA(20)
      INTEGER      AROM1,AROM2,AROM3,AROM4,AROM5,AROM6,AROM7,AROM8
      INTEGER      AR1D1,AR1D2,AR1D3,N1,NBMA,AROM9
      REAL*8       ZERO,UN,T1,T2
      LOGICAL      FINI,FIND,ATRV,BTRV,ADANSM,BDANSM
      CHARACTER*1 K1BID
C
C======================================================================
C
CCC   MXSGEL =  100
C-----------------------------------------------------------------------
      INTEGER J1 ,J2 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      MXSGEL =  10
      PTSGEL =  0
      UN     =  1.0D0
      ZERO   =  0.0D0
      FINI   = .FALSE.
      FIND   = .FALSE.
      ATRV   = .FALSE.
      BTRV   = .FALSE.
      ADANSM = .FALSE.
      BDANSM = .FALSE.
      CALL WKVECT('&&I3IMAS.LISTE.POINT','V V I',6,ALSTPT)
      CALL JECREO('&&I3IMAS.LSTPT.ABSC.SGT ','V V R')
      CALL JEECRA('&&I3IMAS.LSTPT.ABSC.SGT ','LONMAX',14,KBID)
      CALL JEECRA('&&I3IMAS.LSTPT.ABSC.SGT ','LONUTI',14,KBID)
      CALL JEVEUT('&&I3IMAS.LSTPT.ABSC.SGT ','E',ZI(ALSTPT+1-1))
      CALL JECREO('&&I3IMAS.LSTPT.FACE     ','V V I')
      CALL JEECRA('&&I3IMAS.LSTPT.FACE     ','LONMAX',14,KBID)
      CALL JEECRA('&&I3IMAS.LSTPT.FACE     ','LONUTI',14,KBID)
      CALL JEVEUT('&&I3IMAS.LSTPT.FACE     ','E',ZI(ALSTPT+2-1))
      CALL JECREO('&&I3IMAS.LSTPT.ARETE    ','V V I')
      CALL JEECRA('&&I3IMAS.LSTPT.ARETE    ','LONMAX',14,KBID)
      CALL JEECRA('&&I3IMAS.LSTPT.ARETE    ','LONUTI',14,KBID)
      CALL JEVEUT('&&I3IMAS.LSTPT.ARETE    ','E',ZI(ALSTPT+3-1))
      CALL JECREO('&&I3IMAS.LSTPT.TYPE.FACE','V V I')
      CALL JEECRA('&&I3IMAS.LSTPT.TYPE.FACE','LONMAX',14,KBID)
      CALL JEECRA('&&I3IMAS.LSTPT.TYPE.FACE','LONUTI',14,KBID)
      CALL JEVEUT('&&I3IMAS.LSTPT.TYPE.FACE','E',ZI(ALSTPT+4-1))
      CALL JECREO('&&I3IMAS.LSTPT.COORD.REF','V V R')
      CALL JEECRA('&&I3IMAS.LSTPT.COORD.REF','LONMAX',28,KBID)
      CALL JEECRA('&&I3IMAS.LSTPT.COORD.REF','LONUTI',28,KBID)
      CALL JEVEUT('&&I3IMAS.LSTPT.COORD.REF','E',ZI(ALSTPT+5-1))
      CALL JECREO('&&I3IMAS.LSTPT.ORDRE    ','V V I')
      CALL JEECRA('&&I3IMAS.LSTPT.ORDRE    ','LONMAX',14,KBID)
      CALL JEECRA('&&I3IMAS.LSTPT.ORDRE    ','LONUTI',14,KBID)
      CALL JEVEUT('&&I3IMAS.LSTPT.ORDRE    ','E',ZI(ALSTPT+6-1))
      R1D1 = SDRP1D(1:13)//'.SGTEL.ORIG'
      R1D2 = SDRP1D(1:13)//'.SGTEL.EXTR'
      R1D3 = SDRP1D(1:13)//'.SGTEL.TYPE'
      CALL JEEXIN(R1D1,IRET)
      IF ( IRET .EQ. 0 ) THEN
         CALL WKVECT(R1D1,'V V R',MXSGEL,AR1D1)
         CALL WKVECT(R1D2,'V V R',MXSGEL,AR1D2)
         CALL WKVECT(R1D3,'V V I',MXSGEL,AR1D3)
      ELSE
         CALL JEVEUO(R1D1,'E',AR1D1)
         CALL JEVEUO(R1D2,'E',AR1D2)
         CALL JEVEUO(R1D3,'E',AR1D3)
      ENDIF
      ROM1 = SDRPOM(1:13)//'.MAIL      '
      ROM2 = SDRPOM(1:13)//'.FACE .ORIG'
      ROM3 = SDRPOM(1:13)//'.FACE .EXTR'
      ROM4 = SDRPOM(1:13)//'.CREFM.ORIG'
      ROM5 = SDRPOM(1:13)//'.CREFM.EXTR'
      ROM6 = SDRPOM(1:13)//'.ARETE.ORIG'
      ROM7 = SDRPOM(1:13)//'.ARETE.EXTR'
      ROM8 = SDRPOM(1:13)//'.CREFF.ORIG'
      ROM9 = SDRPOM(1:13)//'.CREFF.EXTR'
      CALL JEEXIN(ROM2,IRET)
      IF ( IRET .EQ. 0 ) THEN
         CALL WKVECT(ROM2,'V V I',  MXSGEL,AROM2)
         CALL WKVECT(ROM3,'V V I',  MXSGEL,AROM3)
         CALL WKVECT(ROM4,'V V R',3*MXSGEL,AROM4)
         CALL WKVECT(ROM5,'V V R',3*MXSGEL,AROM5)
         CALL WKVECT(ROM6,'V V I',  MXSGEL,AROM6)
         CALL WKVECT(ROM7,'V V I',  MXSGEL,AROM7)
         CALL WKVECT(ROM8,'V V R',2*MXSGEL,AROM8)
         CALL WKVECT(ROM9,'V V R',2*MXSGEL,AROM9)
      ELSE
         CALL JEVEUO(ROM2,'E',AROM2)
         CALL JEVEUO(ROM3,'E',AROM3)
         CALL JEVEUO(ROM4,'E',AROM4)
         CALL JEVEUO(ROM5,'E',AROM5)
         CALL JEVEUO(ROM6,'E',AROM6)
         CALL JEVEUO(ROM7,'E',AROM7)
         CALL JEVEUO(ROM8,'E',AROM8)
         CALL JEVEUO(ROM9,'E',AROM9)
         CALL JELIRA(ROM1,'NMAXOC',MXSGEL,K1BID)
         CALL JEDETR(ROM1)
      ENDIF
      CALL JECREC(ROM1,'V V I','NU','DISPERSE','VARIABLE',MXSGEL)
      FINI = ( (TETE .EQ. NIL) .OR. FINI .OR. FIND )
10    CONTINUE
      IF ( .NOT. FINI ) THEN
         MAILLE = TETE
         CALL I3IDKS(EPSI,MAILLE,DESC,DESCTM,SGT,ATRV,BTRV,
     +               CONEX(VLC(MAILLE)),COORDO,NBPT,ZI(ALSTPT),FIND)
         IF ( ABS(NBPT) .GT. 0 ) THEN
            CALL I3TRIP(ZI(ALSTPT),ABS(NBPT))
            J1     = ZI(ZI(ALSTPT+6-1)+1-1)
            J2     = ZI(ZI(ALSTPT+6-1)+ABS(NBPT)-1)
            T1     = ZR(ZI(ALSTPT+1-1)+J1-1)
            T2     = ZR(ZI(ALSTPT+1-1)+J2-1)
            ADANSM = ( ABS(T1)    .LE. EPSI )
            BDANSM = ( ABS(T2-UN) .LE. EPSI )
            ATRV   = ( ADANSM .OR. ATRV )
            BTRV   = ( BDANSM .OR. BTRV )
         ENDIF
         NBSGEL = MAX(0,ABS(NBPT)-1)
         IF ( NBPT .EQ. 0 ) THEN
            IF ( (.NOT. ATRV) .AND. (.NOT. BTRV) ) THEN
               CALL I3PDM3(EPSI,MAILLE,DESC,DESCTM,
     +                     CONEX(VLC(MAILLE)),COORDO,SGT,ADANSM)
               ATRV = ADANSM
               IF (ADANSM ) THEN
                  CALL I3PDM3(EPSI,MAILLE,DESC,DESCTM,
     +                        CONEX(VLC(MAILLE)),COORDO,SGT(4),BDANSM)
                  BTRV = BDANSM
                  IF ( BDANSM ) THEN
                     FIND   = .TRUE.
                     NBSGEL = 1
                     NBPT   = 2
                     ZR(ZI(ALSTPT + 1-1) + 1-1) = ZERO
                     ZI(ZI(ALSTPT + 2-1) + 1-1) = 0
                     ZI(ZI(ALSTPT + 3-1) + 1-1) = 0
                     ZI(ZI(ALSTPT + 4-1) + 1-1) = 0
                     ZR(ZI(ALSTPT + 5-1) + 1-1) = ZERO
                     ZR(ZI(ALSTPT + 5-1) + 2-1) = ZERO
                     ZI(ZI(ALSTPT + 6-1) + 1-1) = 1
                     ZR(ZI(ALSTPT + 1-1) + 2-1) = UN
                     ZI(ZI(ALSTPT + 2-1) + 2-1) = 0
                     ZI(ZI(ALSTPT + 3-1) + 2-1) = 0
                     ZI(ZI(ALSTPT + 4-1) + 2-1) = 0
                     ZR(ZI(ALSTPT + 5-1) + 3-1) = ZERO
                     ZR(ZI(ALSTPT + 5-1) + 4-1) = ZERO
                     ZI(ZI(ALSTPT + 6-1) + 2-1) = 2
                  ELSE
                     BTRV = .FALSE.
                     ATRV = .FALSE.
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         IF ( NBPT .GT. 0 ) THEN
            IF ( (.NOT. ADANSM) .AND. (.NOT. ATRV) ) THEN
               CALL I3PDM3(EPSI,MAILLE,DESC,DESCTM,
     +                     CONEX(VLC(MAILLE)),COORDO,SGT,ADANSM)
               ATRV = ( ADANSM .OR. ATRV )
               IF ( ADANSM ) THEN
                  ZR(ZI(ALSTPT + 1-1) +   NBPT+1-1) = ZERO
                  ZI(ZI(ALSTPT + 2-1) +   NBPT+1-1) = 0
                  ZI(ZI(ALSTPT + 3-1) +   NBPT+1-1) = 0
                  ZI(ZI(ALSTPT + 4-1) +   NBPT+1-1) = 0
                  ZR(ZI(ALSTPT + 5-1) + 2*NBPT+1-1) = ZERO
                  ZR(ZI(ALSTPT + 5-1) + 2*NBPT+2-1) = ZERO
                  ZI(ZI(ALSTPT + 6-1) +   NBPT+1-1) = NBPT + 1
                  NBSGEL = NBSGEL + 1
                  NBPT   = NBPT + 1
               ENDIF
            ENDIF
            IF ( (.NOT. BDANSM) .AND. (.NOT. BTRV) ) THEN
               CALL I3PDM3(EPSI,MAILLE,DESC,DESCTM,
     +                     CONEX(VLC(MAILLE)),COORDO,SGT(4),BDANSM)
               BTRV = ( BDANSM .OR. BTRV )
               IF ( BDANSM ) THEN
                  ZR(ZI(ALSTPT + 1-1) +   NBPT+1-1) = UN
                  ZI(ZI(ALSTPT + 2-1) +   NBPT+1-1) = 0
                  ZI(ZI(ALSTPT + 3-1) +   NBPT+1-1) = 0
                  ZI(ZI(ALSTPT + 4-1) +   NBPT+1-1) = 0
                  ZR(ZI(ALSTPT + 5-1) + 2*NBPT+1-1) = ZERO
                  ZR(ZI(ALSTPT + 5-1) + 2*NBPT+2-1) = ZERO
                  ZI(ZI(ALSTPT + 6-1) +   NBPT+1-1) = NBPT + 2
                  NBSGEL = NBSGEL + 1
                  NBPT   = NBPT + 1
               ENDIF
            ENDIF
         ENDIF
         CALL I3TRIP(ZI(ALSTPT),ABS(NBPT))
         IF ( (PTSGEL + NBSGEL) .GT. MXSGEL ) THEN
            CALL JECREC('&&I3IMAS.XD.TMP','V V I',
     +                  'NU','DISPERSE','VARIABLE',MXSGEL)
CCC         MXSGEL = MXSGEL + 100
            MXSGEL = MXSGEL + 10
            CALL JUVECA(R1D1,  MXSGEL)
            CALL JEVEUO(R1D1,'E',AR1D1)
            CALL JUVECA(R1D2,  MXSGEL)
            CALL JEVEUO(R1D2,'E',AR1D2)
            CALL JUVECA(R1D3,  MXSGEL)
            CALL JEVEUO(R1D3,'E',AR1D3)
            CALL JUVECA(ROM2,  MXSGEL)
            CALL JEVEUO(ROM2,'E',AROM2)
            CALL JUVECA(ROM3,  MXSGEL)
            CALL JEVEUO(ROM3,'E',AROM3)
            CALL JUVECA(ROM4,3*MXSGEL)
            CALL JEVEUO(ROM4,'E',AROM4)
            CALL JUVECA(ROM5,3*MXSGEL)
            CALL JEVEUO(ROM5,'E',AROM5)
            CALL JUVECA(ROM6,  MXSGEL)
            CALL JEVEUO(ROM6,'E',AROM6)
            CALL JUVECA(ROM7,  MXSGEL)
            CALL JEVEUO(ROM7,'E',AROM7)
            CALL JUVECA(ROM8,2*MXSGEL)
            CALL JEVEUO(ROM8,'E',AROM8)
            CALL JUVECA(ROM9,2*MXSGEL)
            CALL JEVEUO(ROM9,'E',AROM9)
            DO 20, I = 1, PTSGEL, 1
               CALL JELIRA(JEXNUM(ROM1,I),'LONMAX',N1,K1BID)
               CALL JEVEUO(JEXNUM(ROM1,I),'L',I2)
               CALL JECROC(JEXNUM('&&I3IMAS.XD.TMP',I))
               CALL JEECRA(JEXNUM('&&I3IMAS.XD.TMP',I),'LONMAX',N1,' ')
               CALL JEVEUO(JEXNUM('&&I3IMAS.XD.TMP',I),'E',I1)
               DO 21, J = 1, N1, 1
                  ZI(I1 + J-1) = ZI(I2 + J-1)
21             CONTINUE
20          CONTINUE
            CALL JEDETR(ROM1)
            CALL JECREC(ROM1,'V V I','NU','DISPERSE','VARIABLE',MXSGEL)
            DO 30, I = 1, PTSGEL, 1
               CALL JELIRA(JEXNUM('&&I3IMAS.XD.TMP',I),'LONMAX',
     +                     N1,K1BID)
               CALL JEVEUO(JEXNUM('&&I3IMAS.XD.TMP',I),'L',I2)
               CALL JECROC(JEXNUM(ROM1,I))
               CALL JEECRA(JEXNUM(ROM1,I),'LONMAX',N1,' ')
               CALL JEVEUO(JEXNUM(ROM1,I),'E',I1)
               DO 31, J = 1, N1, 1
                  ZI(I1 + J-1) = ZI(I2 + J-1)
31             CONTINUE
30          CONTINUE
            CALL JEDETR('&&I3IMAS.XD.TMP')
         ENDIF
         DO 100, I = 1, NBSGEL, 1
            J1  = ZI(ZI(ALSTPT+6-1)+I-1)
            J2  = ZI(ZI(ALSTPT+6-1)+I  )
            T1  = ZR(ZI(ALSTPT+1-1)+J1-1)
            T2  = ZR(ZI(ALSTPT+1-1)+J2-1)
            F1  = ZI(ZI(ALSTPT+2-1)+J1-1)
            F2  = ZI(ZI(ALSTPT+2-1)+J2-1)
            A1  = ZI(ZI(ALSTPT+3-1)+J1-1)
            A2  = ZI(ZI(ALSTPT+3-1)+J2-1)
            TF1 = ZI(ZI(ALSTPT+4-1)+J1-1)
            TF2 = ZI(ZI(ALSTPT+4-1)+J2-1)
            IF ( (F1 .EQ. F2) .AND. (F1 .GT. 0) ) THEN
               N1    = DESCTM(DESC(MAILLE))
               NBND  = ZI(N1-1 + 2 + F1)
               IF ( (A1 .EQ. A2) .AND. (A1 .GT. 0) ) THEN
                  I1     = ZI(N1-1 + 8+F1+(A1-1)*6)
                  I2     = MAX(1,MOD(A1+1,NBND+1))
                  LND(1) = CONEX(VLC(MAILLE)+I1-1)
                  I1     = ZI(N1-1 + 8+F1+(I2-1)*6)
                  LND(2) = CONEX(VLC(MAILLE)+I1-1)
                  NBND   = 2
                  I1     =-1
                  I2     = 1
               ELSE
                  DO 110, J = 1, NBND, 1
                     I1     = ZI(N1-1 + 8+F1+(J-1)*6)
                     LND(J) = CONEX(VLC(MAILLE)-1+I1)
110               CONTINUE
                  I1 = 2
                  I2 = 2
               ENDIF
               CALL I3FMVN(NIL,DESC,SUCC,PREC,DESCTM,MAILLE,
     +                     CONEX,VLC,LND,NBND,I1,NBMA,LMA)
               IF ( (TF1 .EQ. -1) .AND. ( TF2 .EQ. -1) ) THEN
                  IF ( NBMA .LE. 1 ) THEN
                     I2 = 0
                  ELSE
                     I2 = 3
                     NBMA = 1
                     LMA(1) = LMA(2)
                  ENDIF
               ENDIF
            ELSE
               I2 = 3
               LMA(1) = MAILLE
               NBMA   = 1
            ENDIF
            ZR(AR1D1 +    PTSGEL     ) = T1
            ZR(AR1D2 +    PTSGEL     ) = T2
            ZI(AR1D3 +    PTSGEL     ) = I2
            ZI(AROM2 +    PTSGEL     ) = F1
            ZI(AROM3 +    PTSGEL     ) = F2
            ZI(AROM6 +    PTSGEL     ) = A1
            ZI(AROM7 +    PTSGEL     ) = A2
            ZR(AROM8 + 2* PTSGEL     ) = ZR(ZI(ALSTPT+5-1)+2*(J1-1))
            ZR(AROM8 + 2* PTSGEL   +1) = ZR(ZI(ALSTPT+5-1)+2*(J1-1)+1)
            ZR(AROM9 + 2* PTSGEL     ) = ZR(ZI(ALSTPT+5-1)+2*(J2-1))
            ZR(AROM9 + 2* PTSGEL   +1) = ZR(ZI(ALSTPT+5-1)+2*(J2-1)+1)
            CALL I3CRK3(DESC(MAILLE),F1,ZR(ZI(ALSTPT+5-1)+2*(J1-1)),
     +                  ZR(AROM4 + 3*PTSGEL))
            CALL I3CRK3(DESC(MAILLE),F2,ZR(ZI(ALSTPT+5-1)+2*(J2-1)),
     +                  ZR(AROM5 + 3*PTSGEL))
            CALL JECROC(JEXNUM(ROM1,PTSGEL+1))
            CALL JEECRA(JEXNUM(ROM1,PTSGEL+1),'LONMAX',NBMA,' ')
            CALL JEVEUO(JEXNUM(ROM1,PTSGEL+1),'E',AROM1)
            DO 120, J = 1, NBMA, 1
               ZI(AROM1 + J-1) = LMA(J)
120         CONTINUE
            PTSGEL = PTSGEL + 1
            CALL I3LCHS(NIL,TETE,QUEUE,SUCC,PREC,LMA(2),NBMA-1)
100      CONTINUE
         LMA(1) = MAILLE
         NBMA   = 1
         CALL I3LCHS(NIL,TETE,QUEUE,SUCC,PREC,LMA,NBMA)
         FINI = ( (TETE .EQ. NIL) .OR. FINI .OR. FIND )
         GOTO 10
      ENDIF
      NBSGTE = PTSGEL
      CALL JEDETR('&&I3IMAS.LISTE.POINT')
      CALL JEDETR('&&I3IMAS.LSTPT.ABSC.SGT ')
      CALL JEDETR('&&I3IMAS.LSTPT.FACE     ')
      CALL JEDETR('&&I3IMAS.LSTPT.ARETE    ')
      CALL JEDETR('&&I3IMAS.LSTPT.TYPE.FACE')
      CALL JEDETR('&&I3IMAS.LSTPT.COORD.REF')
      CALL JEDETR('&&I3IMAS.LSTPT.ORDRE    ')
      CALL JEDEMA()
      END
