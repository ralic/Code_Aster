      SUBROUTINE OP0024(IER)
      IMPLICIT NONE
      INTEGER IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
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
C RESPONSABLE VABHHTS J.PELLET
C     ------------------------------------------------------------------

C     COMMANDE : DEFI_LIST_REEL

C     ON CREE LES OBJETS CONTENANT LA LISTE DE REELS:
C     -----------------------------------------------
C     RESU  .LPAS = DT_1 ,DT_2 ,... ,DT_N
C                   DT_I : PAS DE TEMPS DE L'INTERVALLE I
C     RESU  .NBPA = NPT_1 ,NPT_2 ,... ,NPT_N
C                   NPT_I : NOMBRE DE PAS DE TEMPS DE L'INTERVALLE I
C     RESU  .BINT = B_0 ,B_1 ,B_2 ,... ,B_N
C                   B_I : BORNE DE L'INTERVALLE DONNE PAR L'UTILISATEUR
C     RESU  .VALE = I_0 ,I_1 ,I_2 ,... ,I_N
C                   I_I : VALEUR DU I-EME PAS

C     ------------------------------------------------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      REAL*8 R8B,DEBUT,FIN,PAS,XXX,XPDT,TOLER,DERPAS
      INTEGER IFM,NIV,NV,NBVALE,NDIM,JPAS,JNBP,JBOR,JVAL,KVAL,I
      INTEGER N1,NBOCC,NSUP,IOCC,NP,NBPAS,NBVAL,IINTER,ICO,J
      CHARACTER*19 RESU
      CHARACTER*16 NOMCMD,CONCEP
C     ------------------------------------------------------------------
      CALL JEMARQ()

      CALL INFMAJ
      CALL INFNIV(IFM,NIV)



      CALL GETRES(RESU,CONCEP,NOMCMD)
      CALL GETVR8(' ','VALE',0,1,0,R8B,NV)


C     CAS DU MOT CLE VALE=
C    ----------------------
      IF (NV.NE.0) THEN
        NBVALE = -NV
        NDIM = MAX(1,NBVALE-1)
        CALL WKVECT(RESU//'.LPAS','G V R',NDIM,JPAS)
        CALL WKVECT(RESU//'.NBPA','G V I',NDIM,JNBP)
        CALL WKVECT(RESU//'.BINT','G V R',NBVALE,JBOR)
        CALL WKVECT(RESU//'.VALE','G V R',NBVALE,JVAL)
        CALL WKVECT('&&OP0024.VALE','V V R',NBVALE,KVAL)
        CALL GETVR8(' ','VALE',0,1,NBVALE,ZR(KVAL),NV)
        DO 10 I = 1,NBVALE - 1
          ZR(JPAS+I-1) = ZR(KVAL+I) - ZR(KVAL+I-1)
          ZI(JNBP+I-1) = 1
          ZR(JBOR+I-1) = ZR(KVAL+I-1)
          ZR(JVAL+I-1) = ZR(KVAL+I-1)
   10   CONTINUE
        ZR(JBOR+NBVALE-1) = ZR(KVAL+NBVALE-1)
        ZR(JVAL+NBVALE-1) = ZR(KVAL+NBVALE-1)


C     CAS DU MOT CLE INTERVALLE=
C    ----------------------------
      ELSE
        CALL GETVR8(' ','DEBUT',0,1,1,DEBUT,N1)
        CALL GETFAC('INTERVALLE',NBOCC)
        TOLER = 1.D-3

C       -- ON COMPTE LE NOMBRE D'INTERVALLES SUPPLEMENTAIRES (NSUP)
C          QU'IL FAUDRA CREER DU FAIT DES ARRONDIS (MOT CLE PAS)
C       ---------------------------------------------------------
        NSUP = 0
        DO 20 IOCC = 1,NBOCC
          CALL GETVR8('INTERVALLE','JUSQU_A',IOCC,1,1,FIN,N1)
          CALL GETVR8('INTERVALLE','PAS',IOCC,1,1,PAS,NP)
          IF (NP.EQ.1) THEN
            IF (PAS.EQ.0.D0) CALL UTMESS('F','OP0025',
     &                                   'LE PAS'//' EST NUL')

            NBPAS = NINT((FIN-DEBUT)/PAS)
            IF (NBPAS.LE.0) CALL UTMESS('F','OP0025',
     &             'LE NOMBRE DE PAS EST NEGATIF')

            DERPAS = FIN - (DEBUT+ (NBPAS-1)*PAS)
            IF (ABS((DERPAS-PAS)/PAS).GT.TOLER) NSUP = NSUP + 1
          END IF
          DEBUT = FIN
   20   CONTINUE



        CALL WKVECT(RESU//'.LPAS','G V R',NBOCC+NSUP,JPAS)
        CALL WKVECT(RESU//'.NBPA','G V I',NBOCC+NSUP,JNBP)
        CALL WKVECT(RESU//'.BINT','G V R',NBOCC+NSUP+1,JBOR)

        CALL GETVR8(' ','DEBUT',0,1,1,DEBUT,N1)
        ZR(JBOR-1+1) = DEBUT
        NBVAL = 1
        IINTER = 0
        DO 30 IOCC = 1,NBOCC
          IINTER = IINTER + 1
          CALL GETVR8('INTERVALLE','JUSQU_A',IOCC,1,1,FIN,N1)

          XXX = FIN - DEBUT
          CALL GETVR8('INTERVALLE','PAS',IOCC,1,1,PAS,NP)

          IF (NP.EQ.1) THEN
            NBPAS = NINT(XXX/PAS)

            DERPAS = FIN - (DEBUT+ (NBPAS-1)*PAS)
            IF (ABS((DERPAS-PAS)/PAS).GT.TOLER) THEN
              IF ((DEBUT+NBPAS*PAS).GT.FIN) NBPAS = NBPAS - 1
              ZI(JNBP-1+IINTER) = NBPAS
              ZR(JPAS-1+IINTER) = PAS
              ZR(JBOR-1+IINTER+1) = DEBUT+PAS*NBPAS
              NBVAL = NBVAL + NBPAS

C             -- CREATION D'UN INTERVALLE SUPPLEMENTAIRE:
              IINTER = IINTER + 1
              ZI(JNBP-1+IINTER) = 1
              ZR(JPAS-1+IINTER) = FIN - (DEBUT+PAS*NBPAS)
              ZR(JBOR-1+IINTER+1) = FIN
              NBVAL = NBVAL + 1

              CALL UTDEBM('A',NOMCMD,'LA DISTANCE ENTRE LES '//
     &                    'DEUX DERNIERS REELS NE SERA PAS EGAL')
              CALL UTIMPR('S',' AU PAS COURANT : ',1,PAS)
              CALL UTIMPI('S',', POUR L''INTERVALLE ',1,IOCC)
              CALL UTFINM()



            ELSE
              ZI(JNBP-1+IINTER) = NBPAS
              ZR(JPAS-1+IINTER) = PAS
              ZR(JBOR-1+IINTER+1) = FIN
              NBVAL = NBVAL + NBPAS
            END IF

          ELSE
            CALL GETVIS('INTERVALLE','NOMBRE',IOCC,1,1,NBPAS,N1)
            IF (NBPAS.LE.0) CALL UTMESS('F','OP0025',
     &            'LE NOMBRE DE PAS EST NEGATIF')
            ZI(JNBP-1+IINTER) = NBPAS
            ZR(JPAS-1+IINTER) = XXX/NBPAS
            ZR(JBOR-1+IINTER+1) = FIN
            NBVAL = NBVAL + NBPAS
          END IF
          DEBUT = FIN
   30   CONTINUE


C       -- CREATION DE L'OBJET .VALE :
C       -------------------------------
        CALL WKVECT(RESU//'.VALE','G V R',NBVAL,JVAL)
        ZR(JVAL) = ZR(JBOR)
        ICO = 0
        DO 50 I = 1,NBOCC + NSUP
          XPDT = ZR(JPAS-1+I)
          DO 40 J = 1,ZI(JNBP-1+I) - 1
            ICO = ICO + 1
            ZR(JVAL+ICO) = ZR(JVAL+ICO-1) + XPDT
   40     CONTINUE
          ICO = ICO + 1
          ZR(JVAL+ICO) = ZR(JBOR+I)
   50   CONTINUE
      END IF


C     --- TITRE ---
      CALL TITRE


C     --- IMPRESSION ---
      IF (NIV.GT.1) CALL LIIMPR(RESU,NIV,'MESSAGE')

      CALL JEDEMA()
      END
