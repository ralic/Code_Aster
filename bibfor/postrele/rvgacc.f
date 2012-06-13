      SUBROUTINE RVGACC(IOCC,TYPAC,NIVAL,NRVAL,NBVAL)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      INTEGER       IOCC,NBVAL
      CHARACTER*2   TYPAC
      CHARACTER*24  NIVAL,NRVAL
C     ------------------------------------------------------------------
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C     ------------------------------------------------------------------
C     SAISIE DES VALEURS DES ENTIERS OU REELS PERMETTANT L' ACCES
C     AUX CHAMP EFFECTIFS D' UN CHAMP SYMBOLIQUE D' UNE SD RESULTAT
C     ------------------------------------------------------------------
C IN  IOCC   : I : NUMERO DE L' OCCURENCE TRAITEE
C IN  NIVAL  : K : NOM DE L' OJB DE SAISIE DES ENTIERS
C IN  NRVAL  : K : NOM DE L' OJB DE SAISIE DES REELS
C OUT NBVAL  : I : NOMBRE DE VALEURS SAISIES
C OUT TYPAC  : I : CODE DU TYPE D' ACCES (C.F. RVGCHF)
C     ------------------------------------------------------------------
C     LES OJB SONT DU GENRE 'V V SCAL' 'LONMAX' = NBVAL
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER      AIVAL, ARVAL, ALIST, I, IBID, N1, NBCM, NBIF, NBII,
     +             NBIM, NBIO, NBIS, NBNC, NBNF, NBNI, NBNM, NBNO, 
     +             NBR8, NBTO, NBTROU
      REAL*8       PREC, R8B
      COMPLEX*16   C16B
      CHARACTER*8  K8B, RESU, CRIT
      CHARACTER*16 NOMCAS
      CHARACTER*24 NLIST
      INTEGER      IARG
C
C================== CORPS DE LA ROUTINE ===============================
C
      CALL JEMARQ()
      CALL GETVID('ACTION','RESULTAT'  ,IOCC,IARG,1,RESU,N1)
      CALL GETVR8('ACTION','PRECISION' ,IOCC,IARG,1,PREC,N1)
      CALL GETVTX('ACTION','CRITERE'   ,IOCC,IARG,1,CRIT,N1)
      CALL GETVTX('ACTION','TOUT_ORDRE',IOCC,IARG,0,ZK80,NBTO)
      CALL GETVIS('ACTION','NUME_ORDRE',IOCC,IARG,0,ZI  ,NBNO)
      CALL GETVID('ACTION','LIST_ORDRE',IOCC,IARG,0,ZK8 ,NBIO)
      CALL GETVIS('ACTION','NUME_MODE' ,IOCC,IARG,0,ZI  ,NBNM)
      CALL GETVTX('ACTION','NOM_CAS'   ,IOCC,IARG,0,ZK16,NBNC)
      CALL GETVTX('ACTION','NOEUD_CMP' ,IOCC,IARG,0,ZK16,NBCM)
      CALL GETVID('ACTION','LIST_MODE' ,IOCC,IARG,0,ZK8 ,NBIM)
      CALL GETVR8('ACTION','INST'      ,IOCC,IARG,0,ZR  ,NBNI)
      CALL GETVID('ACTION','LIST_INST' ,IOCC,IARG,0,ZK8 ,NBII)
      CALL GETVR8('ACTION','FREQ'      ,IOCC,IARG,0,ZR  ,NBNF)
      CALL GETVID('ACTION','LIST_FREQ' ,IOCC,IARG,0,ZK8 ,NBIF)
      NBTO = -NBTO
      NBNO = -NBNO
      NBIO = -NBIO
      NBNM = -NBNM
      NBNC = -NBNC
      NBCM = -NBCM
      NBIM = -NBIM
      NBNI = -NBNI
      NBII = -NBII
      NBNF = -NBNF
      NBIF = -NBIF
      NBIS = NBNO + NBIO + NBNM + NBNC + NBCM + NBIM
      NBR8 = NBNI + NBII + NBNF + NBIF
      IF ( NBIS .NE. 0 ) THEN
         CALL WKVECT(NRVAL,'V V R',1,ARVAL)
         ZR(ARVAL) = 0.0D0
         IF (NBNO .NE. 0 ) THEN
            TYPAC = 'NO'
            NBVAL =  NBNO
            CALL WKVECT(NIVAL,'V V I',NBNO,AIVAL)
            CALL GETVIS('ACTION','NUME_ORDRE',IOCC,IARG,NBNO,
     &                  ZI(AIVAL),N1)
         ELSE IF (NBNM .NE. 0 ) THEN
            TYPAC = 'NM'
            NBVAL =  NBNM
            CALL WKVECT(NIVAL,'V V I',NBNM,AIVAL)
            CALL GETVIS('ACTION','NUME_MODE',IOCC,IARG,NBNM,
     &                  ZI(AIVAL),N1)
         ELSE IF (NBNC .NE. 0 ) THEN
            TYPAC = 'NO'
            NBVAL =  NBNC
            CALL WKVECT(NIVAL,'V V I',NBNC,AIVAL)
            CALL GETVTX('ACTION','NOM_CAS',IOCC,IARG,NBNC,NOMCAS,N1)
            CALL RSORAC(RESU,'NOM_CAS',IBID,R8B,NOMCAS,C16B,PREC,CRIT,
     +                                          ZI(AIVAL),1,NBTROU)
         ELSE IF (NBCM .NE. 0 ) THEN
            TYPAC = 'NO'
            NBVAL =  NBCM
            CALL WKVECT(NIVAL,'V V I',NBNC,AIVAL)
            CALL GETVTX('ACTION','NOEUD_CMP',IOCC,IARG,NBCM,NOMCAS,N1)
            CALL RSORAC(RESU,'NOEUD_CMP',IBID,R8B,NOMCAS,C16B,PREC,CRIT,
     +                                          ZI(AIVAL),1,NBTROU)
         ELSE
            IF ( NBIO .NE. 0 ) THEN
               TYPAC = 'NO'
               CALL GETVID('ACTION','LIST_ORDRE',IOCC,IARG,1,NLIST,N1)
            ELSE
               TYPAC = 'NM'
               CALL GETVID('ACTION','LIST_MODE',IOCC,IARG,1,NLIST,N1)
            ENDIF
            NLIST(9:24) = '           .VALE'
            CALL JELIRA(NLIST,'LONMAX',NBVAL,K8B)
            CALL JEVEUO(NLIST,'L',ALIST)
            CALL WKVECT(NIVAL,'V V I',NBVAL,AIVAL)
            DO 10, I = 1, NBVAL, 1
               ZI(AIVAL + I-1) = ZI(ALIST + I-1)
10          CONTINUE
         ENDIF
      ELSE IF ( NBR8 .NE. 0 ) THEN
         CALL WKVECT(NIVAL,'V V I',1   ,AIVAL)
         ZI(AIVAL) = 0
         IF (NBNI .NE. 0 ) THEN
            TYPAC = 'NI'
            NBVAL =  NBNI
            CALL WKVECT(NRVAL,'V V R',NBNI,ARVAL)
            CALL GETVR8('ACTION','INST',IOCC,IARG,NBNI,ZR(ARVAL),N1)
         ELSE IF (NBNF .NE. 0 ) THEN
            TYPAC = 'NF'
            NBVAL =  NBNF
            CALL WKVECT(NRVAL,'V V R',NBNF,ARVAL)
            CALL GETVR8('ACTION','FREQ',IOCC,IARG,NBNF,ZR(ARVAL),N1)
         ELSE
            IF ( NBII .NE. 0 ) THEN
               TYPAC = 'NI'
               CALL GETVID('ACTION','LIST_INST',IOCC,IARG,1,NLIST,N1)
            ELSE
               TYPAC = 'NF'
               CALL GETVID('ACTION','LIST_FREQ',IOCC,IARG,1,NLIST,N1)
            ENDIF
            NLIST(9:24) = '           .VALE'
            CALL JELIRA(NLIST,'LONMAX',NBVAL,K8B)
            CALL JEVEUO(NLIST,'L',ALIST)
            CALL WKVECT(NRVAL,'V V R',NBVAL,ARVAL)
            DO 20, I = 1, NBVAL, 1
               ZR(ARVAL + I-1) = ZR(ALIST + I-1)
20          CONTINUE
         ENDIF
      ELSE
         CALL WKVECT(NRVAL,'V V R',1,ARVAL)
         CALL WKVECT(NIVAL,'V V I',1,AIVAL)
         ZI(AIVAL) = 0
         ZR(ARVAL) = 0.0D0
         NBVAL = 0
         TYPAC = 'TO'
      ENDIF
      CALL JEDEMA()
      END
