      SUBROUTINE RECFOU(ICHPT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           ICHPT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/01/2002   AUTEUR DURAND C.DURAND 
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
C     OPERATEUR COMB_CHAM_NO, COMB_CHAM_ELEM
C     RECOMBINAISON DE FOURIER SUR LES CHAMPS
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
      REAL*8       R8DGRD
      CHARACTER*8  CHPRES, K8B, REP, REP2
      CHARACTER*16 CONCEP, NOMCMD
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(CHPRES,CONCEP,NOMCMD)
      CALL GETFAC('COMB_FOURIER',NBOCC)
C
      CALL WKVECT('&&RECFOU.NOM' ,'V V K8',NBOCC,LNOM )
      CALL WKVECT('&&RECFOU.MODE','V V I' ,NBOCC,LMODE)
      CALL WKVECT('&&RECFOU.TYPE','V V K8',NBOCC,LTYPE)
      CALL WKVECT('&&RECFOU.COEF','V V R8',NBOCC,LCOEF)
C
      IF (ICHPT.EQ.1) THEN
         DO 10 IOCC = 1,NBOCC
            CALL GETVID('COMB_FOURIER','CHAM_NO'  ,IOCC,1,1,
     +                                             ZK8(LNOM-1+IOCC),L)
            CALL GETVIS('COMB_FOURIER','NUME_MODE',IOCC,1,1,
     +                                             ZI(LMODE-1+IOCC),L)
            CALL GETVTX('COMB_FOURIER','TYPE_MODE',IOCC,1,1,
     +                                             ZK8(LTYPE-1+IOCC),L)
            CALL GETVR8('COMB_FOURIER','COEF_R'   ,IOCC,1,1,
     +                                             ZR(LCOEF-1+IOCC),L)
 10      CONTINUE
      ELSEIF (ICHPT.EQ.2) THEN
         DO 12 IOCC = 1,NBOCC
            CALL GETVID('COMB_FOURIER','CHAM_ELEM',IOCC,1,1,
     +                                             ZK8(LNOM-1+IOCC),L)
            CALL GETVIS('COMB_FOURIER','NUME_MODE',IOCC,1,1,
     +                                             ZI(LMODE-1+IOCC),L)
            CALL GETVTX('COMB_FOURIER','TYPE_MODE',IOCC,1,1,
     +                                             ZK8(LTYPE-1+IOCC),L)
            CALL GETVR8('COMB_FOURIER','COEF_R'   ,IOCC,1,1,
     +                                             ZR(LCOEF-1+IOCC),L)
 12      CONTINUE
      ENDIF
      CALL GETVR8(' ','ANGL',0,1,1,ANGLE,L)
      ANGLE = ANGLE * R8DGRD()
C
C
C     --- VERIFICATION DE NON PRESENCE DU RESULTAT EN ARGUMENT ---
C
      DO 20 IOCC = 0,NBOCC - 1
         IF (CHPRES.EQ.ZK8(LNOM+IOCC)) THEN
            CALL CODENT(IOCC+1,'D',K8B(1:3))
            CALL UTMESS('F',NOMCMD,K8B(1:3)//
     +             '-EME OCCURRENCE DU MOT-CLE FACTEUR MODE_FOURIER'
     +              //' LE CHAM_NO RESULTAT NE DOIT PAS APPARAITRE'
     +              //' DANS LES ARGUMENTS.')
         ENDIF
   20 CONTINUE
C
C
C     --- VERIFICATION DU TYPE DES CHAMPS A RECOMBINER ---
C
      CALL DISMOI('F','NOM_GD',ZK8(LNOM),'CHAMP',IBID,REP,IE)
      DO 22 IOCC = 1,NBOCC-1
         CALL DISMOI('F','NOM_GD',ZK8(LNOM+IOCC),'CHAMP',IBID,REP2,IE)
         IF (REP2.NE.REP) THEN
            CALL UTMESS('F',NOMCMD,'ERREUR MOT-CLE FACTEUR MODE_FOURIER'
     +                 //': DANS 2 OCCURENCES DIFFERENTES ON TROUVE'
     +                 //' DES CHAMPS ASSOCIES A DES GRANDEURS DIFF'
     +                 //'ERENTES. LA RECOMBINAISON EST IMPOSSIBLE')
         ENDIF
 22   CONTINUE
C
C     --- CONTROLE DES REFERENCES ---
      IF (ICHPT.EQ.1) THEN
         DO 30 IOCC = 0,NBOCC-2
            CALL VRREFE(ZK8(LNOM+IOCC),ZK8(LNOM+IOCC+1),IRET)
            IF (IRET.NE.0) THEN
               CALL UTMESS('F',NOMCMD,
     +                     'LES "CHAM_NO" "'//ZK8(LNOM+IOCC)//
     +                     '"  ET  "'//ZK8(LNOM+IOCC+1)//
     +                     '"  N''ONT PAS LA MEME NUMEROTATION.')
            ENDIF
 30      CONTINUE
      ELSEIF (ICHPT.EQ.2) THEN
         DO 32 IOCC = 0,NBOCC-2
            CALL VRDESC(ZK8(LNOM+IOCC),ZK8(LNOM+IOCC+1),IRE1)
            CALL VRNOLI(ZK8(LNOM+IOCC),ZK8(LNOM+IOCC+1),IRE2)
            IRET = IRE1 + IRE2
            IF (IRET.NE.0) THEN
               CALL UTMESS('F',NOMCMD,'LES "CHAM_ELEM" "'//
     +                      ZK8(LNOM+IOCC)//'"  ET  "'//ZK8(LNOM+IOCC+1)
     +                        //'"  N''ONT PAS LES MEMES LIGRELS.')
            ENDIF
 32      CONTINUE
      ENDIF
C
C     --- CREATION OU VERIFICATION DE COHERENCE DU CHAMP RESULTAT ---
      CALL EXISD('CHAMP_GD',CHPRES,IRET)
      IF (IRET.EQ.0) THEN
C        --- ON CREE ---
         CALL VTDEFS(CHPRES,ZK8(LNOM),'G','R')
      ELSE
C        --- ON VERIFIE ---
         IF (ICHPT.EQ.1) THEN
            CALL VRREFE(CHPRES,ZK8(LNOM),IRET)
            IF (IRET.NE.0) THEN
               CALL UTMESS('F',NOMCMD,'LE "CHAM_NO" RESULTAT "'//
     +                         CHPRES//'"  ET  "'//ZK8(LNOM)//
     +                         '"  N''ONT PAS LA MEME NUMEROTATION.')
            ENDIF
         ELSEIF (ICHPT.EQ.2) THEN
            CALL VRDESC(CHPRES,ZK8(LNOM),IRE1)
            CALL VRNOLI(CHPRES,ZK8(LNOM),IRE2)
            IRET = IRE1 + IRE2
            IF (IRET.NE.0) THEN
               CALL UTMESS('F',NOMCMD,'LE "CHAM_ELEM" RESULTAT "'//
     +                         CHPRES//'"  ET  "'//ZK8(LNOM)//
     +                         '"  N''ONT PAS LES MEMES LIGRELS.')
            ENDIF
         ENDIF
      ENDIF
C
C     ------ COMBINAISON DES CHAMPS ------------------------------------
      CALL REFODE(NBOCC,ANGLE,ZK8(LNOM),ZI(LMODE),
     +                                ZK8(LTYPE),ZR(LCOEF),'V',CHPRES)
C     ------------------------------------------------------------------
      CALL JEDETR('&&RECFOU.NOM' )
      CALL JEDETR('&&RECFOU.MODE')
      CALL JEDETR('&&RECFOU.TYPE')
      CALL JEDETR('&&RECFOU.COEF')
 9999 CONTINUE
      CALL JEDEMA()
      END
