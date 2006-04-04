      SUBROUTINE OPS007 ( ICMD, ICOND, IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER ICMD,ICOND,IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 03/04/2006   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C     OPERATEUR DESTRUCTION DE CONCEPT ET D'OBJETS JEVEUX
C     ------------------------------------------------------------------
C
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      CHARACTER*1 KLAS
      CHARACTER*6 NOMPRO
      CHARACTER*8 KBID
      CHARACTER*8 LERESU
      CHARACTER*3 ALARME
      CHARACTER*32 KCH
      CHARACTER*16 TYPECO  
      CHARACTER*24  NORECG
      INTEGER LBID, IEX
      INTEGER NCON, LCON, NSENS
      INTEGER IRET,NRPASS,NBPASS,ADRECG

      PARAMETER (NOMPRO='OPS007')
C
      CALL INFMAJ()
      NORECG = '&&'//NOMPRO//'_RESULTA_GD     '
      IF (ICOND.EQ.0) THEN
         CALL JEMARQ()
         CALL INFNIV(IFM,NIV)
         IF (NIV.GT.1) LBID=JVINFO('AFFECT', NIV)
         CALL GETVTX(' ','ALARME',0,1,1,ALARME,LBID)
         CALL GETFAC('CONCEPT',NBOCC)
         DO 3 IOCC = 1,NBOCC

C NON DES CONCEPTS

            CALL GETVID('CONCEPT','NOM',IOCC,1,0,KBID,NCON)
            NCON = -NCON
            IF (NCON.GT.0) THEN
              CALL WKVECT ('&&OPS007.LISTE_CO','V V K8',NCON,LCON)
              CALL GETVID('CONCEPT','NOM',IOCC,1,NCON,ZK8(LCON),LBID)
              CALL GETVID('CONCEPT','SENSIBILITE',IOCC,1,0,KBID,NSENS)
C
              DO 300 II=1,NCON
                 IF(NSENS.EQ.0) THEN
                   CALL JEDETC('G',ZK8(LCON-1+II),1)
                   CALL GCDETC(ZK8(LCON-1+II))
C
                   CALL GCUCON(ZK8(LCON-1+II),' ',IEX)
                   IF ( IEX.EQ.0 .AND. ALARME.NE.'NON' ) THEN
C
C   SI LE CONCEPT A DETRUIRE N EXISTE PAS : ALARME
C   (SAUF SI ON DEMANDE A LES SUPPRIMER DANS UNE MACRO PAR EXEMPLE)
                     CALL UTMESS('A','DETRUIRE','LE CONCEPT '//
     &                           'DE NOM '' ' // ZK8(LCON-1+II) //
     &                           ' '' N''EXISTE PAS')
                   ENDIF
                 ELSE
                   IAUX = IOCC
                   CALL JEEXIN(NORECG,IER)
                   IF (IER.NE.0) CALL JEDETR(NORECG)
                   CALL PSRESE('CONCEPT',IAUX,1,ZK8(LCON-1+II),1,
     1                          NBPASS,NORECG,IRET)
                   CALL JEVEUO(NORECG,'L',ADRECG)
                   DO 310 NRPASS = 1,NBPASS
                      LERESU = ZK24(ADRECG+2*NRPASS-2) (1:8)
                      CALL JEDETC('G',LERESU,1)
                      CALL GCDETC(LERESU)
310                CONTINUE
                 ENDIF
300           CONTINUE
            ENDIF
 3       CONTINUE

         CALL GETFAC('OBJET',NBOCC)
         DO 2 IOCC = 1,NBOCC
            CALL GETVTX('OBJET','CLASSE',IOCC,1,1,KLAS,NOBJ)
            CALL GETVTX('OBJET','CHAINE',IOCC,1,0,KBID,NOBJ)
            NOBJ = -NOBJ
            CALL WKVECT('&&OPS007.NOMOBJ','V V K24',NOBJ,JOBJ)
            CALL GETVTX('OBJET','CHAINE',IOCC,1,NOBJ,ZK24(JOBJ),LBID)
            CALL GETVIS('OBJET','POSITION',IOCC,1,0,LBID,NIPO)
            NIPO = -NIPO
            IF (NIPO .LT. NOBJ) THEN
              CALL WKVECT('&&OPS007.NIPOSI','V V IS',NOBJ,JPO)
              DO 201 K=NIPO+1,NOBJ
                ZI(JPO+K-1) = 1
 201          CONTINUE 
            ELSE
              CALL WKVECT('&&OPS007.NIPOSI','V V IS',NIPO,JPO)
            ENDIF
            CALL GETVIS('OBJET','POSITION',IOCC,1,NIPO,ZI(JPO),LBID)
            DO 105 II =1,NOBJ
               KCH = ZK24(JOBJ+II-1)
               L=INDEX(KCH,' ')
               IF (L.GT.0) CALL JEDETC(KLAS,KCH(1:L-1),ZI(JPO+II-1))
 105        CONTINUE
            CALL JEDETR('&&OPS007.NOMOBJ')
            CALL JEDETR('&&OPS007.NIPOSI')
 2       CONTINUE
         IF (NIV.GT.1) LBID=JVINFO('AFFECT', 0)
         CALL JEDEMA()
      ENDIF
      END
