      SUBROUTINE RVPOST ( MCF, IOCC, DIM, I1, I2, NCHEFF,
     >                    XNOMCP,
     >                    NRESU, NCH19, NCH19N, SENSOP,
     >                    NLSMAC, NLSNAC, NOMTAB, XNOVAR)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     PILOTAGE DU POST-TRAITEMENT
C     ------------------------------------------------------------------
C IN  IOCC   : I : INDICE DE L' OCCURENCE
C IN  DIM    : K : '2D' OU '3D'
C IN  I1, I2 : I : REPERAGE DU CHAMP DANS UNE SD RESULTAT_COMPOSE
C IN  XNOMCP : K : NOM DE LA COLLECTION DES NOMS DE CMP
C IN  NCH19  : K : NOM DU CHAMP A TRAITER
C IN  NCH19N : K : NOM DU CHAMP NOMINAL QUAND ON TRAITE UNE DERIVEE
C IN  SENSOP : K : OPTION POUR LA SENSIBILITE
C IN  NLSMAC : K : NOM DU VECTEUR DES MAILLES ACTIVES
C IN  NLSNAC : K : NOM DU VECTEUR DES NOEUDS ACTIFS
C     ------------------------------------------------------------------
      IMPLICIT   NONE
C
      INCLUDE 'jeveux.h'
      INTEGER IOCC, I1, I2
      CHARACTER*2 DIM
      CHARACTER*8 NRESU
      CHARACTER*16 NCHEFF
      CHARACTER*18 SENSOP
      CHARACTER*19 NCH19, NCH19N, NOMTAB
      CHARACTER*24 XNOMCP, NLSMAC, NLSNAC, XNOVAR
      CHARACTER*(*) MCF
C
      INTEGER      GD, I, IDIR, IE, NIV, IRET, ISD, JCMPCD, JCMPNC,
     >             JDIR, JLSMAC, JLSNAC, JNOMCP, JSDEV, JSDLI, JVEC1,
     >             JVEC2, N, N0, N1, N2, N3, NBCAC, NBCPN, NBCRB,
     >             NBMAC, NBNAC, NBOPER, NBSD, NR, IFM ,IBID
      INTEGER NY
      REAL*8 VECTY(3)
      LOGICAL TRIDIM
      CHARACTER*24 LSCPNC, QUANT, SDLIEU, SDEVAL, LSCPCD
      CHARACTER*24 SDEV, SDLI, SDMOYE, SDMAIL
      CHARACTER*19 SDPOST, EVAL, LIEU, SDNEWR, SSCH19
      CHARACTER*16 OPTION, OPER, OPERAT(2)
      CHARACTER*8  K8B, TYPCO, COURBE, MAILLA, REPERE
      CHARACTER*4  DOCU
      CHARACTER*1  CA
      LOGICAL      CHOK
      INTEGER      IARG
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
      CHOK = .TRUE.
      CALL INFNIV ( IFM, NIV )
C
      CALL GETVTX(MCF,'OPERATION',IOCC,IARG,0,K8B,NBOPER)
      NBOPER = -NBOPER
      CALL GETVTX(MCF,'OPERATION',IOCC,IARG,NBOPER,OPERAT,N0)
C
      IF ( NCH19(1:1) .EQ. '&' ) THEN
         IF (NIV.GT.1) CALL RVINFO(IFM,IOCC,I1,I2,'E',NCHEFF)
      ELSE
         LSCPCD = '&&RVPOST.NOM.CMP.CAND.OC'
         LSCPNC = '&&RVPOST.NOM.CMP.NCSR.OC'
         CALL JEEXIN(NCH19//'.DESC',IBID)
         IF (IBID.GT.0) THEN
           CALL JELIRA(NCH19//'.DESC','DOCU',N,DOCU)
           CALL JEVEUO(NCH19//'.DESC','L',N1)
         ELSE
           CALL JELIRA(NCH19//'.CELD','DOCU',N,DOCU)
           CALL JEVEUO(NCH19//'.CELD','L',N1)
         END IF
         CALL JEVEUO(JEXNUM(XNOMCP,IOCC),'L',JNOMCP)
         CALL JELIRA(JEXNUM(XNOMCP,IOCC),'LONMAX',NBCAC,K8B)
         CALL WKVECT(LSCPCD,'V V K8',NBCAC,JCMPCD)
         CALL WKVECT('&&RVPOST.VAL.DIR','V V R',3,JDIR)
         DO 10, I = 1, NBCAC, 1
            ZK8(JCMPCD + I-1) = ZK8(JNOMCP + I-1)
10       CONTINUE
         GD = ZI(N1 + 1-1)
         IF (NIV.GT.1) CALL RVINFO(IFM,IOCC,I1,I2,'B',NCHEFF)
         IF ( NRESU(1:1).EQ.' ' ) NRESU = NCH19(1:8)
         CALL RVCPNC(MCF,IOCC,NCH19,GD,DOCU,NBCAC,LSCPCD,LSCPNC,
     +               REPERE,OPTION,QUANT,IDIR,ZR(JDIR),IRET)
C        /* POSSIBILITE AGRANDISSEMENT DE LSCPCD => ON REFAIT JEVEUO */
         CALL JEVEUO(LSCPCD,'L',JCMPCD)
C
         IF ( IRET .NE. 0 ) THEN
            SSCH19 = '&&RVPOST.SOUS.CH.GD'
            CALL JELIRA(LSCPNC,'LONMAX',NBCPN,K8B)
            CALL JEVEUO(LSCPNC,'L',JCMPNC)
C
            IF ( DOCU .EQ. 'CHNO' ) THEN
               CALL JELIRA(NLSNAC,'LONMAX',NBNAC,K8B)
               CALL JEVEUO(NLSNAC,'L',JLSNAC)
               CALL EXTCHN(NCH19,NCH19N, SENSOP,
     >                     K8B,ZI(JLSNAC),ZK8(JCMPNC),
     >                     NBNAC,NBCPN,'NUMERO',SSCH19, MCF, IOCC )
C
            ELSE
               CALL JEEXIN(NLSNAC,IBID)
               IF (IBID.GT.0) THEN
                  CALL JELIRA(NLSNAC,'LONMAX',NBNAC,K8B)
                  CALL JEVEUO(NLSNAC,'L',JLSNAC)
               ELSE
                  JLSNAC = 1
                  NBNAC = 0
               ENDIF
               CALL JELIRA(NLSMAC,'LONMAX',NBMAC,K8B)
               CALL JEVEUO(NLSMAC,'L',JLSMAC)
               CALL EXTCHE(NCH19,NCH19N, SENSOP,
     >                     K8B,ZI(JLSMAC),ZK8(JCMPNC),
     >                     NBMAC,NBCPN,'NUMERO',SSCH19, MCF, IOCC,
     >                     NBNAC, ZI(JLSNAC) )
            ENDIF
C
           CALL GETVR8('ACTION','VECT_Y',IOCC,IARG,3,VECTY,NY)
           TRIDIM=NY.NE.0

            IF ( CHOK ) THEN
               CALL DISMOI('F','NOM_MAILLA',NCH19,'CHAMP',I,MAILLA,IE)
               CALL GETVID(MCF,'CHEMIN',IOCC,IARG,0,K8B,NBCRB)
               NBCRB = -NBCRB
               IF ( NBCRB .NE. 0 ) THEN
                  CALL GETVID(MCF,'CHEMIN',IOCC,IARG,NBCRB,COURBE,N1)
                  TYPCO = 'CHEMIN'
               ELSE
                  TYPCO = 'AUTRE'
                  COURBE = '&&YAPAS'
               ENDIF
               SDLIEU = '&&RVPOST.NOM.VECT.LIEU'
               SDEVAL = '&&RVPOST.NOM.VECT.EVAL'
C
               CALL GETVTX(MCF,'MOYE_NOEUD',IOCC,IARG,1,K8B,N)
               IF ( K8B(1:1) .EQ. 'O' ) THEN
                  CA = 'N'
               ELSE
                  CA = 'E'
               ENDIF
C
               CALL RVLIEU(MAILLA,TYPCO,COURBE,NLSNAC,SDLIEU)
               CALL RVPSTE(DIM,SDLIEU,SSCH19,SDEVAL,CA)
               CALL JELIRA(SDLIEU,'LONMAX',NBSD,K8B)
               CALL JEVEUO(SDLIEU,'L',JSDLI)
               CALL JEVEUO(SDEVAL,'L',JSDEV)
               CALL GETVTX(MCF,'RESULTANTE',IOCC,IARG,0,ZK80,NR   )
                SDNEWR = '&&RVPOST.NEW.REPERE'
                IF ( REPERE(1:1) .NE. 'G' .AND. .NOT.TRIDIM) THEN
                   CALL RVCHGR(MAILLA,COURBE,NLSNAC,REPERE,SDNEWR,IRET)
                ELSE
                   IRET  = 1
                ENDIF
C
                IF ( IRET .NE. 0 ) THEN
                   SDPOST = '&&RVPOST.FINAL.POST'
                   DO 100, ISD = 1, NBSD, 1
                      IF ( REPERE(1:1) .NE. 'G'.AND. .NOT.TRIDIM ) THEN
                         CALL JEVEUO(JEXNUM(SDNEWR//'.VEC1',ISD),'L',
     +                               JVEC1)
                         CALL JEVEUO(JEXNUM(SDNEWR//'.VEC2',ISD),'L',
     +                               JVEC2)
                      ELSE
                         JVEC1 = 0
                         JVEC2 = 0
                      ENDIF
C
                      SDEV = ZK24(JSDEV + ISD-1)
                      SDLI = ZK24(JSDLI + ISD-1)
C
                      CALL RVCALQ(IOCC,SDEV,ZR(JVEC1),ZR(JVEC2),
     +                            REPERE,ZK8(JCMPCD),NBCPN,NBCAC,
     +                            OPTION,QUANT,SDLI,
     +                            IDIR,ZR(JDIR),SDPOST,COURBE)
C
                      IF ( NR .EQ. 0 ) THEN
                         IF ( NBOPER .EQ. 2 ) THEN
                            SDMAIL = SDEV(1:19)//'.MAIL'
                            SDMOYE = '&&RVPOST.MOYENNE'
                            CALL RVPSTM (SDLI,SDPOST,SDMOYE)
                            CALL RVAFFE(MCF,IOCC,SDLI,SDPOST,SDMAIL,CA,
     +                                  QUANT,OPTION,REPERE,
     +                                  NOMTAB,XNOVAR,NCHEFF,I1,ISD)
                            OPER = 'MOYENNE'
                            CALL RVAFFM(MCF,IOCC,SDLI,SDPOST,SDMOYE,
     +                                  OPER,QUANT,OPTION,REPERE,
     +                                  NOMTAB,XNOVAR,NCHEFF,I1,ISD)
                            CALL JEDETR(SDMOYE)
C
                         ELSE
                           OPER = OPERAT(1)
                           IF ( OPER .EQ.'EXTRACTION' ) THEN
                             SDMAIL = SDEV(1:19)//'.MAIL'
                             CALL RVAFFE(MCF,IOCC,SDLI,SDPOST,SDMAIL,CA,
     +                                   QUANT,OPTION,REPERE,
     +                                   NOMTAB,XNOVAR,NCHEFF,I1,ISD)
C
                            ELSE
                              SDMOYE = '&&RVPOST.MOYENNE'
                              CALL RVPSTM(SDLI,SDPOST,SDMOYE)
                              CALL RVAFFM(MCF,IOCC,SDLI,SDPOST,SDMOYE,
     +                                    OPER,QUANT,OPTION,REPERE,
     +                                    NOMTAB,XNOVAR,NCHEFF,I1,ISD)
                              CALL JEDETR(SDMOYE)
                            ENDIF
                         ENDIF
                      ELSE
                         SDMOYE = '&&RVPOST.SOMME'
                         CALL RVPSTS(IOCC,SDLI,SDPOST,SDMOYE)
                         CALL RVAFFS(MCF,IOCC,SDLI,SDPOST,SDMOYE,QUANT,
     +                               OPTION,REPERE,NOMTAB,NCHEFF,I1,ISD)
                         SDMOYE(20:24) = '.VALE'
                         CALL JEDETR(SDMOYE)
                         SDMOYE(20:24) = '.NOCP'
                         CALL JEDETR(SDMOYE)
                      ENDIF
C
                      CALL JEDETR(SDPOST//'.VALE')
                      CALL JEDETR(SDPOST//'.PADR')
                      CALL JEDETR(SDPOST//'.PNBN')
                      CALL JEDETR(SDPOST//'.NOCP')
                      CALL JEDETR(SDPOST//'.PNCO')
                      CALL JEDETR(SDPOST//'.PNSP')
100                CONTINUE
                   CALL JEDETC('V','&&RVIMPM',1)
                ENDIF
                CALL JEEXIN(SDNEWR//'.VEC1',N1)
                IF ( N1 .NE. 0 ) THEN
                   CALL JEDETR(SDNEWR//'.VEC1')
                   CALL JEDETR(SDNEWR//'.VEC2')
                ENDIF
                CALL JELIRA(SDLIEU,'LONMAX',N,K8B)
                CALL JEVEUO(SDLIEU,'L',N1)
                CALL JEVEUO(SDEVAL,'L',N2)
                DO 20, I = 1, N, 1
                   LIEU = ZK24(N1 + I-1)(1:19)
                   EVAL = ZK24(N2 + I-1)(1:19)
                   CALL JEDETR(LIEU//'.ABSC')
                   CALL JEDETR(LIEU//'.REFE')
                   CALL JEDETR(LIEU//'.DESC')
                   CALL JEDETR(LIEU//'.NUME')
                   CALL JEDETR(LIEU//'.COOR')
                   CALL TUESCH(EVAL)
                   CALL JEEXIN(EVAL//'.MAIL',N3)
                   IF ( N3 .NE. 0 ) THEN
                      CALL JEDETR(EVAL//'.MAIL')
                   ENDIF
20              CONTINUE
                CALL JEDETR(SDLIEU)
                CALL JEDETR(SDEVAL)
            ENDIF
            CALL JEDETR(LSCPNC)
            CALL TUESCH(SSCH19)
         ENDIF
         CALL JEDETR(LSCPCD)
         CALL JEDETR('&&RVPOST.VAL.DIR')
      ENDIF
C
      CALL JEDEMA()
      END
