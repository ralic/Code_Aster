      SUBROUTINE CRPRN2(PFCHNO,BASE,NBNOEU,NEQUA,NEC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 20/01/99   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     CREATION ET ALLOCATION D'UNE STRUCTURE PROF_CHNO "PFCHNO"
C     ------------------------------------------------------------------
C IN  PFCHNO : CH19: NOM DU PROF_CHNO A CREER
C IN  BASE   : CH1 : NOM DE LA BASE SUR LAQUELLE LE PROF_CHNO DOIT ETRE
C                    CREE
C IN  NBNOEU : I   : NOMBRE DE NOEUDS DU MAILLAGE ASSOCIE AU PROF_CHNO
C IN  NEQUA  : I   : NOMBRE DE CMPS TOTAL DU PFCHNO (LONG(.VALE))
C IN  NEC    : I   : NOMBRE D'ENTIERS CODES POUR LA GRANDEUR
C     ------------------------------------------------------------------
C     PRECAUTIONS D'EMPLOI :
C       1) LE PROF_CHNO "PFCHNO" NE DOIT PAS EXISTER
C       2) LE .PRNO N'EST PAS AFFECTE
C       3) LE DEEQ N'EST PAS AFFECTE DANS CETTE ROUTINE (VOIR PTEEQU)
C     ------------------------------------------------------------------
C
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
C     ------------------------------------------------------------------
      INTEGER NBVAL,NBNOEU,NEQUA,NEC,LPRNO,LONPRN,IANUEQ,I
      CHARACTER*(*) PFCHNO,BASE
      CHARACTER*1 CLASSE
      CHARACTER*8 CBID
      CHARACTER*24 LILI,PRNO,NUEQ
      CHARACTER*32 JEXNOM,JEXNUM
C     ------------------------------------------------------------------
      DATA LILI/'                   .LILI'/
      DATA PRNO/'                   .PRNO'/
      DATA NUEQ/'                   .NUEQ'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CLASSE = BASE(1:1)
C
C     --------------------------- LILI --------------------------------
C     --- REPERTOIRE DES NOMS DE LIGREL ---
C     --- ATTENTION ICI UN SEUL LIGREL : &MAILLA ---
      NBVAL = 1
      LILI(1:19) = PFCHNO
      CALL JECREO(LILI,CLASSE//' N K24')
      CALL JEECRA(LILI,'NOMMAX',NBVAL,'  ')


C
C
C     --------------------------- PRNO --------------------------------
C     ------------- CREATION DE LA COLLECTION PROFIL NOEUD ------------
C
      PRNO(1:19) = PFCHNO
      LONPRN = (2+NEC)*NBNOEU
      CALL JECREC(PRNO,CLASSE//' V I','NU','CONTIG','CONSTANT',1)
      CALL JEECRA(PRNO,'LONMAX',LONPRN,CBID)
      CALL JECROC(JEXNOM(PRNO(1:19)//'.LILI','&MAILLA'))
      CALL JECROC(JEXNUM(PRNO,1))
      CALL JEVEUO(PRNO,'E',LPRNO)
C     --------------------------- PRNO --------------------------------
C     ------------- CREATION DE L'OBJET .NUEQ -------------------------
C     ----------ON LE REMPLIT PAR L'INDIRECTION "IDENTITE" !!
C
      NUEQ(1:19) = PFCHNO
      CALL WKVECT(NUEQ,CLASSE//' V I',NEQUA,IANUEQ)
      DO 1,I = 1,NEQUA
          ZI(IANUEQ-1+I) = I
    1 CONTINUE
C
      CALL JEDEMA()
      END
