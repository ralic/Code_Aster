      SUBROUTINE TRCART ( IFIC, NOCC )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER    IFIC, NOCC
C RESPONSABLE PELLET J.PELLET
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     COMMANDE:  TEST_RESU
C                MOT CLE FACTEUR "CARTE"
C ----------------------------------------------------------------------


      INTEGER       IOCC, IBID, IRET
      INTEGER      N1, N2, N3,IVARI
      INTEGER  NL1, NL2, LXLGUT,VALI
      REAL*8        EPSI, VALR
      COMPLEX*16    VALC
      CHARACTER*1  TYPRES
      CHARACTER*8  CRIT,NODDL,NOMMA,NOMAIL,NOMGD
      CHARACTER*11 MOTCLE
      CHARACTER*19 CHAM19
      CHARACTER*16 TBTXT(2)
      CHARACTER*200 LIGN1,LIGN2
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ()

      MOTCLE = 'CARTE'

      DO 100 IOCC = 1,NOCC
        LIGN1  = ' '
        LIGN2  = ' '
        NODDL = ' '
        CALL GETVID('CARTE','CHAM_GD',IOCC,IARG,1,CHAM19,N1)
        LIGN1(1:21)='---- '//MOTCLE(1:9)
        LIGN1(22:22)='.'
        LIGN2(1:21)='     '//CHAM19(1:8)
        LIGN2(22:22)='.'
        CALL UTEST3('CARTE',IOCC,TBTXT)

        CALL GETVTX ( 'CARTE', 'NOM_CMP',  IOCC,IARG,1, NODDL, N1 )
        CALL ASSERT(N1.EQ.1)
        NL1 = LXLGUT(LIGN1)
        NL2 = LXLGUT(LIGN2)
        LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOM_CMP'
        LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NODDL
        LIGN1(NL1+17:NL1+17)='.'
        LIGN2(NL2+17:NL2+17)='.'


        CALL GETVR8 ( 'CARTE', 'PRECISION',IOCC,IARG,1, EPSI,  N1 )
        CALL GETVTX ( 'CARTE', 'CRITERE',  IOCC,IARG,1, CRIT,  N1 )

        CALL GETVR8('CARTE','VALE'    , IOCC,IARG,1,VALR,N1)
        CALL GETVIS('CARTE','VALE_I'  , IOCC,IARG,1,VALI,N2)
        CALL GETVC8('CARTE','VALE_C'  , IOCC,IARG,1,VALC,N3)

        IF( N1 .EQ. 1) THEN
          TYPRES = 'R'
        ELSE IF( N2 .EQ. 1) THEN
          TYPRES = 'I'
        ELSE
          CALL ASSERT(N3.EQ.1)
          TYPRES = 'C'
        ENDIF


        CALL GETVTX('CARTE','NOM_CMP',IOCC,IARG,1,NODDL,N1)
        CALL DISMOI('F','NOM_MAILLA',CHAM19,'CHAMP',IBID,NOMMA,IRET)
        CALL GETVEM(NOMMA,'MAILLE','CARTE','MAILLE',IOCC,IARG,1,
     &              NOMAIL,N1)
        CALL ASSERT(N1.EQ.1)
        NL1 = LXLGUT(LIGN1)
        NL2 = LXLGUT(LIGN2)
        LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' MAILLE'
        LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NOMAIL
        LIGN1(NL1+17:NL1+17)='.'
        LIGN2(NL2+17:NL2+17)='.'


        CALL DISMOI('F','NOM_GD',CHAM19,'CHAMP',IBID,NOMGD,IRET)
        CALL UTCMP1(NOMGD,'CARTE',IOCC,NODDL,IVARI)

        NL1 = LXLGUT(LIGN1)
        NL1 = LXLGUT(LIGN1(1:NL1-1))
        NL2 = LXLGUT(LIGN2)
        NL2 = LXLGUT(LIGN2(1:NL2-1))
        WRITE (IFIC,*) LIGN1(1:NL1)
        WRITE (IFIC,*) LIGN2(1:NL2)

        CALL UTEST5(CHAM19,NOMAIL,NODDL,
     &              TBTXT,VALI,VALR,VALC,TYPRES,EPSI,CRIT,IFIC)
        WRITE (IFIC,*)' '

 100  CONTINUE


      CALL JEDEMA()
      END
