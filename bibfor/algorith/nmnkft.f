      SUBROUTINE NMNKFT(SOLVEU,SDDISC,ITERAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/12/2011   AUTEUR BEAURAIN J.BEAURAIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER      ITERAT
      CHARACTER*19 SDDISC,MATASS
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE POUR METHODE DE NEWTON INEXACTE
C
C CALCUL DE LA PRECISION DE LA RESOLUTION DU SYSTEME LINEAIRE A CHAQUE
C ITERATION DE NEWTON POUR NEWTON-KRYLOV APPELEE FORCING TERM
C ----------------------------------------------------------------------
C
C IN  MATASS : SD MATRICE ASSEMBLEE
C IN  SDDISC : SD DISCRETISATION
C IN  ITERAT : NUMERO ITERATION NEWTON
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C

      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      JSLVR,JSLVK,IBID
      REAL*8       EPSI,EPSOLD,RESNEW,RESOLD,EPSMIN
      CHARACTER*8  METRES
      CHARACTER*19 SOLVEU
C
C ----------------------------------------------------------------------
C

      CALL JEMARQ()
C
C --- CALCUL DE LA PRECISION DE RESOLUTION POUR L'ITERATION SUIVANTE
C

C --- SHEMA DE CALCUL INPIRE DE "SOLVING NONLINEAR EQUATION WITH
C     NEWTON'S METHOD", C.T. KELLEY, SIAM, PAGE 62-63
        CALL JEVEUO(SOLVEU//'.SLVR','E',JSLVR)
        IF (ITERAT.EQ.-1 ) THEN
            CALL NMLERR(SDDISC,'L','INIT_NEWTON_KRYLOV',EPSI,IBID)
        ELSE
            IF (ITERAT.EQ.0 ) THEN
                CALL NMLERE(SDDISC,'L','VCHAR',ITERAT,RESOLD)
            ELSE
                CALL NMLERE(SDDISC,'L','VMAXI',ITERAT-1,RESOLD)
            ENDIF
            CALL NMLERR(SDDISC,'L','ITER_NEWTON_KRYLOV',EPSOLD,IBID)
            CALL NMLERE(SDDISC,'L','VMAXI',ITERAT,RESNEW)
            IF (RESOLD.EQ.0.D0) THEN
                EPSI=EPSOLD
                GO TO 10
            ENDIF
            IF ((0.9D0*EPSOLD**2).GT. 0.1D0) THEN
                EPSI=MIN(MAX(0.9D0*RESNEW**2/RESOLD**2,0.9D0*EPSOLD**2)
     &                                                   ,5.D-1*EPSOLD)
            ELSE
                EPSMIN = ZR(JSLVR)
                EPSI=MAX(MIN(0.9D0*RESNEW**2/RESOLD**2,5.D-1*EPSOLD)
     &                                                      ,EPSMIN)


            ENDIF
        ENDIF

 10     CONTINUE

C
C --- STOCKAGE DE LA PRECISION CALCULEE POUR ITERATION SUIVANTE
C
        CALL NMLERR(SDDISC,'E','ITER_NEWTON_KRYLOV',EPSI,IBID)
C
C --- COPIE DE LA PRECISION CALCULEE DANS LA SD SOLVEUR
C
        ZR(JSLVR+1)=EPSI


      CALL JEDEMA()
      END
